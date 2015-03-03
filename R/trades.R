
# infer time and sales from event data.
# (event contains maker/taker)

# determine t&s with maker/taker match.
# timestamp = min(maker timestamp, taker timestamp) (first time we learned of this trade)
# price = maker price (taker limit price can cross the book.)
# volume = traded volume
# direction = buy or sell (side of the aggressor/taker
# maker event id
# taker event id
time.and.sales <- function(events) {

  print(paste("inferring time and sales from", nrow(events), "events..."))

  # trades with matching maker/taker.
  # align them by event id.
  matching.bids <- events[events$direction == "bid" & !is.na(events$matching.event), ]
  matching.bids <- matching.bids[order(matching.bids$event.id), ]
  matching.asks <- events[events$direction == "ask" & !is.na(events$matching.event), ]
  matching.asks <- matching.asks[order(matching.asks$matching.event), ]
  stopifnot(all(matching.bids$event.id - matching.asks$matching.event == 0))

  # makers/takers.
  # maker = exchange.timestamp < counterpart exchangetime.stamp
  stopifnot(all(matching.bids$exchange.timestamp != matching.asks$exchange.timestamp)) # based on this assertion.
  bid.maker <- matching.bids$exchange.timestamp < matching.asks$exchange.timestamp # bid side is the maker

  # t&s timestamp is the first observation in the 2 matching trades.   
  timestamp <- as.POSIXct(ifelse(matching.bids$timestamp < matching.asks$timestamp, matching.bids$timestamp, matching.asks$timestamp), origin="1970-01-01", tz="UTC")

  # the price at which the trade occurred is the maker price.
  price <- ifelse(bid.maker, matching.bids$price, matching.asks$price)

  # volume is either side of trade
  volume <- matching.bids$fill

  # if bid is maker, trade was initiated by a seller.
  direction <- factor(ifelse(bid.maker, "sell", "buy"))

  # finally, maker+taker id.
  maker.event.id <- ifelse(bid.maker, matching.bids$event.id, matching.asks$event.id)
  taker.event.id <- ifelse(bid.maker, matching.asks$event.id, matching.bids$event.id)

  # return timestamp ordered series.
  combined <- data.frame(timestamp, price, volume, direction, maker.event.id, taker.event.id)
  combined[order(timestamp), ]

}

# group trades by taker id.
group.trades <- function(trades, events, fields="id") cbind(trades, taker=events[match(trades$taker.event.id, events$event.id), fields])

# order id, start.price, end.price, vwap.price, lifted.orders, lifted.volume, type, start.time, end.time
trade.impacts <- function(trades, events, direction) {
  grouped <- group.trades(trades[trades$direction == direction, ], events, c("id", "type"))
  grouped <- grouped[order(grouped$taker.id, grouped$timestamp), ]

  taker.ids <- grouped$taker.id

  by.group <- by(grouped, taker.ids, function(g) {
    list(timestamp=max(g$timestamp), from=min(g$timestamp), max=max(g$price), min=min(g$price), vwap=vwap(g$price, g$volume), hits=nrow(g), vol=sum(g$volume))
  })

  df <- data.frame(t(data.frame(lapply(by.group, unlist)))) # convert grouped (class of by) to data frame.
  df$timestamp <- as.POSIXct(df$timestamp, tz="UTC", origin="1970-01-01")
  df$from <- as.POSIXct(df$from, tz="UTC", origin="1970-01-01")

  df <- cbind(id=unique(taker.ids[!is.na(taker.ids)]), df, vol=df$vol, type=factor(tapply(grouped$taker.type, taker.ids, function(g) head(as.character(g), 1))))
  rownames(df) <- NULL

  if(direction == "buy")
    df <- df[, c(1:3, 5, 4, 6:10)]

  colnames(df)[4:5] <- c("from.price", "to.price") 

  df
}


