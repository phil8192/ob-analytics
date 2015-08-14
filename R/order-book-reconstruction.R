##' Instantaneous limit order book reconstruction.
##'
##' Reconstructs a limit order book for a specific point in time.
##' 
##' @param events Limit order events data.frame.
##' @param tp Time point to re-construct order book at.
##' @param max.price.levels Max number of price levels to return.
##' @param bps.range Max depth to return +- BPS from best bid/ask. 
##' @param min.bid Min bid to return.
##' @param max.ask Max ask to return.
##' @return Limit Order Book structure:
##'
##' \preformatted{
##' > tp <- as.POSIXct("2015-05-01 12:30:15.342", tz="UTC")
##' > orderBook(lob.data$events, tp, max.price.levels=5)
##' $timestamp
##' [1] "2015-05-01 12:45:15.342 UTC"
##' 
##' $asks
##'         id               timestamp  exchange.timestamp  price    volume liquidity      bps
##' 1 65661008 2015-05-01 12:45:06.117 2015-05-01 12:45:05 235.01 377676509 693496995 12.35515
##' 2 65642214 2015-05-01 09:31:48.053 2015-05-01 09:04:46 235.00  94518509 315820486 11.92911
##' 3 65645212 2015-05-01 09:37:13.487 2015-05-01 09:37:13 234.94 100000000 221301977  9.37287
##' 4 65661007 2015-05-01 12:45:05.469 2015-05-01 12:45:05 234.93 100000000 121301977  8.94683
##' 5 65661003 2015-05-01 12:45:01.993 2015-05-01 12:45:01 234.72  21301977  21301977  0.00000
##'
##' $bids
##'         id               timestamp  exchange.timestamp  price     volume  liquidity       bps
##' 1 65661023 2015-05-01 12:45:13.551 2015-05-01 12:45:13 234.66   88776681   88776681 0.0000000 
##' 2 65661019 2015-05-01 12:45:11.976 2015-05-01 12:45:11 234.65 1000887713 1089664394 0.4261485
##' 3 65661012 2015-05-01 12:45:08.180 2015-05-01 12:45:07 234.64  115144874 1204809268 0.8522969
##' 4 65660900 2015-05-01 12:44:02.312 2015-05-01 12:43:34 234.61  388686219 1593495487 2.1307424
##' 5 65661002 2015-05-01 12:45:01.720 2015-05-01 12:45:01 234.45   21326509 1614821996 8.9491179
##'
##'     where        id = Limit order id.
##'           timestamp = Last update time (last time order was partially hit)
##'  exchange.timestamp = Order creation time.
##'               price = Limit price.
##'              volume = Remaining order volume.
##'           liquidity = Cumulative sum of volume up util this order (depth).
##'                 bps = Distance in BPS from current best bid/ask.
##' }
##' @author phil
##' @export orderBook
##' @examples
##' 
##' tp <- as.POSIXct("2015-05-01 12:30:15.342", tz="UTC")
##' orderBook(lob.data$events, max.price.levels=5)
##'
orderBook <- function(events, tp=as.POSIXlt(Sys.time(), tz="UTC"),
    max.price.levels=NULL, bps.range=0, min.bid=0, max.ask=Inf) {

  pct.range <- bps.range*0.0001

  activeBids <- function(active.orders) {
    bids <- active.orders[active.orders$direction=="bid" & 
        active.orders$type != "market", ]
    # order by price, then each price level is fifo, ordered by order id (could 
    # also be timestamp)
    bids <- bids[order(-bids$price, bids$id), ]
    first.price <- head(bids, 1)$price
    cbind(bids, bps=((first.price-bids$price)/first.price)*10000, 
        liquidity=cumsum(bids$volume))
  }

  activeAsks <- function(active.orders) {
    asks <- active.orders[active.orders$direction=="ask" & 
        active.orders$type != "market", ]
    asks <- asks[order(asks$price, asks$id), ]
    first.price <- head(asks, 1)$price
    cbind(asks, bps=((asks$price-first.price)/first.price)*10000, 
        liquidity=cumsum(asks$volume))
  }

  active.orders <- (function() {
    # current active orders (all active orders + their entire lifecycle)
    created.before <- events[events$action == "created" & 
        events$timestamp <= tp, "id"]
    deleted.before <- events[events$action == "deleted" & 
        events$timestamp <= tp, "id"]
    active.order.ids <- setdiff(created.before, deleted.before)
    active.orders <- events[events$id %in% active.order.ids, ]
    # remove order updates (deleted, changed) after now
    active.orders <- active.orders[active.orders$timestamp <= tp, ]
    # for partial fills (order changed) <= now, keep the most recent change 
    #(least remaining volume),
    changed.orders <- active.orders$action == "changed"
    changed.before <- active.orders[changed.orders, ]
    changed.before <- changed.before[order(changed.before[, "volume"]), ]
    changed.before <- changed.before[!duplicated(changed.before$id), ]
    # remove, put back:
    # 1) remove all changed orders
    # 2) remove any created orders associated with the changed orders
    active.orders <- active.orders[!changed.orders, ]
    active.orders <- active.orders[!(active.orders$id %in% changed.before$id), ]
    active.orders <- rbind(active.orders, changed.before)

    # sanity check
    stopifnot(all(active.orders$timestamp <= tp))
    stopifnot(all(!duplicated(active.orders$id)))

    active.orders

  })()
  
  asks <- activeAsks(active.orders)[, c("id", "timestamp",
      "exchange.timestamp", "price", "volume", "liquidity", "bps")]
  # reverse the asks (ascending price)
  asks <- asks[rev(1:nrow(asks)), ]
  bids <- activeBids(active.orders)[, c("id", "timestamp",
      "exchange.timestamp", "price", "volume", "liquidity", "bps")]

  if(pct.range > 0) {
    max.ask <- tail(asks, 1)$price*(1+pct.range)
    asks <- asks[asks$price <= max.ask, ]
    min.bid <- head(bids, 1)$price*(1-pct.range)
    bids <- bids[bids$price >= min.bid, ]
  } 

  if(!is.null(max.price.levels)) {
    asks <- tail(asks, max.price.levels)
    bids <- head(bids, max.price.levels)
  }

  rownames(asks) <- NULL
  rownames(bids) <- NULL

  list(timestamp=tp, asks=asks, bids=bids)

}
