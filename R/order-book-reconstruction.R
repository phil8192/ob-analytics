# instantaneous limit order book reconstruction.
# reconstructs an order book for a specific point in time.

active.order.state <- function(quotes, tp=as.POSIXlt(Sys.time(), tz="UTC"), 
    min.bid=0, max.ask=Inf) {
  # current active orders (all active orders + their entire lifecycle)
  created.before <- quotes[quotes$action == "created" & 
      quotes$timestamp <= tp, "id"]
  deleted.before <- quotes[quotes$action == "deleted" & 
      quotes$timestamp <= tp, "id"]
  active.order.ids <- setdiff(created.before, deleted.before)
  active.orders <- quotes[quotes$id %in% active.order.ids, ]
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
}

bids <- function(active.orders) {
  bids <- active.orders[active.orders$direction=="bid" & 
      active.orders$type != "market", ]
  # order by price, then each price level is fifo, ordered by order id (could 
  # also be timestamp)
  bids <- bids[order(-bids$price, bids$id), ]
  first.price <- head(bids, 1)$price
  cbind(bids, pct=(first.price-bids$price)/first.price, 
      liquidity=cumsum(bids$volume))
}

asks <- function(active.orders) {
  asks <- active.orders[active.orders$direction=="ask" & 
      active.orders$type != "market", ]
  asks <- asks[order(asks$price, asks$id), ]
  first.price <- head(asks, 1)$price
  cbind(asks, pct=(asks$price-first.price)/first.price, 
      liquidity=cumsum(asks$volume))
}

# get a nice order book
current.order.book <- function(active.orders, max.price.levels=NULL, 
    pct.range=0, ts="") {
  asks <- asks(active.orders)[, c("id", "timestamp", "exchange.timestamp", 
      "price", "volume", "liquidity", "pct")]
  asks <- asks[rev(1:nrow(asks)), ] # reverse the asks (ascending price)
  bids <- bids(active.orders)[, c("id", "timestamp", "exchange.timestamp", 
      "price", "volume", "liquidity", "pct")]
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

  list(timestamp=ts, asks=asks, bids=bids)
}

order.book <- function(events, ts, max.price.levels=NULL, pct.range=0) {
  cur <- active.order.state(events, ts)
  current.order.book(cur, max.price.levels, pct.range, ts)
}

