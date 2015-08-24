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
##' > tp <- as.POSIXct("2015-05-01 03:30:15.342", tz="UTC")
##' > orderBook(lob.data$events, tp, max.price.levels=5)
##' $timestamp
##' [1] "2015-05-01 03:30:15.342 UTC"
##'
##' $asks
##'         id               timestamp  exchange.timestamp  price     volume  liquidity       bps
##' 1 65613703 2015-05-01 03:29:53.127 2015-05-01 03:29:52 236.58  910229141 6341547077 2.1138968
##' 2 65613655 2015-05-01 03:29:24.703 2015-05-01 03:29:24 236.56 1320000000 5431317936 1.2683381
##' 3 65613700 2015-05-01 03:29:52.040 2015-05-01 03:29:51 236.55 1320000000 4111317936 0.8455587
##' 4 65613698 2015-05-01 03:29:51.510 2015-05-01 03:29:51 236.54 1600000000 2791317936 0.4227794
##' 5 65613712 2015-05-01 03:29:56.755 2015-05-01 03:29:56 236.53 1191317936 1191317936 0.0000000
##' 
##' $bids
##'         id               timestamp  exchange.timestamp  price    volume liquidity      bps
##' 1 65613225 2015-05-01 03:27:44.450 2015-05-01 03:24:00 236.36  16154172  16154172 0.000000
##' 2 65613681 2015-05-01 03:29:42.259 2015-05-01 03:29:41 236.31 200000000 216154172 2.115417
##' 3 65613220 2015-05-01 03:23:57.963 2015-05-01 03:23:57 236.30 100000000 316154172 2.538501
##' 4 65612978 2015-05-01 03:21:10.454 2015-05-01 03:21:10 236.28 100000000 416154172 3.384667
##' 5 65612388 2015-05-01 03:13:08.808 2015-05-01 03:13:08 236.17 100000000 516154172 8.038585
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
##' tp <- as.POSIXct("2015-05-01 04:25:15.342", tz="UTC")
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
