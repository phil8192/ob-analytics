## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

##' Instantaneous limit order book reconstruction.
##'
##' Given a set of \code{\link{events}}, reconstructs a limit order book for a
##' specific point in time. 
##'
##' An order book consists of 2 sides: \emph{bids} and \emph{asks}, an example of
##' which is shown below:
##' 
##' \tabular{rrrrrrr}{
##'       id \tab  price \tab     volume \tab  liquidity \tab  bps \cr
##' 65613703 \tab 236.58 \tab  910229141 \tab 6341547077 \tab 2.11 \cr
##' 65613655 \tab 236.56 \tab 1320000000 \tab 5431317936 \tab 1.26 \cr
##' 65613700 \tab 236.55 \tab 1320000000 \tab 4111317936 \tab 0.84 \cr
##' 65613698 \tab 236.54 \tab 1600000000 \tab 2791317936 \tab 0.42 \cr
##' 65613712 \tab 236.53 \tab 1191317936 \tab 1191317936 \tab 0.00 \cr
##'        - \tab      - \tab          - \tab          - \tab    - \cr
##' 65613225 \tab 236.36 \tab   16154172 \tab   16154172 \tab 0.00 \cr
##' 65613681 \tab 236.31 \tab  200000000 \tab  216154172 \tab 2.11 \cr
##' 65613220 \tab 236.30 \tab  100000000 \tab  316154172 \tab 2.53 \cr
##' 65612978 \tab 236.28 \tab  100000000 \tab  416154172 \tab 3.38 \cr
##' 65612388 \tab 236.17 \tab  100000000 \tab  516154172 \tab 8.03}
##' 
##' @param events Limit order \code{\link{events}} data.frame.
##' @param tp Time point to re-construct order book at.
##' @param max.levels Max number of price levels to return.
##' @param bps.range Max depth to return +- BPS from best bid/ask. 
##' @param min.bid Min bid to return.
##' @param max.ask Max ask to return.
##' @return Limit Order Book structure. A list containing 3 fields:
##'
##' \describe{
##'   \item{timestamp}{Timestamp the order book was reconstructed for.}
##'   \item{asks}{A data.frame containing the Ask side of the order book.}
##'   \item{bids}{A data.frame containing the Bid side of the order book.}
##' }
##'
##' The \emph{bids} and \emph{asks} data consists of the following:
##'
##' \describe{
##'   \item{id}{Limit order Id.}
##'   \item{timestamp}{Last modification time to limit order.}
##'   \item{exchange.timestamp}{Time at which order was placed in order book.}
##'   \item{price}{Limit order price.}
##'   \item{volume}{Limit orer volume.}
##'   \item{liquidity}{Cumulative sum of volume from best bid/ask up until price.}
##'   \item{bps}{Distance (in BPS) of order from best bid/ask.}
##' }
##'
##' Both the \emph{bids} and \emph{asks} data are ordered by descending price.
##' 
##' @author phil
##' @export orderBook
##' @examples
##' 
##' tp <- as.POSIXct("2015-05-01 04:25:15.342", tz="UTC")
##' orderBook(lob.data$events, max.levels=5)
##'
orderBook <- function(events, tp=as.POSIXlt(Sys.time(), tz="UTC"),
    max.levels=NULL, bps.range=0, min.bid=0, max.ask=Inf) {

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
    # for order changed(partial fills or order revisions) <= now, keep the most recent change 
    # (latest timestamp),
    changed.orders <- active.orders$action == "changed"
    changed.before <- active.orders[changed.orders, ]
    changed.before <- changed.before[order(changed.before[, "timestamp"], decreasing = TRUE), ]
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

  if(!is.null(max.levels)) {
    asks <- tail(asks, max.levels)
    bids <- head(bids, max.levels)
  }

  rownames(asks) <- NULL
  rownames(bids) <- NULL

  list(timestamp=tp, asks=asks, bids=bids)

}
