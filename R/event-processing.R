## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

##' Read raw limit order event data from a CSV file.
##'
##' The function performs some data sanitisation: removing duplicate events
##' and ensuring that order events are in the appropriate order. The data are
##' ordered according to the life-cycle of a limit order:
##'     order id, volume (desc), action (created,changed,deleted)
##' 
##' In addition to returning the sanitised raw data, the function also
##' appends a column "fill.deltas" representing the change in volume for each
##' limit order event.
##' 
##' @param file Location of CSV file containing limit order events. 
##' @return A data.frame containing the raw limit order events data.
##' @author phil
##' @keywords internal
loadEventData <- function(file) {

  # data may contain duplicate events.
  removeDuplicates <- function(events) {
    dups <- which(duplicated(events[, c("id", "price", "volume", "action")]))
    if(length(dups) > 0) {
      ids <- paste(unique(events[dups, ]$id), collapse=" ")
      events <- events[-dups, ]
      warning(paste("removed", length(dups), "duplicate events:", ids))
    }
    events
  }

  events <- read.csv(file, header=T, sep=",")
  events <- removeDuplicates(events)
  events$timestamp <- as.POSIXct(events$timestamp/1000, origin="1970-01-01", 
      tz="UTC")
  events$exchange.timestamp <- as.POSIXct(events$exchange.timestamp/1000, 
      origin="1970-01-01", tz="UTC")
  # an order can be in 1 of these 3 ordered states.
  events$action <- factor(events$action, c("created", "changed", "deleted"))
  events$direction <- factor(events$direction, c("bid", "ask"))

  # order event data by id, then by volume (decreasing), then finally, by order 
  # of action: created,changed,deleted.
  # then finally, in the case of multiple changes and no volume change 
  # (price update) order by our timestamp. (this is for exchanges that allow 
  # in-place event updates)
  events <- events[order(events[, "id"], -events[, "volume"], 
      events[, "action"], events[, "timestamp"]), ]

  events <- cbind(event.id=1:nrow(events), events)

  fill.deltas <- unlist(tapply(events$volume, events$id, vectorDiff), 
      use.names=F)
  # for pacman orders, do not log volume for price update events.
  fill.deltas <- ifelse(unlist(tapply(events$price, events$id, vectorDiff), 
      use.names=F) == 0, fill.deltas, 0)
  events <- cbind(events, fill=abs(fill.deltas))

  ### fix timestamps: most of the time the event stream is out of order.
  ### the events have been ordered by id, volume, then action. now re-assign 
  ### the timestamps to match this order for each order id.
  ts.ordered <- unlist(tapply(events$timestamp, events$id, sort), use.names=F)
  events$timestamp <- as.POSIXct(ts.ordered, origin="1970-01-01", tz="UTC")

  events
}

##' Calculate order aggressiveness with respect to best bid or ask in BPS.
##' 
##' Added or deleted limit orders can be assigned a level of "aggressiveness"
##' with respect to the current best bid (ask) immediately before the order is
##' added or removed from the order book. Orders placed at the best bid (ask)
##' are assigned an aggressiveness of 0 BPS, a negative BPS below the best bid
##' (ask) and a positive BPS if placed inside the spread.
##' 
##' @param events The events data.frame.
##' @param depth.summary Order book summary statistics.
##' @return The events data.frame containing a new aggressiveness.bps column.
##' @author phil
##' @keywords internal
orderAggressiveness <- function(events, depth.summary) {
  eventDiffBps <- function(events, direction) {
    orders <- events[events$direction == ifelse(direction == 1, "bid", "ask") & 
        events$action != "changed" & (events$type == "flashed-limit" | 
        events$type == "resting-limit"), ]
    orders <- orders[order(orders$timestamp), ]
    stopifnot(all(orders$timestamp %in% depth.summary$timestamp))
    best <- depth.summary[match(orders$timestamp, depth.summary$timestamp), 
        ifelse(direction == 1, "best.bid.price", "best.ask.price")]
    orders <- tail(orders, -1)
    best <- head(best, -1)
    diff.price <- direction * (orders$price - best)
    diff.bps <- 10000*diff.price/best
    data.frame(event.id=orders$event.id, diff.bps=diff.bps) 
  }
  bid.diff <- eventDiffBps(events, 1)
  ask.diff <- eventDiffBps(events, -1)
  events$aggressiveness.bps <- NA
  events[match(bid.diff$event.id, events$event.id), ]$aggressiveness.bps <- 
     bid.diff$diff.bps
  events[match(ask.diff$event.id, events$event.id), ]$aggressiveness.bps <- 
     ask.diff$diff.bps
      
  events
}

