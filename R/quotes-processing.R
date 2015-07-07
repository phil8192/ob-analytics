
# for some reason, stream sometimes contains duplicate delete events.
# most of time order deleted, but then filled. sometimes duplicates (exactly 
# the same).
remove.duplicates <- function(events) {
  deletes <- events[events$action == "deleted", ]
  deletes <- deletes[order(deletes$id, deletes$volume), ]
  duplicate.deletes <- deletes[deletes$id %in% 
      deletes[which(duplicated(deletes$id)), ]$id, ]
  duplicate.event.ids <- duplicate.deletes[duplicated(duplicate.deletes$id), 
      ]$event.id
  logger(paste("removed", length(duplicate.event.ids), 
      "duplicate order cancellations: ", 
      paste(events[duplicate.event.ids, ]$id, collapse=" ")))
  events[!events$event.id %in% duplicate.event.ids, ]
}

load.event.data <- function(file) {

  logger(paste("loading data from", file))

  events <- read.csv(file, header=T, sep=",")
  events$timestamp <- as.POSIXct(events$timestamp/1000, origin="1970-01-01", 
      tz="UTC")
  events$exchange.timestamp <- as.POSIXct(events$exchange.timestamp/1000, 
      origin="1970-01-01", tz="UTC")
  # an order can be in 1 of these 3 ordered states.
  events$action <- factor(events$action, c("created", "changed", "deleted"))
  events$direction <- factor(events$direction, c("bid", "ask"))

  # order quote data by id, then by volume (decreasing), then finally, by order 
  # of action: created,changed,deleted.
  # then finally, in the case of multiple changes and no volume change 
  # (price update) order by our timestamp. (this is for exchanges that allow 
  # in-place quote updates)
  events <- events[order(events[, "id"], -events[, "volume"], 
      events[, "action"], events[, "timestamp"]), ]

  events <- cbind(event.id=1:nrow(events), events)
  events <- remove.duplicates(events)

  fill.deltas <- unlist(tapply(events$volume, events$id, vector.diff), 
      use.names=F)
  # for pacman orders, do not log volume for price update events.
  fill.deltas <- ifelse(unlist(tapply(events$price, events$id, vector.diff), 
      use.names=F) == 0, fill.deltas, 0)
  events <- cbind(events, fill=abs(fill.deltas))

  ### fix timestamps: most of the time the event stream is out of order.
  ### the events have been ordered by id, volume, then action. now re-assign 
  ### the timestamps to match this order for each order id.
  logger("realigning event timestamps...")
  ts.ordered <- unlist(tapply(events$timestamp, events$id, sort), use.names=F)
  events$timestamp <- as.POSIXct(ts.ordered, origin="1970-01-01", tz="UTC")

  matched <- event.match(events, 5000) #within 5 seconds.
  stopifnot(all(!duplicated(matched[, 1])))
  stopifnot(all(!duplicated(matched[, 2])))

  events <- cbind(events, matching.event=NA)
  # ensure bid event.id's in same order a events matrix
  matched <- matched[order(matched[, 1]), ]
  #bid->ask events
  events[events$event.id %in% matched[, 1], ]$matching.event <- matched[, 2]
  # ensure ask event.id's in same order a events matrix
  matched <- matched[order(matched[, 2]), ]
  #ask->bid events
  events[events$event.id %in% matched[, 2], ]$matching.event <- matched[, 1]

  events
}

# distance of bid/ask limit order from best bid/ask (immediately before order 
# added/deleted) in bps.
# "aggressiveness" of limit order.
# < 0 = below/above best bid/ask
# = 0 = on best bid/ask (most activity)
# > 0 = inside spread. (aggressors)
order.aggressiveness <- function(events, depth.summary) {
  event.diff.bps <- function(events, direction) {
    orders <- events[events$direction == ifelse(direction == 1, "bid", "ask") & 
        events$action != "changed" & (events$type == "flashed-limit" | 
        events$type == "resting-limit"), ]
    orders <- orders[order(orders$timestamp), ]
    stopifnot(all(orders$timestamp %in% depth.summary$timestamp))
    best <- depth.summary[match(orders$timestamp, depth.summary$timestamp), 
        ifelse(direction == 1, "best.bid.price", "best.ask.price")]
    orders <- tail(orders, -1)
    best <- head(best, -1)
    diff.cents <- direction * (100*orders$price - best)
    diff.bps <- 10000*diff.cents/best
    data.frame(event.id=orders$event.id, diff.bps=diff.bps) 
  }
  bid.diff.bps <- event.diff.bps(events, 1)
  ask.diff.bps <- event.diff.bps(events, -1)
  events <- cbind(events, aggressiveness.bps=NA)
  events[match(bid.diff.bps$event.id, events$event.id), ]$aggressiveness.bps <- 
      bid.diff.bps$diff.bps
  events[match(ask.diff.bps$event.id, events$event.id), ]$aggressiveness.bps <- 
      ask.diff.bps$diff.bps
 
  events
}

