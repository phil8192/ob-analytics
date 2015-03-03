
event.match <- function(events, cut.off.ms) {

  print(paste("matching", nrow(events), "events..."))

  res <- integer()
  bid.fills <- events[events$direction=="bid" & events$fill != 0, c("event.id", "fill", "timestamp")]
  ask.fills <- events[events$direction=="ask" & events$fill != 0, c("event.id", "fill", "timestamp")]

  identifiable.bid.fills <- bid.fills[bid.fills$fill %in% ask.fills$fill, ]
  identifiable.bid.fills <- identifiable.bid.fills[order(-identifiable.bid.fills$fill, identifiable.bid.fills$timestamp), ]
  identifiable.ask.fills <- ask.fills[ask.fills$fill %in% bid.fills$fill, ]
  identifiable.ask.fills <- identifiable.ask.fills[order(-identifiable.ask.fills$fill, identifiable.ask.fills$timestamp), ]

  for(volume in unique(identifiable.bid.fills$fill)) {
    bids <- identifiable.bid.fills[identifiable.bid.fills$fill == volume, ]
    asks <- identifiable.ask.fills[identifiable.ask.fills$fill == volume, ]
    distance.matrix.ms <- sapply(bids$timestamp, function(b) as.integer(difftime(b, asks$timestamp, units="secs")*1000))

    # only 1 ask. convert to 1 row matrix.
    if(!is.matrix(distance.matrix.ms)) {
      distance.matrix.ms <- t(distance.matrix.ms)
    }

    # first try simple matching:
    # for each bid timestamp, return closest ask timestamp.
    ask.event.ids <- apply(abs(distance.matrix.ms), 2, function(column.vector) {
      min.idx <- which.min(column.vector)
      ifelse(column.vector[min.idx] <= cut.off.ms, asks[min.idx, "event.id"], NA)
    })

    if(all(!duplicated(ask.event.ids))) {
      matches <- cbind(bids$event.id, ask.event.ids)
      matches <- matches[!is.na(matches[, 2]), ]
      res <- rbind(res, matches)
    } else {

      # there is a chance 1 bid matched multiple ask timestamps, so first matching 
      # attempt failed... now use needleman–wunsch algorithm to find optimal alignment.
      # note that the first attempt can fail if multiple orders for the same volume occur
      # in rapid fire. could group and then process events by "bursts"/"pulses".

      # same as: t(ifelse(abs(distance.matrix.ms) <= 5000, 1, -1))
      sm <- s.matrix(bids$timestamp, asks$timestamp, filter=function(f1, f2) {
        ifelse(abs(as.integer(difftime(f1, f2, units="secs")*1000)) <= cut.off.ms, 1, -1)
      })

      aligned.idx <- align.s(s.matrix=sm)

      matched.bids <- bids[aligned.idx[, 1], ]
      matched.asks <- asks[aligned.idx[, 2], ]
      in.bounds <- abs(difftime(matched.bids$timestamp, matched.asks$timestamp, units="secs")*1000) <= cut.off.ms

      res <- rbind(res, cbind(matched.bids[in.bounds, ]$event.id, matched.asks[in.bounds, ]$event.id))

    }
  }
  res
}

