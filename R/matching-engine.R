##' Match Market Orders (takers) to Limit Orders (makers). 
##'
##' Matches corresponding Bid(s) and Ask(s) for each trade event. A trade event
##' (a market impact) will generate a list of volume change events. This 
##' function will line up 2 time ordered event lists for each side of the book
##' and attempt to align them by matching volume. If the result contains 
##' duplicate matches, then the matching is treated as a sequence alignment 
##' problem, and the Needleman-Wunsch algorithm is applied. As such, the
##' function acts as a type of "one shot" matching engine, simulating an order
##' book matching engine without the need to reconstruct the whole book.
##' 
##' @param events data frame of order book events. 
##' @param cut.off.ms events occuring outside of this time (in milliseconds) 
##'        will be considered as candidate matches.  
##' @keywords internal
eventMatch <- function(events, cut.off.ms=5000) {
  matcher <- function() {
    logger(paste("matching", nrow(events), "events..."))

    res <- integer()
    cols <- c("event.id", "fill", "timestamp")
    bid.fills <- events[events$direction=="bid" & events$fill != 0, cols]
    ask.fills <- events[events$direction=="ask" & events$fill != 0, cols]

    # identifiable bid and ask fills.
    id.bid.fills <- bid.fills[bid.fills$fill %in% ask.fills$fill, ]
    id.bid.fill.order <- order(-id.bid.fills$fill, id.bid.fills$timestamp)
    id.bid.fills <- id.bid.fills[id.bid.fill.order, ]

    id.ask.fills <- ask.fills[ask.fills$fill %in% bid.fills$fill, ]
    id.ask.fill.order <- order(-id.ask.fills$fill, id.ask.fills$timestamp)
    id.ask.fills <- id.ask.fills[id.ask.fill.order, ]

    for(volume in unique(id.bid.fills$fill)) {
      bids <- id.bid.fills[id.bid.fills$fill == volume, ]
      asks <- id.ask.fills[id.ask.fills$fill == volume, ]
      distance.matrix.ms <- sapply(bids$timestamp, function(b) {
        as.integer(difftime(b, asks$timestamp, units="secs")*1000)
      })

      # only 1 ask. convert to 1 row matrix.
      if(!is.matrix(distance.matrix.ms)) {
        distance.matrix.ms <- t(distance.matrix.ms)
      }

      # first try simple matching:
      # for each bid timestamp, return closest ask timestamp.
      ask.event.ids <- apply(abs(distance.matrix.ms), 2,
          function(column.vector) {
        min.idx <- which.min(column.vector)
        ifelse(column.vector[min.idx] <= cut.off.ms, 
            asks[min.idx, "event.id"], NA)
      })

      if(all(!duplicated(ask.event.ids))) {
        matches <- cbind(bids$event.id, ask.event.ids)
        matches <- matches[!is.na(matches[, 2]), ]
        res <- rbind(res, matches)
      } else {

        # there is a chance 1 bid matched multiple ask timestamps, so first 
        # matching attempt failed... now use alignment algorithm to find
        # optimal alignment. note that the first attempt can fail if multiple 
        # orders for the same volume occur in rapid fire. could group and then 
        # process events by "bursts"/"pulses".

        # same as: t(ifelse(abs(distance.matrix.ms) <= 5000, 1, -1))
        sm <- sMatrix(bids$timestamp, asks$timestamp, filter=function(f1, f2) {
          ifelse(abs(as.integer(difftime(f1, f2, units="secs")*1000)) <=
              cut.off.ms, 1, -1)
        })

        aligned.idx <- alignS(s.matrix=sm)

        matched.bids <- bids[aligned.idx[, 1], ]
        matched.asks <- asks[aligned.idx[, 2], ]
        in.bounds <- abs(difftime(matched.bids$timestamp,
            matched.asks$timestamp, units="secs")*1000) <= cut.off.ms

        res <- rbind(res, cbind(matched.bids[in.bounds, ]$event.id, 
            matched.asks[in.bounds, ]$event.id))
      }

    }
    res
  }
  
  matched <- matcher()

  stopifnot(all(!duplicated(matched[, 1])))
  stopifnot(all(!duplicated(matched[, 2])))

  events <- cbind(events, matching.event=NA)
  # ensure bid event.id's in same order a events matrix
  matched <- matched[order(matched[, 1]), ]
  #bid->ask events
  events[events$event.id %in% matched[, 1], ]$matching.event <- matched[, 2]
  # ensure ask event.id's in same order a events matrix
  matched <- matched[order(matched[, 2]), ]
  # ask->bid events
  events[events$event.id %in% matched[, 2], ]$matching.event <- matched[, 1]

  events
}

