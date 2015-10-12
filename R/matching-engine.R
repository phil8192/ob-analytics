##' Match Market Orders (takers) to Limit Orders (makers). 
##'
##' Matches corresponding Bid(s) and Ask(s) for each trade event.
##'
##' A trade event (a market impact) will generate a list of volume change events.
##' This function will line up 2 time ordered event lists for each side of the
##' book and attempt to align them by matching volume. If the result contains 
##' duplicate matches, then the matching is treated as a sequence alignment 
##' problem, and the Needleman-Wunsch algorithm is applied. As such, the
##' function acts as a type of "one shot" matching engine, simulating an order
##' book matching engine without the need to reconstruct the whole book.
##' 
##' @param events data frame of order book events. 
##' @param cut.off.ms events occuring outside of this time (in milliseconds) 
##'        will be considered as candidate matches.  
##' @author phil
##' @examples
##'
##' asTime <- function(s) as.POSIXct(s, tz="UTC")
##' events <- data.frame(timestamp=c(asTime("2015-10-10 21:32:00.000"),
##'                                  asTime("2015-10-10 21:32:00.010"),
##'                                  asTime("2015-10-10 21:32:10.000"),
##'                                  asTime("2015-10-10 21:32:10.010")),
##'                       direction=c("bid", "ask", "bid", "ask"),
##'                       event.id=c(1, 2, 3, 4),
##'                       fill=rep(1234, 4))
##' matched <- obAnalytics:::eventMatch(events, cut.off.ms=1000)
##' 
##' @keywords internal
eventMatch <- function(events, cut.off.ms=5000) {
  matcher <- function() {
    logger(paste("matching", nrow(events), "events..."))

    res <- integer()
    cols <- c("event.id", "fill", "timestamp")

    bid.fills <- events[events$direction=="bid" & events$fill != 0, cols]
    ask.fills <- events[events$direction=="ask" & events$fill != 0, cols]

    # identifiable bid and ask fills.
    fillId <- function(src, dst) {
      id.fills <- src[src$fill %in% dst$fill, ]
      id.fills[order(-id.fills$fill, id.fills$timestamp), ]
    }
    id.bid.fills <- fillId(bid.fills, ask.fills)
    id.ask.fills <- fillId(ask.fills, bid.fills)  
      
    for(volume in unique(id.bid.fills$fill)) {
      bids <- id.bid.fills[id.bid.fills$fill == volume, ]
      asks <- id.ask.fills[id.ask.fills$fill == volume, ]
      distance.matrix.ms <- sapply(bids$timestamp, function(b) {
        as.numeric(difftime(b, asks$timestamp, units="secs")*1000)
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
        sm <- sMatrix(bids$timestamp, asks$timestamp, filter=function(f1, f2) {
          diff.ms <- abs(as.numeric(difftime(f1, f2, units="secs")*1000)) 
          ifelse(diff.ms == 0, cut.off.ms, cut.off.ms/diff.ms)
        })

        aligned.idx <- alignS(s.matrix=sm, gap=-1)

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
  bid.matches <- match(matched[, 1], events$event.id)
  ask.matches <- match(matched[, 2], events$event.id)

  events[bid.matches, ]$matching.event <- matched[, 2]
  events[ask.matches, ]$matching.event <- matched[, 1]  
    
  events
}

