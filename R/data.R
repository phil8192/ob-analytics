##' Import CSV file containing limit order events.
##'
##' Imports and performs pre-processing of limit order data contained in a CSV.
##'
##' @param csv.file Location of CSV file to import
##' @return A list of order book events, trades, order book depth and summary
##' @author phil
##' @examples
##' \dontrun{
##'
##' csv.file <- system.file("extdata", "orders.csv.bz2", package="microstructure2") 
##' lob.data <- processData(csv.file)
##'
##' }
##' @export processData
processData <- function(csv.file) {
  getZombieIds <- function(events, trades) {
    cancelled <- events[events$action == "deleted", ]$id
    zombies <- events[!events$id %in% cancelled, ]
    bid.zombies <- zombies[zombies$direction == "bid", ]
    ask.zombies <- zombies[zombies$direction == "ask", ]
    bid.zombie.ids <- unique(bid.zombies[unlist(lapply(bid.zombies$id, 
        function(id) {
      zombie <- tail(bid.zombies[bid.zombies$id == id, ], 1)
      any(trades$direction == "sell" & trades$timestamp >= zombie$timestamp 
          & trades$price < zombie$price)
    })), "id"])
    ask.zombie.ids <- unique(ask.zombies[unlist(lapply(ask.zombies$id, 
        function(id) {
      zombie <- tail(ask.zombies[ask.zombies$id == id, ], 1)
      any(trades$direction == "buy" & trades$timestamp >= zombie$timestamp 
          & trades$price > zombie$price)
    })), "id"])
    c(bid.zombie.ids, ask.zombie.ids)
  }

  events <- loadEventData(csv.file)
  events <- eventMatch(events)
  trades <- matchTrades(events)
  events <- setOrderTypes(events, trades)
  zombie.ids <- getZombieIds(events, trades)
  zombies <- events[events$id %in% zombie.ids, ]
  events <- events[!events$id %in% zombie.ids, ]
  logger(paste("removed", length(zombie.ids), "zombies"))
  created.ids <- events[events$action == "created", ]$id
  duplicate.update.event.ids <- events[events$type != "pacman" & events$action
      == "changed" & events$fill == 0 & events$id %in% created.ids, ]$event.id
  events <- events[!events$event.id %in% duplicate.update.event.ids, ]
  logger(paste("removed", length(duplicate.update.event.ids), 
      "duplicated updates"))
  depth <- priceLevelVolume(events)
  logger("calculating depth metrics (may take some time...)")
  depth.summary <- depthMetrics(depth)
  logger("calculating order aggressiveness...")
  events <- orderAggressiveness(events, depth.summary)
  # depth summary data starts 1 minute later to allow for order book population.
  offset <- min(events$timestamp) + 60
  list(
    events=events, 
    trades=trades, 
    depth=depth, 
    depth.summary=depth.summary[depth.summary$timestamp >= offset, ]
  )
}

##' Load pre-processed data. 
##'
##' Loads previously saved pre-processed data.
##' @param bin.file File location.
##' @return Limit order, trade and depth data structure.
##' @author phil
##' @export loadData
loadData <- function(bin.file) {
  logger(paste("loading binary from", bin.file))
  readRDS(file=bin.file)
}

##' Save processed data.
##'
##' Saves processed data to file. 
##' @param lob.data Limit order, trade and depth data structure.
##' @param bin.file File location. 
##' @author phil
##' @export saveData
saveData <- function(lob.data, bin.file) {
  logger(paste("saving binary to", bin.file))
  saveRDS(lob.data, file=bin.file)
}
