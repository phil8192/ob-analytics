#' @export
load.data <- function(bin.file) {
  load(file=bin.file, verbose=T)
  ob.data
}

##' Import CSV file containing limit order events.
##'
##' Imports and performs pre-processing of limit order data contained in a CSV.
##'
##' @param csv.file Location of CSV file to import
##' @return A list of order book events, trades, order book depth and summary
##' @author phil
##' @export process.data
##' @examples
##' \donotrun{
##' csv.file <- system.file("extdata", "2015-05-01.csv", package="microstructure2") 
##' lob.data <- process.data(csv.file)
##' }
process.data <- function(csv.file) {
  events <- load.event.data(csv.file)
  trades <- match.trades(events)
  events <- set.order.types(events, trades)
  zombie.ids <- get.zombie.ids(events, trades)
  zombies <- events[events$id %in% zombie.ids, ]
  events <- events[!events$id %in% zombie.ids, ]
  logger(paste("removed", length(zombie.ids), "zombies"))
  created.ids <- events[events$action == "created", ]$id
  duplicate.update.event.ids <- events[events$type != "pacman" & events$action
      == "changed" & events$fill == 0 & events$id %in% created.ids, ]$event.id
  events <- events[!events$event.id %in% duplicate.update.event.ids, ]
  logger(paste("removed", length(duplicate.update.event.ids), 
      "duplicated updates"))
  depth <- price.level.volume(events)
  logger("calculating depth metrics (may take some time...)")
  depth.summary <- depth.metrics(depth)
  logger("calculating order aggressiveness...")
  events <- order.aggressiveness(events, depth.summary)
  list(
    events=events, 
    trades=trades, 
    depth=depth, 
    zombies=zombies, 
    depth.summary=depth.summary
  )
}

#' @export
save.data <- function(ob.data, bin.file) {
  logger("saving binary")
  save(file=bin.file, ob.data, compress="bzip2")
}

get.zombie.ids <- function(events, trades) {
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

