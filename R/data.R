options(digits.secs=3)
options(scipen=999)

load.data <- function(bin.file) {
  load(file=bin.file, verbose=T)
  ob.data
}

process.data <- function(csv.file) {
  events <- load.event.data(csv.file)
  trades <- match.trades(events)
  events <- set.order.types(events, trades)
  zombie.ids <- get.zombie.ids(events, trades)
  zombies <- events[events$id %in% zombie.ids, ]
  events <- events[!events$id %in% zombie.ids, ]
  print(paste("removed", length(zombie.ids), "zombies"))
  # remove update events that contain no new information. for example:
  # 85155    86392 34147557 2014-08-22 07:44:15.976 2014-08-22 07:44:15   507  5e+06 created       ask resting-limit 0e+00             NA
  # 85159    86393 34147557 2014-08-22 07:44:17.226 2014-08-22 07:44:15   507  5e+06 changed       ask resting-limit 0e+00             NA <-- duplicate 
  # 90893    86394 34147557 2014-08-22 08:19:00.611 2014-08-22 07:44:15   507  0e+00 deleted       ask resting-limit 5e+06          92151
  created.ids <- events[events$action == "created", ]$id
  duplicate.update.event.ids <- events[events$type != "pacman" & events$action == "changed" & events$fill == 0 & events$id %in% created.ids, ]$event.id
  events <- events[!events$event.id %in% duplicate.update.event.ids, ]
  print(paste("removed", length(duplicate.update.event.ids), "duplicated updates"))
  depth <- price.level.volume(events)
  print("calculating depth metrics (may take some time...)")
  depth.summary <- depth.metrics(depth)
  print("calculating order aggressiveness...")
  events <- order.aggressiveness(events, depth.summary)
  list(
    events=events, 
    trades=trades, 
    depth=depth, 
    zombies=zombies, 
    depth.summary=depth.summary
  )
}

save.data <- function(ob.data, bin.file) {
  print("saving binary")
  save(file=bin.file, ob.data, compress="bzip2")
}

get.zombie.ids <- function(events, trades) {
  cancelled <- events[events$action == "deleted", ]$id
  zombies <- events[!events$id %in% cancelled, ]
  bid.zombies <- zombies[zombies$direction == "bid", ]
  ask.zombies <- zombies[zombies$direction == "ask", ]
  bid.zombie.ids <- unique(bid.zombies[unlist(lapply(bid.zombies$id, function(id) {
    zombie <- tail(bid.zombies[bid.zombies$id == id, ], 1)
    any(trades$direction == "sell" & trades$timestamp >= zombie$timestamp & trades$price < zombie$price)
  })), "id"])
  ask.zombie.ids <- unique(ask.zombies[unlist(lapply(ask.zombies$id, function(id) {
    zombie <- tail(ask.zombies[ask.zombies$id == id, ], 1)
    any(trades$direction == "buy" & trades$timestamp >= zombie$timestamp & trades$price > zombie$price)
  })), "id"])
  c(bid.zombie.ids, ask.zombie.ids)
}

