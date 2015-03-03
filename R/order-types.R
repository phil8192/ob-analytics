
is.pacman <- function(quotes) {
  tapply(quotes$price, quotes$id, function(prices) {
    any(vector.diff(prices) != 0)
  })
}

set.order.types <- function(events, trades) {
  print("identifying order types...")

  events$type <- "unknown"
  events$type <- factor(events$type, c("unknown", "flashed-limit", "resting-limit", "market-limit", "pacman", "market"))

  # pacman orders (this needs to be determined first)
  pac.men <- which(is.pacman(events))
  print(paste("found", length(pac.men), "pacman orders"))
  events[which(events$id %in% names(pac.men)), ]$type <- "pacman"
  
  # flashed and resting limit orders.
  # an order is an order that is never filled, or has only ever been a maker.
  # this includes "flashed" orders and orders that sit in the book forever.
  created <- events[events$action=="created", ]
  created <- created[order(created$id), ]
  deleted <- events[events$action=="deleted", ]
  deleted <- deleted[order(deleted$id), ]
  changed <- events[events$action=="changed", ]
  # never filled   
  created.deleted.ids <- created[(!created$id %in% changed$id) & created$id %in% deleted$id, ]$id 
  volume.matched <- deleted[deleted$id %in% created.deleted.ids, ]$volume == created[created$id %in% created.deleted.ids, ]$volume
  flashed.ids <- created.deleted.ids[volume.matched] 
  forever.ids <- created[!created$id %in% changed$id & !created$id %in% deleted$id, ]$id 
  # only ever a maker. never a pacman.
  maker.ids <- unique(events[events$event.id %in% trades$maker.event.id, ]$id)
  taker.ids <- unique(events[events$event.id %in% trades$taker.event.id, ]$id)
  pacman.ids <- unique(events[events$type=="pacman", ]$id)
  maker.ids <- maker.ids[!maker.ids %in% taker.ids]
  maker.ids <- maker.ids[!maker.ids %in% pacman.ids]
  print(paste("found", length(flashed.ids), "flashed-limit and", length(forever.ids)+length(maker.ids), "resting orders"))
  events[events$id %in% flashed.ids, ]$type <- "flashed-limit"
  events[events$id %in% forever.ids | events$id %in% maker.ids, ]$type <- "resting-limit"

  # market limit. a market limit order starts out as a market order and comes to rest in the order book when the limit is reached.
  # orders that have been _both_ maker and taker but not pacman.
  ml.ids <- taker.ids[taker.ids %in% unique(events[events$event.id %in% trades$maker.event.id, ]$id)]
  ml.ids <- ml.ids[!ml.ids %in% pacman.ids]
  print(paste("found", length(ml.ids), "market-limit orders"))
  events[events$id %in% ml.ids, ]$type <- "market-limit"

  # market orders: at least 1 taking event, no identified making events.
  mo.ids <- taker.ids[!taker.ids %in% unique(events[events$event.id %in% trades$maker.event.id, ]$id)]
  mo.ids <- mo.ids[!mo.ids %in% pacman.ids]
  print(paste("found", length(mo.ids), "market orders"))
  events[events$id %in% mo.ids, ]$type <- "market"  

  print(paste("could not identify", length(which(events$type=="unknown")), "orders"))

  events
}

