## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

##' Determine limit order types.
##'
##' This function infers order type given trade and order event data.
##' The following categories are assigned to the event data type field:
##' \describe{
##'   \item{unknown}{It was not possible to determine the order type}
##'   \item{flashed-limit}{An order which was created and at some future
##' time deleted without ever being hit}
##'   \item{resting-limit}{The order was added and left to rest in the
##' order book. it may or may not be hit later}
##'   \item{market-limit}{This is a limit order that crosses the book,
##' it's volume is filled until it's limit price is reached, at which
##' point the order comes to land in the book}
##'   \item{pacman}{This is a special type of (algorithmic) order executed
##' by the exchange (the order "eats" the best bid or ask at intervals
##' until filled.}
##'   \item{market}{This is an order that crosses the book, it's volume
##' is filled before it's limit price is reached (order never comes to
##' land in the book.) A market order is the most aggressive order type.}
##' }
##' @param events Limit order event data.
##' @param trades Execution data.
##' @return The limit order event data with updated type field.
##' @author phil
##' @keywords internal
setOrderTypes <- function(events, trades) {
  isPacman <- function(events) {
    tapply(events$price, events$id, function(prices) {
      any(vectorDiff(prices) != 0)
    })
  }

  events$type <- "unknown"
  events$type <- factor(events$type, c("unknown", "flashed-limit", 
      "resting-limit", "market-limit", "pacman", "market"))

  # pacman orders (this needs to be determined first)
  pac.men <- which(isPacman(events))
  if(length(pac.men) > 0)
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
  created.deleted.ids <- created[(!created$id %in% changed$id) & 
      created$id %in% deleted$id, ]$id 
  volume.matched <- deleted[deleted$id %in% created.deleted.ids, ]$volume == 
      created[created$id %in% created.deleted.ids, ]$volume
  flashed.ids <- created.deleted.ids[volume.matched] 
  forever.ids <- created[!created$id %in% changed$id & 
      !created$id %in% deleted$id, ]$id
  # only ever a maker. never a pacman.
  maker.ids <- unique(events[events$event.id %in% trades$maker.event.id, ]$id)
  taker.ids <- unique(events[events$event.id %in% trades$taker.event.id, ]$id)
  pacman.ids <- unique(events[events$type=="pacman", ]$id)
  maker.ids <- maker.ids[!maker.ids %in% taker.ids]
  maker.ids <- maker.ids[!maker.ids %in% pacman.ids]
  if(length(flashed.ids) > 0)
    events[events$id %in% flashed.ids, ]$type <- "flashed-limit"
  if(length(forever.ids) > 0 || length(maker.ids) > 0)
    events[events$id %in% forever.ids | 
           events$id %in% maker.ids, ]$type <- "resting-limit"

  # market limit. a market limit order starts out as a market order and comes 
  # to rest in the order book when the limit is reached.
  # orders that have been _both_ maker and taker but not pacman.
  ml.ids <- taker.ids[taker.ids %in% unique(events[events$event.id %in% 
      trades$maker.event.id, ]$id)]
  ml.ids <- ml.ids[!ml.ids %in% pacman.ids]
  if(length(ml.ids) > 0)
    events[events$id %in% ml.ids, ]$type <- "market-limit"

  # market orders: at least 1 taking event, no identified making events.
  mo.ids <- taker.ids[!taker.ids %in% unique(events[events$event.id %in% 
      trades$maker.event.id, ]$id)]
  mo.ids <- mo.ids[!mo.ids %in% pacman.ids]
  if(length(mo.ids) > 0)
    events[events$id %in% mo.ids, ]$type <- "market"  
  
  unidentified <- length(which(events$type=="unknown"))
  if(unidentified > 0)
    warning(paste("could not identify", unidentified, "orders")) 

  events
}
