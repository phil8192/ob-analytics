##' microstructure2.
##'
##' package description.
##'
##' package details.
##' 
##' @name microstructure2
##' @docType package
##' @import ggplot2 zoo
##' @importFrom reshape2 melt
##' @author phil nocturne \email{phil@@parasec.net}
##' @references \url{http://parasec.net/transmission/order-book-visualisation}
NULL

##' Example limit order book data.
##'
##' 50,393 limit order events. 482 trades.
##'
##' 5 hours of limit order book event data obtained from the Bitstamp (bitcoin)
##' exchange on 2015-05-01 (midnight until 5am). The data has been preprocessed
##' with the \code{\link{processData}} function.
##' 
##' @docType data
##' @keywords datasets
##' @name lob.data
##' @usage data(lob.data)
##' @source \url{https://www.bitstamp.net/websocket}
##' @references \url{https://github.com/phil8192/ticker}
##' @author phil
##' @format A list containing 4 data frames as returned by
##'         \code{\link{processData}}
##' @seealso \code{\link{events}},
##'          \code{\link{trades}},
##'          \code{\link{depth}},
##'          \code{\link{depth.summary}}
NULL

##' Limit order events.
##'
##' event desc.
##'
##' test details
##'
##' @docType data
##' @keywords datasets
##' @name events
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{event.id}{Event ID.}
##'   \item{id}{Limit Order ID.}
##'   \item{timestamp}{Local timestamp for order update (create/modify/delete).}
##'   \item{exchange.timestamp}{Exchange order creation time.}
##'   \item{price}{Limit order price level.}
##'   \item{volume}{Remaining limit order volume.}
##'   \item{action}{Event action: created, changed, deleted.}
##'   \item{direction}{Order book side: bid, ask.}
##'   \item{fill}{For changed or deleted events, indicates the change in volume.}
##'   \item{matching.event}{Matching \code{event.id} if this event is part of a
##' trade. \code{NA} otherwise.}
##'   \item{type}{Limit order type (see \emph{Event types} below.)}
##'   \item{aggressiveness.bps}{The distance of the order from the edge of the
##' book in Basis Points (BPS).}
##' }
##'
##' Each limit order \emph{type} has been categorised as follows:
##' 
##' \describe{
##'   \item{unknown}{It was not possible to infer the order type given the
##' available data.}
##'   \item{flashed-limit}{Order was created then subsequently deleted. 96% of
##' example data.}
##'   \item{resting-limit}{Order was created and left in order book indefinitely
##' until filled.}
##'   \item{market-limit}{Order was partially filled before landing in the order
##' book at it's limit price.}
##'   \item{market}{Order was completely filled and did not come to rest in the
##' order book.}
##'   \item{pacman}{An order which is modified \emph{in situ} (a exchange
##' algorithmic order).}
##' }
##' 
##' @family Limit order book data
NULL

##' Trades.
##'
##' Inferred trades (executions).
##'
##' trade details
##'
##' @docType data
##' @keywords datasets
##' @name trades
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{event.id}{}
##'   \item{id}{}
##'   \item{timestamp}{}
##'   \item{exchange.timestamp}{}
##'   \item{price}{}
##'   \item{volume}{}
##'   \item{action}{}
##'   \item{direction}{}
##'   \item{fill}{}
##'   \item{matching.event}{}
##'   \item{type}{}
##'   \item{aggressiveness.bps}{}
##' }
##' @family Limit order book data
NULL

##' Depth.
##'
##' Inferred trades (executions).
##'
##' trade details
##'
##' @docType data
##' @keywords datasets
##' @name depth
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{event.id}{}
##'   \item{id}{}
##'   \item{timestamp}{}
##'   \item{exchange.timestamp}{}
##'   \item{price}{}
##'   \item{volume}{}
##'   \item{action}{}
##'   \item{direction}{}
##'   \item{fill}{}
##'   \item{matching.event}{}
##'   \item{type}{}
##'   \item{aggressiveness.bps}{}
##' }
##' @family Limit order book data
NULL

##' Depth summary.
##'
##' Inferred trades (executions).
##'
##' trade details
##'
##' @docType data
##' @keywords datasets
##' @name depth.summary
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{event.id}{}
##'   \item{id}{}
##'   \item{timestamp}{}
##'   \item{exchange.timestamp}{}
##'   \item{price}{}
##'   \item{volume}{}
##'   \item{action}{}
##'   \item{direction}{}
##'   \item{fill}{}
##'   \item{matching.event}{}
##'   \item{type}{}
##'   \item{aggressiveness.bps}{}
##' }
##' @family Limit order book data
NULL

