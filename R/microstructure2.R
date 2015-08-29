##' microstructure2.
##'
##' Limit order book event analysis.
##'
##' Main functionality:
##' \itemize{
##'   \item Limit order book event processing and analysis.
##'   \item Visualise order book state and market impacts.
##'   \item Order book reconstruction.
##' }
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
##' A data.table containing the lifecycle of limit orders.
##'
##' The purpose of this table is to keep account of the lifecycle of all orders
##' in both sides of the limit order book. The lifecycle of an individual limit
##' order follows a sequence of events:
##'
##' \describe{
##'   \item{created}{The order is created with a specified amount of volume and
##' a limit price.}
##'   \item{changed}{0 or more modification events occur when the order is
##' partially filled. On each modification, the remaining volume will decrease.}
##'   \item{deleted}{The order may be deleted at the request of the trader or, in
##' the event that the order has been completely filled, deleted by the exchange.
##' An order deleted by the exchange as a result of being filled with have 0
##' remaining volume at time of deletion.}
##' }
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
##'   \item{flashed-limit}{Order was created then subsequently deleted. 96\% of
##' example data.}
##'   \item{resting-limit}{Order was created and left in order book indefinitely
##' until filled.}
##'   \item{market-limit}{Order was partially filled before landing in the order
##' book at it's limit price.}
##'   \item{market}{Order was completely filled and did not come to rest in the
##' order book.}
##'   \item{pacman}{A limit-price modified \emph{in situ} (exchange algorithmic
##' order).}
##' }
##' 
##' @family Limit order book data
NULL

##' Trades.
##'
##' Inferred trades (executions).
##'
##' The trades data.table contains a log of all executions ordered by local
##' timestamp. In addition to the usual timestamp, price and volume information,
##' each row also contains the trade direction (buyer or seller initiated) and
##' maker/taker limit order ids. The maker/taker event and limit order ids can
##' be used to group trades into market impacts. See:
##' \code{\link{tradeImpacts}}.
##'
##' @docType data
##' @keywords datasets
##' @name trades
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{timestamp}{Local event timestamp.}
##'   \item{price}{Price at which the trade occured.}
##'   \item{volume}{Amount of traded volume.}
##'   \item{direction}{The trade direction: \emph{buy} or \emph{sell}.}
##'   \item{maker.event.id}{Corresponding market \emph{making} event id in
##' \code{\link{events}}.}
##'   \item{taker.event.id}{Corresponding market \emph{taking} event id in
##' \code{\link{events}}.}
##'   \item{maker}{Id of the market \emph{making} limit order in
##' \code{\link{events}}.}
##'   \item{taker}{Id of the market \emph{taking} limit order in
##' \code{\link{events}}.}
##' }
##' 
##' @family Limit order book data
NULL

##' Depth.
##'
##' Price level depth (liquidity) through time.
##'
##' The depth data.table describes the amount of available volume for all price
##' levels in the limit order book through time. Each row corresponds to a limit
##' order event, in which volume has been added or removed.
##'
##' @docType data
##' @keywords datasets
##' @name depth
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{timestamp}{Time at which volume was added or removed.}
##'   \item{price}{Order book price level.}
##'   \item{volume}{Amount of remaining volume at this price level.}
##'   \item{side}{The side of the price level: \emph{bid} or \emph{ask}.}
##' }
##' 
##' @family Limit order book data
NULL

##' Depth summary.
##'
##' Limit order book summary statistics.
##'
##' Various summary statistics describing the state of the order book after
##' every limit order event. The metrics are intended to quantify the
##' \emph{shape} of the order book through time.
##'
##' @docType data
##' @keywords datasets
##' @name depth.summary
##' @author phil
##' @format A data.frame consisting of the following fields:
##' \describe{
##'   \item{timestamp}{Local timestamp corresponding to \code{\link{events}}.}
##'   \item{best.bid.price}{Best bid price.}
##'   \item{best.bid.vol}{Amount of volume available at the best bid.}
##'   \item{bid.vol25:500bps}{The amount of volume available for 20 25bps
##' percentiles below the best bid.}
##'   \item{bid.vwap25:500bps}{The VWAP for 20 25bps percentiles below the best
##' bid.}
##'   \item{best.ask.price}{The best ask price.}
##'   \item{best.ask.vol}{Amount of volume available at the best ask.}
##'   \item{ask.vol25:500bps}{The amount od volume available for 20 25bps
##' percentiles above the best ask.}
##'   \item{ask.vwap25:500bps}{The VWAP for 20 25bps percentiles above the best
##' ask.}
##' }
##' 
##' @family Limit order book data
NULL
