## Copyright (C) 2015,2016 Philip Stubbings <phil@parasec.net>
## Licensed under the GPL v2 license. See LICENSE.md for full terms.

##' obAnalytics.
##'
##' Limit order book analytics. 
##'
##' @section Main functionality:
##' 
##' \itemize{
##'   \item Limit order book event processing.
##'   \item Visualise order book state and market impacts.
##'   \item Order book reconstruction and analysis.
##' }
##'
##' @section Data processing:
##'
##' The main focus of this package is reconstruction of a limit order book. 
##' The \code{\link{processData}} function will perform data processing based on
##' a supplied CSV file, the schema of which is defined in the
##' \code{\link{processData}} function documentation. Example preprocessed limit
##' order data are also provided (see \code{\link{lob.data}}) which has been
##' derived from the example raw data provided the inst/extdata directory.
##'
##' The data processing consists of a number of stages:
##'
##' \itemize{
##'   \item Cleaning of duplicate and erroneous data.
##'   \item Identification of sequential event relationships.
##'   \item Inference of trade events via order-matching.
##'   \item Inference of order types (limit vs market).
##'   \item Construction of volume by price level series.
##'   \item Construction of order book summary statistics.
##' }
##'
##' Limit order events are related to one another by \emph{volume deltas} (the
##' change in volume for a limit order). To simulate a matching-engine, and thus
##' determine directional trade data, volume deltas from both sides of the limit
##' order book are ordered by time, yielding a sequence alignment problem, to
##' which the the
##' \href{https://en.wikipedia.org/wiki/Needleman-Wunsch_algorithm}{Needleman-Wunsch}
##' algorithm has been applied.
##' 
##' @section Visualisation:
##'
##' The package provides a number of functions for the visualisation of limit
##' order events and order book liquidity. The visualisations all make use of
##' the \href{http://ggplot2.org}{ggplot2} plotting system:
##'
##' \describe{
##'   \item{\link{plotTimeSeries}}{General time series plotting.}
##'   \item{\link{plotTrades}}{Plot \code{\link{trades}} data.}
##'   \item{\link{plotCurrentDepth}}{Visualise the \emph{shape} of an
##' \code{\link{orderBook}}.}
##'   \item{\link{plotPriceLevels}}{Visualise volume by price level through
##' time.}
##'   \item{\link{plotVolumePercentiles}}{Visualise order book liquidity through
##' time.}
##'   \item{\link{plotEventMap}}{Visualise sequential limit order events by price
##' level.}
##'   \item{\link{plotVolumeMap}}{Visualise sequential limit order events by
##' volume.}
##'   \item{\link{plotEventsHistogram}}{Convenience function.}
##' }
##'
##' The \code{\link{plotPriceLevels}} visualisation is designed to show the
##' \emph{ebb and flow} of limit order volume at all price levels including the
##' interplay between the bid/ask spread. It is possible to identify interesting
##' market participant behaviour and to visualise market shocks and resilience
##' with this function.
##'
##' The \code{\link{plotEventMap}} function is useful for studying systematic
##' market participant behaviour. Interesting sequential patterns can be observed
##' in this visualisation as algorithms react to various market events by
##' repositioning orders.
##'
##' The \code{\link{plotVolumeMap}} function shows a visualisation of cancelled
##' volume through time. It is possible to identify and filter out individual
##' trading algorithms from this graph.
##' 
##' The \code{\link{plotVolumePercentiles}} visualisation is inspired by the size
##' map chart included in many articles from
##' \href{http://www.nanex.net/NxResearch}{Nanex research} and is intended to
##' show available market liquidity.
##'
##' In all visualisations it is possible to filter the data by time, price and
##' volume.
##'
##' @section Analysis:
##'
##' In addition to the generated \code{\link{lob.data}} which are intended to be
##' used as a basis for further research, the package currently provides a
##' limited set of trade and order book analysis functions:
##' 
##' \describe{
##'   \item{\link{filterDepth}}{Filter \code{\link{depth}} data by time period.}
##'   \item{\link{getSpread}}{Extract the bid/ask quotes from the
##' \code{\link{depth.summary}} data.}
##'   \item{\link{orderBook}}{Reconstruct a Limit order book from
##' \code{\link{events}} data.}
##'   \item{\link{tradeImpacts}}{Group \code{\link{trades}} into individual impact events.}
##' }
##'
##' Additional functionality will be added to the package in the future.
##' 
##' @name obAnalytics-package
##' @aliases obAnalytics
##' @docType package
##' @import ggplot2 zoo
##' @importFrom reshape2 melt
##' @importFrom grDevices colorRampPalette
##' @importFrom stats aggregate quantile time
##' @importFrom utils head read.csv setTxtProgressBar tail timestamp 
##' @author Philip Stubbings \email{phil@@parasec.net}
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
##' A data.frame containing the lifecycle of limit orders.
##'
##' The purpose of this table is to keep account of the lifecycle of all orders
##' in both sides of the limit order book. The lifecycle of an individual limit
##' order follows a sequence of events:
##'
##' \describe{
##'   \item{created}{The order is created with a specified amount of volume and
##' a limit price.}
##'   \item{changed}{The order has been partially filled. On each modification, 
##' the remaining volume will decrease.}
##'   \item{deleted}{The order may be deleted at the request of the trader or, in
##' the event that the order has been completely filled, deleted by the exchange.
##' An order deleted by the exchange as a result of being filled will have 0
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
##' The trades data.frame contains a log of all executions ordered by local
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
##' The depth data.frame describes the amount of available volume for all price
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
##'   \item{best.ask.price}{The best ask price.}
##'   \item{best.ask.vol}{Amount of volume available at the best ask.}
##'   \item{ask.vol25:500bps}{The amount od volume available for 20 25bps
##' percentiles above the best ask.}
##' }
##' 
##' @family Limit order book data
NULL
