##' Black theme.
##'
##' Default graph look and feel.
##'
##' @return Theme. 
##' @author phil
theme.black <- function() {
  theme_bw() + theme(panel.background=element_rect(fill="#000000"),
                 panel.border=element_rect(size=0),
		 panel.grid.major=element_blank(),
    		 panel.grid.minor=element_blank(),
                 axis.ticks=element_line("gray48", size=0.5),
	 	 plot.background=element_rect(fill="#000000", size=0),
                 text=element_text(colour="#888888"),
		 strip.background=element_rect(fill="#000000", size=0),
	         legend.key=element_rect(fill="#000000", size=0),
		 legend.background=element_rect(fill="#000000", size=0))
}

##' General purpose time series plot.
##'
##' @param timestamp POSIXct timestamps.
##' @param series The time series.
##' @param start.time, end.time, Inclusive start and end time of plot.
##' @param title, y.label Title and Y axis label of the plot.
##' @author phil
##' @examples
##' \dontrun{
##' with(lob.data$trades, plot.time.series(timestamp, price))
##'
##' timestamp <- seq(as.POSIXct("2015-01-01 00:00:00.000", tz="UTC"), 
##'                  as.POSIXct("2015-01-01 00:59:00.000", tz="UTC"), by=60)
##' series <- rep(1:10, 6)
##' plot.time.series(timestamp, series)
##' }
##' @export plot.time.series
plot.time.series <- function(timestamp, series, start.time=min(timestamp),
    end.time=max(timestamp), title="time series", y.label="series") {
  stopifnot(length(timestamp) == length(series))
  logger(paste("plot time series between", start.time, "and", end.time))
  df <- data.frame(ts=timestamp, val=series)
  df <- df[df$ts >= start.time & df$ts <= end.time, ]
  p <- ggplot(data=df, aes(x=ts, y=val))
  ### ggplot ignores timezone, even though explicitly set. work around is:
  p <- p + scale_x_datetime(limits=c(start.time, end.time), 
      labels=function(x) format(x, "%H:%M:%S", tz="UTC"))
  p <- p + scale_y_continuous(labels=function(y) sprintf("%3s", 
      sprintf("%.3s", y)))
  p <- p + ggtitle(title)
  p <- p + geom_line(colour="grey")
  p <- p + xlab("time")
  p <- p + ylab(y.label)
  p + theme.black()
}

##' Plot trades.
##'
##' A convenience function for plotting the trades data.frame in a nice way.
##' 
##' @param trades Trades data.frame
##' @param start.time Plot from
##' @param end.time Plot to
##' @author phil
##' @examples
##' \dontrun{
##' with(lob.data, plot.trades(trades))
##' }
##' @export plot.trades
plot.trades <- function(trades, start.time=min(trades$timestamp),
    end.time=max(trades$timestamp)) {
  ts <- trades[trades$timestamp >= start.time & trades$timestamp <= end.time, ]
  p <- ggplot(data=ts, aes(x=timestamp, y=price))
  p <- p + scale_y_continuous(breaks=seq(round(min(ts$price)), 
      round(max(ts$price)), by=1), name="limit price")
  p <- p + geom_step(data=ts, colour="grey")
  p <- p + xlab("time")
  p + theme.black()
}

##' Plot order book price level heat map. 
##'
##' Produces a visualisation of the limit order book depth through time.
##' The available volume at each price level is colour coded according to the
##' range of volume at all price levels. The colour coding follows the visible
##' spectrum, such that larger amounts of volume appear "hotter" than smaller
##' amounts, where cold = blue, hot = red.
##'
##' Since the distribution of limit order size exponentially decays, it can be
##' difficult to visually differentiate: most values will appear to be blue. The
##' function provides price, volume and a colour bias range to overcome this.
##' 
##' @param depth The order book depth (lob.data$depth).
##' @param spread Spread to overlay (lob.data$depth.summary[, c(1, 2, 64)])
##' @param trades Trades (lob.data$trades).
##' @param show.mp If True, spread will be summarised as midprice.
##' @param show.all.depth If True, show resting (and never hit) limit orders.
##' @param col.bias 1 = uniform colour spectrum. 0.25 = bias toward 0.25
##'                 (more red less blue). <= 0 enables logarithmic scaling.
##' @param start.time Plot depth from this time onward.
##' @param end.time Plot depth up until this time.
##' @param price.from Plot depth with price levels >= this value.
##' @param price.to Plot depth with price levels <= this value.
##' @param volume.from Plot depth with volume >= this value relevant to
##'                    volume.scale
##' @param volume.to Plot depth with volume <= this value relevant to
##'                  volume scale.
##' @param volume.scale Rescale the volume. 0.01 = Cents to Dollars. 
##' @author phil
##' @examples
##' \dontrun{
##' # bid/ask spread.
##' spread <- with(lob.data, depth.summary[, c("timestamp", "best.bid.price",
##'                                            "best.ask.price")])
##'
##' # plot all depth levels, rescaling the volume by 10^-8.
##' # produce 2 plots side-by-side: second plot contains depth levels with > 50
##' # units of volume.
##' p1 <- with(lob.data, plot.price.levels(depth, spread,
##'                                        col.bias=0.1,
##'                                        volume.scale=10^-8))
##' p2 <- with(lob.data, plot.price.levels(depth, spread,
##'                                        col.bias=0.1,
##'                                        volume.scale=10^-8,
##'                                        volume.from=50))
##' library(grid)
##' pushViewport(viewport(layout=grid.layout(1, 2)))
##' print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
##' print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
##'
##' dev.new()
##'
##' # zoom into 1 hour of activity, show the spread and directional trades. 
##' with(lob.data, plot.price.levels(depth, spread, trades,
##'    start.time=as.POSIXct("2015-05-01 14:00:00.000", tz="UTC"),
##'    end.time=as.POSIXct("2015-05-01 15:00:00.000", tz="UTC"),
##'    volume.scale=10^-8))
##'
##' dev.new()
##'
##' # zoom in to 15 minutes of activity, show the bid/ask midprice.
##' with(lob.data, plot.price.levels(depth, spread,
##'    show.mp=F,
##'    start.time=as.POSIXct("2015-05-01 18:00:00.000", tz="UTC"),
##'    end.time=as.POSIXct("2015-05-01 18:15:00.000", tz="UTC")))
##' }
##' @export plot.price.levels
plot.price.levels <- function(depth, spread=NULL, trades=NULL,
    show.mp=T, 
    show.all.depth=F,
    col.bias=0.1,
    start.time=head(depth$timestamp, 1), 
    end.time=tail(depth$timestamp, 1),
    price.from=NULL,
    price.to=NULL, 
    volume.from=NULL,
    volume.to=NULL,
    volume.scale=1) {

  depth$volume <- depth$volume*volume.scale
    
  # filter the spread by start and end time and set price.from, price.to
  # defaults if needed.
  if(!is.null(spread)) {
    spread <- spread[spread$timestamp >= start.time 
                   & spread$timestamp <= end.time, ]
    if(is.null(price.from)) price.from <- 0.995*min(spread$best.bid.price)
    if(is.null(price.to)) price.to <- 1.005*max(spread$best.ask.price)
  }

  # filter trades by start and end time and set price.from, price.to
  # defaults if needed.
  if(!is.null(trades)) {
    trades <- trades[trades$timestamp >= start.time 
                   & trades$timestamp <= end.time, ]

    # set price.from to range below min trade price if not specified.
    # filter trades by specified min price otherwise.
    if(is.null(price.from)) price.from <- 0.995*min(trades$price)
    else trades <- trades[trades$price >= price.from, ]

    # set price.to to be range above max trade price if not specified.
    # filter trades by specified max price otherwise.
    if(is.null(price.to)) price.to <- 1.005*max(trades$price)
    else trades <- trades[trades$price <= price.to, ]
  }

  # filter depth by price and volume  
  if(!is.null(price.from))
    depth <- depth[depth$price >= price.from, ]
  if(!is.null(price.to))
    depth <- depth[depth$price <= price.to, ]
  if(!is.null(volume.from))
    depth <- depth[depth$volume >= volume.from | depth$volume == 0, ]
  if(!is.null(volume.to))
    depth <- depth[depth$volume <= volume.to, ]

  # now filter the depth by time window
  depth.filtered <- filter.depth(depth, start.time, end.time)

  # if requested, remove price levels with no update during time window.
  if(!show.all.depth) {
    unchanged <- tapply(depth.filtered$timestamp, depth.filtered$price, 
        function(v) {
      length(v) == 2 & v[1] == start.time & v[2] == end.time
    })
    unchanged.prices <- unique(depth.filtered$price)
    unchanged.prices <- unchanged.prices[unchanged]
    depth.filtered <- depth.filtered[!depth.filtered$price 
        %in% unchanged.prices, ]
    logger(paste("removed", length(unchanged.prices), "unchanged depths"))
  }
   
  depth.filtered[depth.filtered$volume==0, ]$volume <- NA

  # after filtering, plot.
  plot.price.levels.faster(depth.filtered, spread, trades, show.mp, col.bias)
}

##' Poor man's heatmap.
##'
##' Used by plot.price.levels filtering function. 
##' 
##' An individual order book will consist of hundreds of thousands to millions
##' of updates per day. Plotting a heatmap of order book depth with even a few
##' thousand filtered events with ggplot is excruciatingly slow. This function
##' makes it possible to plot a heat map of all order book updates in a
##' reasonable amount of time. To achieve this, the function plots horizontal
##' colour coded lines for each price level update.
##' 
##' @param depth The order book depth (lob.data$depth).
##' @param spread Spread to overlay (lob.data$depth.summary[, c(1, 2, 64)])
##' @param trades Trades (lob.data$trades).
##' @param show.mp If True, spread will be summarised as midprice.
##' @param show.all.depth If True, show resting (and never hit) limit orders.
##' @param col.bias 1 = uniform colour spectrum. 0.25 = bias toward 0.25
##'                 (more red less blue). <= 0 enables logarithmic scaling.
##' @author phil
plot.price.levels.faster <- function(depth, spread, trades, show.mp=T, 
    col.bias=0.1) {

  buys <- trades[trades$direction == "buy", ]
  sells <- trades[trades$direction == "sell", ]

  log.10 <- F
  if(col.bias <= 0) {
    col.bias <- 1
    log.10 <- T
  }
  col.pal <- colorRampPalette(c("#f92b20", "#fe701b", "#facd1f", "#d6fd1c",
      "#65fe1b", "#1bfe42", "#1cfdb4", "#1fb9fa", "#1e71fb", "#261cfd"), 
      bias=col.bias)(length(unique(depth$volume)))
  col.pal <- rev(col.pal)
  p <- ggplot()
  # set alpha to 0 for na, 0.1 for volume <1, 1 otherwise.
  p <- p + geom_line(data=depth, mapping=aes(colour=volume, x=timestamp, 
      y=price, group=price, alpha=ifelse(is.na(volume), 0, 
      ifelse(volume < 1, 0.1, 1)))) #size=1
  p <- p + scale_y_continuous(breaks=seq(round(min(depth$price)), 
      round(max(depth$price)), by=0.5), name="limit price")
  if(log.10)
    p <- p + scale_colour_gradientn(colours=col.pal, trans="log10", 
      na.value="black")
  else {
    quantiles <- quantile(depth$volume, probs=seq(0.5, 1, 0.5), na.rm=T)
    p <- p + scale_colour_gradientn(colours=col.pal, na.value="black", 
        name="volume        \n", breaks=as.vector(quantiles), 
        labels=sprintf("%7s", sprintf("%.7s", quantiles)))
  }
  #remove alpha legend.
  p <- p + scale_alpha_continuous(range=c(0, 1), guide="none")

  # plot midprice or spread.
  if(!is.null(spread)) {
    if(show.mp) {
      p <- p + geom_line(data=spread, aes(x=timestamp, 
        y=(best.bid.price+best.ask.price)/2), col="#ffffff", size=1.1)
    } else {
      p <- p + geom_step(data=spread, aes(x=timestamp, y=best.ask.price), 
          col="#ff0000", size=1.5)
      p <- p + geom_step(data=spread, aes(x=timestamp, y=best.bid.price), 
          col="#00ff00", size=1.5)
    }
  }

  if(!is.null(trades)) {
    p <- p + geom_point(data=sells, aes(x=timestamp, y=price), colour="#ffffff",
        size=6, shape=1)
    p <- p + geom_point(data=sells, aes(x=timestamp, y=price), colour="#ff0000",
        size=5, shape=1)
    p <- p + geom_point(data=sells, aes(x=timestamp, y=price), colour="#ffffff",
        size=4, shape=1)
    p <- p + geom_point(data=buys, aes(x=timestamp, y=price), colour="#ffffff", 
        size=6, shape=1)
    p <- p + geom_point(data=buys, aes(x=timestamp, y=price), colour="#00ff00", 
        size=5, shape=1)
    p <- p + geom_point(data=buys, aes(x=timestamp, y=price), colour="#00ff00", 
        size=5, shape=1)
  }
  p <- p + theme(legend.title=element_text(hjust=3, vjust=20))
  p <- p + xlab("time")
  p + theme.black()
}

##' Plot limit order event map.
##'
##' Generates a visualisation of limit order events (excluding market and market
##' limit orders). Ask side orders = red, Bid side orders = blue. Volume of
##' order determines size of circle. Opaque = volume was added, transparent =
##' volume was removed.
##' 
##' @param events Limit order events data.frame.
##' @param start.time Plot events from this time onward.
##' @param end.time Plot events up until this time.
##' @param price.from Plot events with price levels >= this value.
##' @param price.to Plot events with price levels <= this value.
##' @param volume.from Plot events with volume >= this value relevant to
##'                    volume.scale
##' @param volume.to Plot events with volume <= this value relevant to
##'                  volume scale.
##' @param volume.scale Rescale the volume. 0.01 = Cents to Dollars. 
##' @author phil
##' @examples
##' \dontrun{
##'
##' # plot all orders 
##' with(lob.data, plot.quote.map(events))
##'
##' # 1 hour of activity and re-scale the volume
##' with(lob.data, plot.quote.map(events,
##'     start.time=as.POSIXct("2015-05-01 14:00:00.000", tz="UTC"),
##'     end.time=as.POSIXct("2015-05-01 15:00:00.000", tz="UTC"),
##'     volume.scale=10^-8))
##' # 15 minutes of activity >= 5 (re-scaled) volume within price range
##' # $ [220, 245]
##' with(lob.data, plot.quote.map(events,
##'     start.time=as.POSIXct("2015-05-01 08:00:00.000", tz="UTC"),
##'     end.time=as.POSIXct("2015-05-01 08:15:00.000", tz="UTC"),
##'     price.from=220,
##'     price.to=245,
##'     volume.from=5,
##'     volume.scale=10^-8))
##'
##' }
##' @export plot.event.map
plot.event.map <- function(events,
    start.time=min(events$timestamp), 
    end.time=max(events$timestamp),
    price.from=NULL,
    price.to=NULL,
    volume.from=NULL,
    volume.to=NULL,
    volume.scale=1) {

  # interested in added, and then subsequently cancelled or resting limit orders
  events <- events[events$timestamp >= start.time & events$timestamp <= end.time
      & (events$type == "flashed-limit" | events$type == "resting-limit"), ]

  events$volume <- events$volume*volume.scale

  # filter by specified volume
  # todo: price, volume, time filtering is very common -move this elsewhere.    
  if(!is.null(volume.from))
    events <- events[events$volume >= volume.from, ]
  if(!is.null(volume.to))
    events <- events[events$volume <= volume.to, ]

  # if price range has not been specified, set it to contain 99% of data
  # (to ignore outlying limit orders).
  if(is.null(price.from))
    price.from <- as.numeric(quantile(events$price, 0.01))
  if(is.null(price.to))
    price.to <- as.numeric(quantile(events$price, 0.99))

  events <- events[events$price >= price.from & events$price <= price.to, ]  
    
  created <- events[events$action == "created", ]
  deleted <- events[events$action == "deleted", ]
  col.pal <- c("#0000ff", "#ff0000")
  names(col.pal) <- c("bid", "ask")
    
  p <- ggplot(data=events, mapping=aes(x=timestamp, y=price))
  p <- p + scale_y_continuous(breaks=seq(round(min(events$price)), 
      round(max(events$price)), by=0.5), name="limit price")
  p <- p + geom_point(data=created, mapping=aes(size=volume), colour="#333333",
      shape=19)
  p <- p + geom_point(data=deleted, mapping=aes(size=volume), colour="#333333", 
      shape=1)
  p <- p + scale_size_continuous(name="volume        \n") 
  p <- p + geom_point(data=events, mapping=aes(colour=direction), size=0.1)
  p <- p + scale_colour_manual(values=col.pal, guide="none")
  p <- p + xlab("time")

  p + theme.black()
}

##' Visualise flashed-limit order volume.
##'
##' Plots the points at which volume was added or removed from the limit order
##' book. A flashed limit-order is a "fleeting" limit order: an order was added,
##' then removed (usually within a very short period of time). This plot is
##' especially useful for identifying individual trading algorithms by price and
##' volume.
##' 
##' @param events Limit order events data.frame.
##' @param action "deleted" for cancelled volume, "added" for added volume.
##' @param start.time Plot events from this time onward.
##' @param end.time Plot events up until this time.
##' @param price.from Plot events with price levels >= this value.
##' @param price.to Plot events with price levels <= this value.
##' @param volume.from Plot events with volume >= this value relevant to
##'                    volume.scale
##' @param volume.to Plot events with volume <= this value relevant to
##'                  volume scale.
##' @param volume.scale Rescale the volume. 0.01 = Cents to Dollars. 
##' @param log.scale If true, plot volume on logarithmic scale.
##' @author phil
##' @examples
##' \dontrun{
##'
##' # plot all fleeting limit order volume using logarithmic scale.
##' with(lob.data, plot.volume.map(events, volume.scale=10^-8, log.scale=T))
##'
##' # plot fleeting limit order volume within 1 hour range up until 10 units of
##' # volume.
##' with(lob.data, plot.volume.map(events, volume.scale=10^-8,
##'     start.time=as.POSIXct("2015-05-01 09:30:00.000", tz="UTC"),
##'     end.time=as.POSIXct("2015-05-01 10:30:00.000", tz="UTC"),
##'     volume.to=10))
##'
##' }
##' @export plot.volume.map
plot.volume.map <- function(events,
    action="deleted", 
    start.time=min(events$timestamp),
    end.time=max(events$timestamp),
    price.from=NULL,
    price.to=NULL,
    volume.from=NULL,
    volume.to=NULL,
    volume.scale=1,
    log.scale=F) {

  stopifnot(action == "deleted" || action == "created")
    
  events$volume <- events$volume*volume.scale

  # interested in flashed-limit (fleeting orders) within time range.  
  events <- events[events$action == action & events$type == "flashed-limit"
      & events$timestamp >= start.time & events$timestamp <= end.time, ]

  # filter events by price and volume. if min,max volume is not set, set it to
  # 99.99% quantile range to avoid plotting outlyers.
  if(!is.null(price.from))
    events <- events[events$price >= price.from, ]
  if(!is.null(price.to))
    events <- events[events$price <= price.to, ]
  if(!is.null(volume.from))
    events <- events[events$volume >= volume.from | events$volume == 0, ]
  else {
    lim <- quantile(events$volume, 0.0001)
    logger(paste("lower volume limit =", lim))
    events <- events[events$volume >= lim, ]
  }
  if(!is.null(volume.to))
    events <- events[events$volume <= volume.to, ]
  else {
    lim <- quantile(events$volume, 0.9999)
    logger(paste("uppper volume limit =", lim))
    events <- events[events$volume <= lim, ]
  }

  vol.scale <- if(log.scale) "log" else "identity" 
    
  col.pal <- c("#0000ff", "#ff0000")
  names(col.pal) <- c("bid", "ask")
  p <- ggplot(data=events, mapping=aes(x=timestamp, y=volume))
  p <- p + geom_point(mapping=aes(colour=direction), size=1, shape=15)
  p <- p + scale_colour_manual(values=col.pal, name="direction     \n")
  p <- p + scale_y_continuous(name="cancelled volume",
      labels=function(y) sprintf("%5s", sprintf("%.2f", y)),
      trans=vol.scale)
  p <- p + xlab("time")

  p + theme.black()
}

##' Visualise order book depth at any given point in time.
##'
##' Plots the cumalative volume on each side of the limit order book.
##' 
##' @param order.book A limit order book structure.
##' @param volume.scale Rescale the volume. 0.01 = Cents to Dollars. 
##' @param show.quantiles If true, highlight top 1% highest volume.
##' @param show.volume  If true, also show non-cumulative volume.
##' @author phil
##' @examples
##' \dontrun{
##'
##' # get a limit order book for a specific point in time, limited to +- 150bps
##' # above/below best bid/ask price.
##' lob <- order.book(lob.data$events,
##'     tp=as.POSIXct("2015-05-01 09:38:17.429", tz="UTC"), bps.range=150)
##'
##' # visualise the order book liquidity.
##' plot.current.depth(lob, volume.scale=10^-8)
##' 
##' }
##' @export plot.current.depth
plot.current.depth <- function(order.book,
    volume.scale=1,
    show.quantiles=T,
    show.volume=T) {

  # order data.
  bids <- reverse.matrix(order.book$bids)
  asks <- reverse.matrix(order.book$asks)

  # combine both sides into single series.  
  x <- c(bids$price, tail(bids$price, 1), head(asks$price, 1), asks$price)
  y1 <- c(bids$liquidity, 0, 0, asks$liquidity) * volume.scale
  y2 <- c(bids$volume, 0, 0, asks$volume) * volume.scale
  col.pal <- c("#ff0000", "#0000ff")
  side <- c(rep("bid", nrow(bids)+1), rep("ask", nrow(asks)+1))

  # "melt" data into single data.frame.
  depth <- data.frame(price=x, liquidity=y1, volume=y2, side=side)
  p <- ggplot(depth, aes(x=price, y=liquidity, group=side, colour=side))
  p <- p + scale_x_continuous(breaks=seq(round(min(bids$price)), 
      round(max(asks$price)), by=1))
  p <- p + scale_colour_manual(values=col.pal)  

  # plot liquidity (cumulative sum of volume)
  p <- p + geom_step()

  # plot volume
  if(show.volume)
    p <- p + geom_bar(stat="identity", mapping=aes(y=volume), colour="#555555")

  # highlight highest 1% volume with vertical lines
  if(show.quantiles) {
    bid.quantiles <- with(bids, price[volume >= quantile(volume, 0.99)])
    ask.quantiles <- with(asks, price[volume >= quantile(volume, 0.99)])

    logger(paste("bid quantiles =", paste(bid.quantiles, collapse=", "),
                 "ask quantiles =", paste(ask.quantiles, collapse=", ")))

    p <- p + geom_vline(xintercept=bid.quantiles, colour="#222222")
    p <- p + geom_vline(xintercept=ask.quantiles, colour="#222222")
  }
    
  p <- p + ggtitle(as.POSIXct(order.book$timestamp, origin="1970-01-01",
                              tz="UTC"))

  p + theme.black()
}

##' Visualise available limit order book liquidity through time.
##'
##' Plots the available volume in 25bps increments on each side of the order
##' book in the form of a stacked area graph. The top of the graph depicts the
##' ask side of the book, whilst the bottom depicts the bid side. Percentiles
##' and order book sides can be separated by an optional subtle line for
##' improved legibility.
##' 
##' @param depth.summary Depth summary data (lob.data$depth.summary).
##' @param start.time Plot events from this time onward.
##' @param end.time Plot events up until this time.
##' @param volume.scale Rescale the volume. 0.01 = Cents to Dollars.
##' @param percentile.line If true, separate percentiles with subtle line.
##' @param side.line If true, separate bid/ask side with subtle line.
##' @author phil
##' @examples
##' \dontrun{
##'
##' # visualise 2 hours of order book liquidity.
##' # data will be aggregated to minute-by-minute resolution.
##' plot.volume.percentiles(lob.data$depth.summary,
##'     start.time=as.POSIXct("2015-05-01 11:00:00.000", tz="UTC"),
##'     end.time=as.POSIXct("2015-05-01 13:00:00.000", tz="UTC"),
##'     volume.scale=10^-8)
##'
##' # visualise 15 minutes of order book liquidity.
##' # data will be aggregated to second-by-second resolution.
##' plot.percentiles(lob.data$depth.summary,
##'     start.time=as.POSIXct("2015-05-01 10:45:00.000", tz="UTC"),
##'     end.time=as.POSIXct("2015-05-01 11:00:00.000", tz="UTC"),
##'     volume.scale=10^-8)
##' }
##' @export plot.volume.percentiles
plot.volume.percentiles <- function(depth.summary, 
    start.time=head(depth.summary$timestamp, 1),
    end.time=tail(depth.summary$timestamp, 1),
    volume.scale=1,
    percentile.line=T,
    side.line=T) {     
        
  logger(paste("plot depth percentiles between", start.time, "and", end.time))

  bid.names <- paste0("bid.vol", seq(from=25, to=500, by=25), "bps")
  ask.names <- paste0("ask.vol", seq(from=25, to=500, by=25), "bps")

  td <- difftime(end.time, start.time, units="secs")
  logger(paste("time range =", td, "secs"))
  td <- round(as.numeric(td))

  # resolution: if(td > 15 minutes, minute ticks, else seconds. 
  frequency <- ifelse(td > 900, "mins", "secs")
  ob.percentiles <- depth.summary[depth.summary$timestamp 
      >= start.time-ifelse(frequency == "mins", 60, 1) & depth.summary$timestamp
      <= end.time, c("timestamp", bid.names, ask.names)]
  logger(paste("aggregating to", frequency, "intervals"))

  # remove duplicates (take last entry) (for zoo to work)
  ob.percentiles <- ob.percentiles[!duplicated(ob.percentiles$timestamp, 
      fromLast=T), ] 

  # convert to zoo object
  zoo.obj <- to.zoo(ob.percentiles)

  # intervals truncated to frequency
  intervals <- as.POSIXct(trunc(time(zoo.obj), frequency))
  logger(paste("aggregation:", min(intervals), ":", max(intervals), "by =", 
      frequency))

  # use zoo to aggregate by intervals. take mean of each interval.
  aggregated <- aggregate(zoo.obj, intervals, mean)
  ob.percentiles <- data.frame(timestamp=unique(intervals)+ifelse(frequency == 
      "mins", 60, 1), aggregated, row.names=NULL)

  bid.names <- paste0("bid.vol", sprintf("%03d", seq(from=25, to=500, 
      by=25)), "bps")
  ask.names <- paste0("ask.vol", sprintf("%03d", seq(from=25, to=500, 
      by=25)), "bps")
  colnames(ob.percentiles) <- c("timestamp", bid.names, ask.names) 

  max.ask <- max(rowSums(ob.percentiles[, 22:41]))
  max.bid <- max(rowSums(ob.percentiles[, 2:21]))

  # use reshape2 to flatten ob.percentiles into single data.frame.  
  melted.asks <- melt(ob.percentiles, id.vars="timestamp", 
      measure.vars=ask.names, variable.name="percentile", 
      value.name="liquidity")
  melted.asks$percentile <- factor(melted.asks$percentile, rev(ask.names))
  melted.asks$liquidity <- volume.scale*(melted.asks$liquidity)
  melted.bids <- melt(ob.percentiles, id.vars="timestamp", 
      measure.vars=bid.names, variable.name="percentile", 
      value.name="liquidity")
  melted.bids$percentile <- factor(melted.bids$percentile, bid.names)
  melted.bids$liquidity <- volume.scale*(melted.bids$liquidity)

  col.pal <- colorRampPalette(c("#f92b20", "#fe701b", "#facd1f", "#d6fd1c", 
      "#65fe1b", "#1bfe42", "#1cfdb4", "#1fb9fa", "#1e71fb", "#261cfd"))(20)
  col.pal <- c(col.pal, col.pal)
  breaks <- c(rev(paste0("ask.vol", sprintf("%03d", seq(from=50, to=500, 
      by=50)), "bps")), paste0("bid.vol", sprintf("%03d", seq(from=50, 
      to=500, by=50)), "bps"))
  legend.names <- c(rev(paste0("+", sprintf("%03d", seq(from=50, to=500, 
      by=50)), "bps")), paste0("-", sprintf("%03d", seq(from=50, to=500, 
      by=50)), "bps"))

  # top stack (asks)  
  p <- ggplot(data=melted.asks, mapping=aes(x=timestamp, y=liquidity, 
      fill=percentile))
  p <- p + geom_area(position="stack")

  # bottom stack (bids)
  p <- p + geom_area(data=melted.bids, aes(x=timestamp, y=-liquidity, 
      fill=percentile), position="stack")
    
  # seperate percentiles by black line    
  if(percentile.line) {
    p <- p + geom_line(mapping=aes(ymax=0), position="stack", col="#000000",
        size=0.1)
    p <- p + geom_line(data=melted.bids, aes(x=timestamp, y=-liquidity, ymax=0), 
        position="stack", col="#000000", size=0.1)
  }

  # colour  
  p <- p + scale_fill_manual(values=col.pal, breaks=breaks, labels=legend.names,
      name="depth         \n")

  # seperate bid ask sides by black line  
  if(side.line)
    p <- p + geom_hline(yintercept=0, col="#000000", size=0.1)

  # limit the volume range    
  y.range <- volume.scale*(max(max.ask, max.bid))
  p <- p + ylim(-y.range, y.range)

  p + xlab("time") + theme.black()
}

# val = volume | price

#' @export plot.histogram
plot.histogram <- function(events,
    start.time=head(events$timestamp, 1),
    end.time=tail(events$timestamp, 1),
    val="") {
    
  events <- events[events$timestamp >= start.time 
                 & events$timestamp <= end.time, ] 
  td <- difftime(end.time, start.time, units="secs")
  td <- round(as.numeric(td)) 
  if(val=="volume") {
    if(td > 10800) bw <- 10
    else bw <- 1
  }
  if(val=="price") {
    if(td > 10800) bw <- 5
    else bw <- 0.25
  }
  p <- ggplot(data=events, mapping=aes(x=val, fill=direction,
      colour=direction))
  p <- p + geom_bar(binwidth=bw, position="dodge")
  p <- p + scale_colour_manual(values=c("#0000ff", "#ff0000"))
  p <- p + scale_fill_manual(values=c("#0000ff", "#ff0000"))
  p <- p + ggtitle(paste("events", val, "distribution"))
  p + theme.black()
}

# x=volume vs event count. 

#' @export plot.volume.histogram
plot.volume.histogram <- function(...) plot.histogram(..., val="volume")

#' @export plot.price.histogram
plot.price.histogram  <- function(...) plot.histogram(..., val="price")

