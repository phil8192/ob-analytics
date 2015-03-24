library(ggplot2)
library(zoo)

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

#' General time series plot. 
#'
#' For general time series plotting.
#' 
#' @param timestamp POSIXct timestamps.
#' @param series The time series.
#' @param start.time, end.time, Inclusive start and end time of plot.
#' @param title, y.label Title and Y axis label of the plot.
#' @export
#' @examples
#' \donotrun{
#' p <- plot.time.series(timestamp=x$trades$timestamp, series=x$trades$price)
#' p
#'
#' timestamp <- seq(as.POSIXct("2015-01-01 00:00:00.000", tz="UTC"), 
#'                  as.POSIXct("2015-01-01 00:59:00.000", tz="UTC"), by=60)
#' series <- rep(1:10, 6)
#' p <- plot.time.series(timestamp, series)
#' p
#' }
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

#' Plot trades. 
#'
#' Plots executed prices in trades data.frame, where data.frame contains 
#' timestamp and price columns.
#' 
#' @param timestamp POSIXct timestamps.
#' @param start.time, end.time, Inclusive start and end time of plot.
#' @export
#' @examples
#' \donotrun{
#' x <- load.data("order.book.data.Rda")
#' p <- plot.trades(x$trades)
#' p
#' }
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

# poor mans heatmap
# spread.filtered <- depth.summary[, c("timestamp", "best.bid.price", "best.ask.price")]
# spread.filtered[, 2:3] <- spread.filtered[, 2:3]*0.01
#depth.filtered <- depth[depth$price >= min(trades$price)-5
#                      & depth$price <= max(trades$price)+5, ]

plot.price.levels <- function(depth, depth.summary, trades, show.mp=F, 
    show.all.depth=T, col.bias=0.1, start.time=head(depth$timestamp, 1), 
    end.time=tail(depth$timestamp, 1), price.from=NULL, price.to=NULL, 
    volume.from=NULL, volume.to=NULL) {

  trades.filtered <- trades[trades$timestamp >= from.time 
                          & trades$timestamp <= end.time, ]
  spread.filtered <- depth.summary[depth.summary$timestamp >= from.time 
                                 & depth.summary$timestamp <= end.time, 
      c("timestamp", "best.bid.price", "best.ask.price")]

  if(!(is.null(price.from) || is.null(price.to) 
      || is.null(volume.from) || is.null(volume.to))) {
    trades.filtered <- trades.filtered[trades.filtered$price  >= price.from
                                     & trades.filtered$price  <= price.to 
                                     & trades.filtered$volume >= volume.from
                                     & trades.fitlered$volume <= volume.to, ]
    depth.filtered <- depth[depth$price  >= price.from
                         &  depth$price  <= price.to
                         & (depth$volume >= volume.from | depth$volume == 0)
                         &  depth$volume <= volume.to, ]
  } else {
    depth.filtered <- depth[depth$price >= min(trades$price)*0.99
                          & depth$price <= max(trades$price)*1.01, ]   
  }

  depth.filtered <- filter.depth(depth.filtered, start.time, end.time)

  # remove price levels with no update during time window.
  if(!show.all.depth) {
    unchanged <- tapply(depth.filtered$timestamp, depth.filtered$price, 
        function(v) {
      length(v) == 2 & v[1] == start.time & v[2] == end.time
    })
    unchanged.prices <- unique(depth.filtered$price)
    unchanged.price <- unchanged.prices[unchanged]
    depth.filtered <- depth.filtered[!depth.filtered$price 
        %in% unchanged.prices, ]
    logger(paste("removed", length(unchanged.prices), "unchanged depths"))
  }

  depth.filtered[depth.filtered$volume==0, ]$volume <- NA
  
  plot.price.levels.faster(depth.filtered, spread.filtered, trades.filtered, 
      show.mp, col.bias) 
}

plot.price.levels.faster <- function(depth, spread, trades, show.mp=F, 
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
  quantiles <- quantile(depth$volume, probs=seq(0.5, 1, 0.5), na.rm=T)
  logger("price level quantiles:")
  logger(quantiles)
  p <- ggplot()
  if(show.mp & !is.null(spread)) {
    p <- p + geom_line(data=spread, aes(x=timestamp, 
        y=(best.bid.price+best.ask.price)/2), col="#ffffff", size=1.1)
  }
  # set alpha to 0 for na, 0.1 for volume <1, 1 otherwise.
  p <- p + geom_line(data=depth, mapping=aes(colour=volume, x=timestamp, 
      y=price, group=price, alpha=ifelse(is.na(volume), 0, 
      ifelse(volume < 1, 0.1, 1)))) #size=1
  p <- p + scale_y_continuous(breaks=seq(round(min(depth$price)), 
      round(max(depth$price)), by=0.5), name="limit price")
  if(log.10)
    p <- p + scale_colour_gradientn(colours=col.pal, trans="log10", 
      na.value="black")
  else
    p <- p + scale_colour_gradientn(colours=col.pal, na.value="black", 
        name="volume        \n", breaks=as.vector(quantiles), 
        labels=sprintf("%7s", sprintf("%.7s", quantiles)))
  #remove alpha legend.
  p <- p + scale_alpha_continuous(range=c(0, 1), guide="none")

  if(!is.null(spread)) {
    p <- p + geom_step(data=spread, aes(x=timestamp, y=best.ask.price), 
        col="#ff0000", size=1.5)
    p <- p + geom_step(data=spread, aes(x=timestamp, y=best.bid.price), 
        col="#00ff00", size=1.5)
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
  p <- p + theme.black()
  p <- p + theme(legend.title=element_text(hjust=3, vjust=20))
  p + xlab("time")
}

# quote map (shows point in time where an order was added or deleted)
# good for seeing algo patterns and quote stuffers.
plot.quote.map <- function(events, start.time=head(events$timestamp, 1), 
    end.time=tail(events$timestamp, 1)) {
  events <- events[events$timestamp >= start.time & events$timestamp <= end.time
      & (events$type == "flashed-limit" | events$type == "flashed-limit"), ]
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
  p <- p + theme.black()
  p + xlab("time")
}

# cancellation map (by volume)
# good for showing quote stuffing and for algo identification.
# action = deleted | created
plot.volume.map <- function(events, action, 
    start.time=head(events$timestamp, 1), end.time=tail(events$timestamp, 1)) {
  filtered <- events[events$action == action 
      & events$type == "flashed-limit"
      & events$timestamp >= start.time & events$timestamp <= end.time, ]
  col.pal <- c("#0000ff", "#ff0000")
  names(col.pal) <- c("bid", "ask")
  p <- ggplot(data=filtered, mapping=aes(x=timestamp, y=volume))
  p <- p + geom_point(mapping=aes(colour=direction), size=1, shape=15)
  p <- p + scale_colour_manual(values=col.pal, name="direction     \n")
  p <- p + scale_y_continuous(name="cancelled volume", labels=function(y) 
      sprintf("%5s", y))
  p <- p + xlab("time")
  p + theme.black()
}

# order book cumulative volume at given point in time
plot.current.depth <- function(order.book, ascii=F) {
  bids <- reverse.matrix(order.book$bids)
  asks <- reverse.matrix(order.book$asks)
  x <- c(bids$price, tail(bids$price, 1), head(asks$price, 1), asks$price)
  y <- c(bids$liquidity, 0, 0, asks$liquidity)
  col.pal <- c("#ff0000", "#0000ff")
  side <- c(rep("bid", nrow(bids)+1), rep("ask", nrow(asks)+1))
  depth <- data.frame(price=x, liquidity=y, side=side)
  p <- ggplot(depth, aes(x=price, y=liquidity, group=side, colour=side))
  p <- p + scale_x_continuous(breaks=seq(round(min(bids$price)), 
      round(max(asks$price)), by=1))
  p <- p + scale_colour_manual(values=col.pal)  
  p <- p + geom_step()
  p <- p + ggtitle(as.POSIXct(order.book$timestamp, origin="1970-01-01", tz="UTC"))
  p + theme.black()
}

# pct.type = vol | gap
plot.percentiles <- function(pct.type, depth.summary, 
    start.time=head(depth.summary$timestamp, 1),
    end.time=tail(depth.summary$timestamp, 1), 
    transform=function(x) x) {
  logger(paste("plot depth percentiles between", start.time, "and", end.time))
  library(reshape2)
  bid.names <- paste0("bid.", pct.type, seq(from=25, to=500, by=25), "bps")
  ask.names <- paste0("ask.", pct.type, seq(from=25, to=500, by=25), "bps")

  td <- difftime(end.time, start.time, units="secs")
  logger(td)
  td <- round(as.numeric(td))

  # if(td > 15 minutes, minute ticks, else seconds. 
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

  # aggregate by intervals
  aggregated <- aggregate(zoo.obj, intervals, mean)
  ob.percentiles <- data.frame(timestamp=unique(intervals)+ifelse(frequency == 
      "mins", 60, 1), aggregated, row.names=NULL)

  bid.names <- paste0("bid.", pct.type, sprintf("%03d", seq(from=25, to=500, 
      by=25)), "bps")
  ask.names <- paste0("ask.", pct.type, sprintf("%03d", seq(from=25, to=500, 
      by=25)), "bps")
  colnames(ob.percentiles) <- c("timestamp", bid.names, ask.names) 
  max.ask <- max(rowSums(ob.percentiles[, 22:41]))
  max.bid <- max(rowSums(ob.percentiles[, 2:21]))

  # centre
  y.range <- transform(max(max.ask, max.bid)) # centre

  melted.asks <- melt(ob.percentiles, id.vars="timestamp", 
      measure.vars=ask.names, variable.name="percentile", 
      value.name="liquidity")
  melted.asks$percentile <- factor(melted.asks$percentile, rev(ask.names))
  melted.asks$liquidity <- transform(melted.asks$liquidity)
  melted.bids <- melt(ob.percentiles, id.vars="timestamp", 
      measure.vars=bid.names, variable.name="percentile", 
      value.name="liquidity")
  melted.bids$percentile <- factor(melted.bids$percentile, bid.names)
  melted.bids$liquidity <- transform(melted.bids$liquidity)
  col.pal <- colorRampPalette(c("#f92b20", "#fe701b", "#facd1f", "#d6fd1c", 
      "#65fe1b", "#1bfe42", "#1cfdb4", "#1fb9fa", "#1e71fb", "#261cfd"))(20)
  col.pal <- c(col.pal, col.pal)
  breaks <- c(rev(paste0("ask.", pct.type, sprintf("%03d", seq(from=50, to=500, 
      by=50)), "bps")), paste0("bid.", pct.type, sprintf("%03d", seq(from=50, 
      to=500, by=50)), "bps"))
  legend.names <- c(rev(paste0("+", sprintf("%03d", seq(from=50, to=500, 
      by=50)), "bps")), paste0("-", sprintf("%03d", seq(from=50, to=500, 
      by=50)), "bps"))
  logger("creating plot..")                  
  p <- ggplot(data=melted.asks, mapping=aes(x=timestamp, y=liquidity, 
      fill=percentile))
  p <- p + geom_area(position="stack")
  p <- p + geom_line(mapping=aes(ymax=0), position="stack", col="black", 
      size=0.25)
  p <- p + geom_area(data=melted.bids, aes(x=timestamp, y=-liquidity, 
      fill=percentile), position="stack")
  p <- p + geom_line(data=melted.bids, aes(x=timestamp, y=-liquidity, ymax=0), 
      position="stack", col="black", size=0.25)
  p <- p + scale_fill_manual(values=col.pal, breaks=breaks, labels=legend.names, 
      name="depth         \n")
  p <- p + ylim(-y.range, y.range)
  p <- p + xlab("time")
  p + theme.black()
}

# x=price vs volume histogram. volume could be traded volume, cancelled volume, cancellations, addded volume, volume by order type etc.
# here it is just the count of order events at each price level to give an idea of activity.
plot.price.histogram <- function(events, 
    start.time=head(events$timestamp, 1),
    end.time=tail(events$timestamp, 1)) {

  events <- events[events$timestamp >= start.time & events$timestamp 
      <= end.time, ] 

  td <- difftime(end.time, start.time, units="secs")
  td <- round(as.numeric(td))

  bw=1
 
  if(td > 10800)
    bw=10

  p <- ggplot(data=events, mapping=aes(x=price, fill=direction, 
      colour=direction))
  p <- p + geom_bar(position="dodge", binwidth=bw)
  p <- p + scale_colour_manual(values=c("#ff0000", "#0000ff"))
  p <- p + scale_fill_manual(values=c("#ff0000", "#0000ff"))
  p <- p + ggtitle("events price distribution")  
  p + theme.black()
}



############ <<<<
# val = volume | price
plot.histogram <- function(events, val="volume",
    start.time=head(events$timestamp, 1),
    end.time=tail(events$timestamp, 1)) {
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
plot.volume.histogram <- function(events,
    start.time=head(events$timestamp, 1),
    end.time=tail(events$timestamp, 1)) {
  events <- events[events$timestamp >= start.time & events$timestamp 
      <= end.time, ]

  td <- difftime(end.time, start.time, units="secs")
  td <- round(as.numeric(td))

  bw=0.25

  if(td > 10800)
    bw=5

  p <- ggplot(data=events, mapping=aes(x=volume, fill=direction, 
      colour=direction))
  p <- p + geom_bar(binwidth=bw, position="dodge")
  p <- p + scale_colour_manual(values=c("#0000ff", "#ff0000"))
  p <- p + scale_fill_manual(values=c("#0000ff", "#ff0000"))
  p <- p + ggtitle("events volume distribution")
  p + theme.black()
}

