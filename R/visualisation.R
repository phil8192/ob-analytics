library(txtplot)
library(ggplot2)
library(ggthemes)
library(grid) # required for ggplot theme
library(zoo)


default.start.time <- as.POSIXct("1970-01-01 00:00:00.000", tz="UTC")
default.end.time <- as.POSIXct("2070-12-12 23:59:59.999", tz="UTC")

theme.black <- function() {
  p <- theme_bw()
  p <- p + theme(panel.background=element_rect(fill="#000000"),
                 panel.border=element_rect(size=0),
		 panel.grid.major=element_blank(),
    		 panel.grid.minor=element_blank(),
                 axis.ticks=element_line("gray48", size=0.5),
	 	 plot.background=element_rect(fill="#000000", size=0),
                 text=element_text(colour="#888888"),
		 strip.background=element_rect(fill="#000000", size=0),
	         legend.key=element_rect(fill="#000000", size=0),
		 legend.background=element_rect(fill="#000000", size=0))
  p
}

# general
plot.time.series <- function(ts, start.time=min(ts$timestamp), end.time=max(ts$timestamp), title="time series") {
  print(paste("plot time series between", start.time, "and", end.time))
  ts <- ts[ts$timestamp >= start.time & ts$timestamp <= end.time, ]
  p <- ggplot(data=ts, aes(x=timestamp, y=val))
  ### ggplot ignores timezone, even though explicitly set. work around is
  ### to specify a format function. http://stackoverflow.com/questions/10999249/how-to-adjust-x-axis-in-ggplots-density-plot
  p <- p + scale_x_datetime(limits=c(start.time, end.time), labels=function(x) format(x, "%H:%M:%S", tz="UTC"))
  p <- p + scale_y_continuous(labels=function(y) sprintf("%3s", sprintf("%.3s", y)))
  p <- p + ggtitle(title)
  p <- p + geom_line(colour="grey")
  p <- p + xlab("time")
  p + theme.black()
}

# multiple time series
matplot.time.series <- function(ts, start.time=min(ts$timestamp), end.time=max(ts$timestamp), title="time series") {
  ts <- ts[ts$timestamp >= start.time & ts$timestamp <= end.time, ]
  ts <- flatten.matrix(ts)
  p <- ggplot(data=ts, aes(x=timestamp, y=val, group=label, colour=label))
  p <- p + geom_step()
  p + theme.black()
}

plot.time.and.sales <- function(ts, depth.summary, start.time=min(ts$timestamp), end.time=max(ts$timestamp), 
    title=paste("trades between", start.time, "and", end.time)) {
  ts <- ts[ts$timestamp >= start.time & ts$timestamp <= end.time, ]
  p <- ggplot(data=ts, aes(x=timestamp, y=price))
  p <- p + scale_y_continuous(breaks=seq(round(min(ts$price)), round(max(ts$price)), by=1), name="limit price")
  p <- p + geom_step(data=ts, colour="grey")
  p <- p + xlab("time")
  p <- p + theme.black()
}

plot.price.level.depth <- function(ts, price.level.volume, 
    start.time=default.start.time, end.time=default.end.time) {
  ts <- ts[ts$timestamp >= start.time & ts$timestamp <= end.time, ]
  min.price  <- min(ts$price)-1
  max.price  <- max(ts$price)+1
  price.level.volume <- depth.levels(price.level.volume[price.level.volume$price >= min.price
      & price.level.volume$price <= max.price
      & price.level.volume$timestamp <= end.time, ], from=start.time)
  plot.price.level.depth.preprocessed(ts, price.level.volume)
}

plot.price.level.depth.preprocessed <- function(ts, price.level.volume) {
  col.pal <- colorRampPalette(c("#f92b20", "#fe701b", "#facd1f", "#d6fd1c",
      "#65fe1b", "#1bfe42", "#1cfdb4", "#1fb9fa", "#1e71fb", "#261cfd"),
      bias=0.05)(length(unique(price.level.volume$volume)))
  col.pal <- rev(col.pal)
  p <- ggplot(price.level.volume, aes(x=timestamp, y=price, fill=volume))
  p <- p + scale_y_continuous(breaks=seq(min(price.level.volume$price),
      max(price.level.volume$price), by=1))
  p <- p + geom_tile()
  p <- p + scale_fill_gradientn(colours = col.pal)
  p <- p + geom_step(data=ts, aes(x=timestamp, y=price), col="black")
  p <- p + geom_point(data=ts, aes(x=timestamp, y=price, size=6+log10(volume)), colour="white")
  p <- p + geom_point(data=ts, aes(x=timestamp, y=price, colour=direction, size=4+log10(volume)))  
  p <- p + scale_colour_manual(name="direction", values=c("#0000ff", "#ff0000"))
  p
}

# poor mans heatmap
# spread.filtered <- depth.summary[, c("timestamp", "best.bid.price", "best.ask.price")]
# spread.filtered[, 2:3] <- spread.filtered[, 2:3]*0.01
#depth.filtered <- depth[depth$price >= min(trades$price)-5
#                      & depth$price <= max(trades$price)+5, ]


plot.price.levels.faster <- function(trades, depth, spread, start.time=head(spread$timestamp, 1), end.time=tail(spread$timestamp, 1), 
    show.trades=T, show.spread=T, show.mp=F, show.all.depth=T, col.bias=0.1) { 
  trade.range <- trades
  buys <- trade.range[trade.range$direction == "buy", ]
  sells <- trade.range[trade.range$direction == "sell", ]
  filtered <- filter.depth(depth, start.time, end.time)
  # remove price levels with no update during time window.
  if(!show.all.depth) {
    unchanged <- tapply(filtered$timestamp, filtered$price, function(v) {
      length(v) == 2 & v[1] == start.time & v[2] == end.time
    })
    unchanged.prices <- unique(filtered$price)
    unchanged.prices <- unchanged.prices[unchanged]
    filtered <- filtered[!filtered$price %in% unchanged.prices, ]
    print(paste("removed", length(unchanged.prices), "unchanged depth levels"))
  }
  filtered[filtered$volume==0, ]$volume<-NA
  log.10 <- F
  if(col.bias <= 0) {
    col.bias <- 1
    log.10 <- T
  }
  col.pal <- colorRampPalette(c("#f92b20", "#fe701b", "#facd1f", "#d6fd1c",
      "#65fe1b", "#1bfe42", "#1cfdb4", "#1fb9fa", "#1e71fb", "#261cfd"), bias=col.bias)(length(unique(filtered$volume)))
  col.pal <- rev(col.pal)
  quantiles <- quantile(filtered$volume, probs=seq(0.5, 1, 0.5), na.rm=T)
  print("price level quantiles:")
  print(quantiles)
  p <- ggplot()
  if(show.mp) {
    p <- p + geom_line(data=spread, aes(x=timestamp, y=(best.bid.price+best.ask.price)/2), col="#ffffff", size=1.1)
  }
  # set alpha to 0 for na, 0.1 for volume <1, 1 otherwise.
  p <- p + geom_line(data=filtered, mapping=aes(colour=volume, x=timestamp, y=price, group=price, alpha=ifelse(is.na(volume), 0, ifelse(volume < 1, 0.1, 1)))) #size=1
  p <- p + scale_y_continuous(breaks=seq(round(min(filtered$price)), round(max(filtered$price)), by=0.5), name="limit price")
  if(log.10)
    p <- p + scale_colour_gradientn(colours=col.pal, trans="log10", na.value="black")
  else
    p <- p + scale_colour_gradientn(colours=col.pal, na.value="black", name="volume        \n", breaks=as.vector(quantiles), labels=sprintf("%7s", sprintf("%.7s", quantiles)))
  p <- p + scale_alpha_continuous(range=c(0, 1), guide="none") #remove alpha legend.

  if(show.spread) {
    p <- p + geom_step(data=spread, aes(x=timestamp, y=best.ask.price), col="#ff0000", size=1.5) #, linetype="dashed")
    p <- p + geom_step(data=spread, aes(x=timestamp, y=best.bid.price), col="#00ff00", size=1.5) # alpha=0.5)#, linetype="longdash")
  }

  if(show.trades) {
    p <- p + geom_point(data=sells, aes(x=timestamp, y=price), colour="#ffffff", size=6, shape=1)
    p <- p + geom_point(data=sells, aes(x=timestamp, y=price), colour="#ff0000", size=5, shape=1)
    p <- p + geom_point(data=sells, aes(x=timestamp, y=price), colour="#ffffff", size=4, shape=1)
    p <- p + geom_point(data=buys, aes(x=timestamp, y=price), colour="#ffffff", size=6, shape=1)
    p <- p + geom_point(data=buys, aes(x=timestamp, y=price), colour="#00ff00", size=5, shape=1)
    p <- p + geom_point(data=buys, aes(x=timestamp, y=price), colour="#00ff00", size=5, shape=1)
  }
  p <- p + theme.black()
  p <- p + theme(legend.title=element_text(hjust=3, vjust=20))
  p <- p + xlab("time")
  p
}

# quote map (shows point in time where an order was added or deleted)
# good for seeing algo patterns and quote stuffers.
plot.quote.map <- function(events, start.time=default.start.time, end.time=default.end.time) {
  events <- events[events$timestamp >= start.time & events$timestamp <= end.time
                 & (events$type == "flashed-limit" | events$type == "flashed-limit"), ]
  created <- events[events$action == "created", ]
  deleted <- events[events$action == "deleted", ]
  col.pal <- c("#0000ff", "#ff0000")
  names(col.pal) <- c("bid", "ask")
  p <- ggplot(data=events, mapping=aes(x=timestamp, y=price))
  p <- p + scale_y_continuous(breaks=seq(round(min(events$price)), round(max(events$price)), by=0.5), name="limit price")
  p <- p + geom_point(data=created, mapping=aes(size=volume), colour="#333333", shape=19)
  p <- p + geom_point(data=deleted, mapping=aes(size=volume), colour="#333333", shape=1)
  p <- p + scale_size_continuous(name="volume        \n") #, labels=function(y) sprintf("%8s", y))
  p <- p + geom_point(data=events, mapping=aes(colour=direction), size=0.1)
  p <- p + scale_colour_manual(values=col.pal, guide="none")
  p <- p + theme.black()
  p <- p + xlab("time")
  p
}

# cancellation map (by volume)
# good for showing quote stuffing and for algo identification.
plot.cancellation.volume.map <- function(events, start.time=default.start.time, end.time=default.end.time) {
  plot.volume.map(events, "deleted", start.time, end.time)
}

plot.created.volume.map <- function(events, start.time=default.start.time, end.time=default.end.time) {
  plot.volume.map(events, "created", start.time, end.time)
}

plot.volume.map <- function(events, action, start.time=default.start.time, end.time=default.end.time) {
  filtered <- events[events$action == action 
                   & events$type == "flashed-limit"
                   & events$timestamp >= start.time & events$timestamp <= end.time, ]
  col.pal <- c("#0000ff", "#ff0000")
  names(col.pal) <- c("bid", "ask")
  p <- ggplot(data=filtered, mapping=aes(x=timestamp, y=volume))
  p <- p + geom_point(mapping=aes(colour=direction), size=1, shape=15)
  p <- p + scale_colour_manual(values=col.pal, name="direction     \n")
  p <- p + scale_y_continuous(name="cancelled volume", labels=function(y) sprintf("%5s", y))
  p <- p + xlab("time")
  p + theme.black()
}

# combined quote and cancellation map (x axis ligned up.
plot.combined.quote.map <- function(events, trades, start.time=default.start.time, end.time=default.end.time) {
  trade.range <- trades[trades$timestamp >= start.time & trades$timestamp <= end.time, ]
  filtered <- events[events$timestamp >= start.time & events$timestamp <= end.time
                   & events$price >= min(trade.range$price)-5 & events$price <= max(trade.range$price)+5 , ]
  col.pal <- c("#0000ff", "#ff0000")
  names(col.pal) <- c("bid", "ask")

  ###
  #
  # here, we need to trick ggplot so that we can align x-axis of 2 plots...
  #
  ###

  # quotes dataframe for first panel.
  quotes.data <- cbind(filtered, panel=rep("quotes", nrow(filtered)))

  # cancelled volume dataframe for second panel.
  cancelled.data <- filtered[filtered$action == "deleted" , ]
  cancelled.data <- cbind(cancelled.data, panel=rep("cancelled", nrow(cancelled.data)))

  p <- ggplot(data=quotes.data, mapping=aes(x=timestamp, y=price, shape=action))
  p <- p + facet_grid(panel~., scale="free")
  p <- p + geom_point(aes(size=volume), colour="#555555", alpha=0.5)
  p <- p + geom_point(aes(colour=direction), size=0.5)
  p <- p + scale_colour_manual(values=col.pal)
  p <- p + geom_point(data=cancelled.data, aes(x=timestamp, y=volume, colour=direction), size=1, shape=15)

  p <- p + theme.black()
  p 
}

# order book cumulative volume at given point in time
plot.current.depth <- function(order.book, ascii=F) {
  bids <- reverse.matrix(order.book$bids)
  asks <- reverse.matrix(order.book$asks)
  x <- c(bids$price, tail(bids$price, 1), head(asks$price, 1), asks$price)
  y <- c(bids$liquidity, 0, 0, asks$liquidity)
  if(ascii)
    txtplot(y, x=x, width=80)
  else {
    col.pal <- c("#ff0000", "#0000ff")
    side <- c(rep("bid", nrow(bids)+1), rep("ask", nrow(asks)+1))
    depth <- data.frame(price=x, liquidity=y, side=side)
    p <- ggplot(depth, aes(x=price, y=liquidity, group=side, colour=side))
    p <- p + scale_x_continuous(breaks=seq(round(min(bids$price)), round(max(asks$price)), by=1))
    p <- p + scale_colour_manual(values=col.pal)  
    p <- p + geom_step()
    p <- p + ggtitle(as.POSIXct(order.book$timestamp, origin="1970-01-01", tz="UTC"))
    #p <- p + scale_x_reverse()
    p <- p + theme.black()
    p
  }
}

plot.trade.spread <- function(trades) {
  plot(na.locf(ifelse(trades$direction=="sell",trades$price,NA),na.rm=F), type="s", col="blue")
  lines(na.locf(ifelse(trades$direction=="buy",trades$price,NA),na.rm=F), type="s", col="red")
}

plot.test.ob.metrics <- function() {
  plot(tail(best.bids, -1000), type="l", col="blue")
  lines(tail(best.asks, -1000), col="red")
  matplot(t(apply(bid.percentiles, 1, cumsum)), type="l")
  plot(norml(tail(ask.percentiles[,20], -1000)), type="s", col="red")
  lines(norml(tail(bid.percentiles[,20], -1000)), col="blue")
  lines(norml(tail(best.bids, -1000)), col="black")
  plot(sign((bid.percentiles[,20]-ask.percentiles[,20])/(bid.percentiles[,20]+ask.percentiles[,20])), type="s")
}

# volume percentile map
plot.depth.percentiles <- function(depth.summary, start.time=default.start.time, end.time=default.end.time) {
  plot.percentiles("vol", depth.summary, start.time, end.time)
}

# gap percentile map
plot.gap.percentiles <- function(depth.summary, start.time, end.time) {
  plot.percentiles("gap", depth.summary, start.time, end.time)
}

plot.percentiles <- function(pct.type, depth.summary, 
    start.time=default.start.time,
    end.time=default.end.time, 
    transform=function(x) x) {
  print(paste("plot depth percentiles between", start.time, "and", end.time))
  library(reshape2)
  bid.names <- paste0("bid.", pct.type, seq(from=25, to=500, by=25), "bps")
  ask.names <- paste0("ask.", pct.type, seq(from=25, to=500, by=25), "bps")

  td <- difftime(end.time, start.time, units="secs")
  print(td)
  td <- round(as.numeric(td))

#  if(td > 60) { # <= 1 minute = ticks.
    frequency <- ifelse(td > 900, "mins", "secs")
    ob.percentiles <- depth.summary[depth.summary$timestamp >= start.time-ifelse(frequency == "mins", 60, 1) & depth.summary$timestamp <= end.time, c("timestamp", bid.names, ask.names)]
    print(paste("aggregating to", frequency, "intervals"))
    ob.percentiles <- ob.percentiles[!duplicated(ob.percentiles$timestamp, fromLast=T), ] # remove duplicates (take last entry) (for zoo to work)
    zoo.obj <- to.zoo(ob.percentiles) # convert to zoo object
    intervals <- as.POSIXct(trunc(time(zoo.obj), frequency)) # intervals truncated to frequency
    print(paste("aggregation:", min(intervals), ":", max(intervals), "by =", frequency)) 
    aggregated <- aggregate(zoo.obj, intervals, mean) # aggregate by intervals
    ob.percentiles <- data.frame(timestamp=unique(intervals)+ifelse(frequency == "mins", 60, 1), aggregated, row.names=NULL)
#  } else {
#    ob.percentiles <- depth.summary[depth.summary$timestamp >= start.time & depth.summary$timestamp <= end.time, c("timestamp", bid.names, ask.names)]
#  }

  bid.names <- paste0("bid.", pct.type, sprintf("%03d", seq(from=25, to=500, by=25)), "bps")
  ask.names <- paste0("ask.", pct.type, sprintf("%03d", seq(from=25, to=500, by=25)), "bps")
  colnames(ob.percentiles) <- c("timestamp", bid.names, ask.names) 
  max.ask <- max(rowSums(ob.percentiles[, 22:41]))
  max.bid <- max(rowSums(ob.percentiles[, 2:21]))
  y.range <- transform(max(max.ask, max.bid)) # centre
  melted.asks <- melt(ob.percentiles, id.vars="timestamp", measure.vars=ask.names, variable.name="percentile", value.name="liquidity")
  melted.asks$percentile <- factor(melted.asks$percentile, rev(ask.names))
  melted.asks$liquidity <- transform(melted.asks$liquidity)
  melted.bids <- melt(ob.percentiles, id.vars="timestamp", measure.vars=bid.names, variable.name="percentile", value.name="liquidity")
  melted.bids$percentile <- factor(melted.bids$percentile, bid.names)
  melted.bids$liquidity <- transform(melted.bids$liquidity)
  col.pal <- colorRampPalette(c("#f92b20", "#fe701b", "#facd1f", "#d6fd1c", "#65fe1b", "#1bfe42", "#1cfdb4", "#1fb9fa", "#1e71fb", "#261cfd"))(20)
  col.pal <- c(col.pal, col.pal)
  breaks <- c(rev(paste0("ask.", pct.type, sprintf("%03d", seq(from=50, to=500, by=50)), "bps")),
              paste0("bid.", pct.type, sprintf("%03d", seq(from=50, to=500, by=50)), "bps"))
  legend.names <- c(rev(paste0("+", sprintf("%03d", seq(from=50, to=500, by=50)), "bps")),
                    paste0("-", sprintf("%03d", seq(from=50, to=500, by=50)), "bps"))
  print("creating plot..")                  
  p <- ggplot(data=melted.asks, mapping=aes(x=timestamp, y=liquidity, fill=percentile))
  p <- p + geom_area(position="stack")
  p <- p + geom_line(mapping=aes(ymax=0), position="stack", col="black", size=0.25)
  p <- p + geom_area(data=melted.bids, aes(x=timestamp, y=-liquidity, fill=percentile), position="stack")
  p <- p + geom_line(data=melted.bids, aes(x=timestamp, y=-liquidity, ymax=0), position="stack", col="black", size=0.25)
  p <- p + scale_fill_manual(values=col.pal, breaks=breaks, labels=legend.names, name="depth         \n")
  p <- p + ylim(-y.range, y.range)
  p <- p + xlab("time")
  p <- p + theme.black()
  p
}

# x=price vs volume histogram. volume could be traded volume, cancelled volume, cancellations, addded volume, volume by order type etc.
# here it is just the count of order events at each price level to give an idea of activity.
plot.price.histogram <- function(events,
    start.time=default.start.time,
    end.time=default.end.time) {
  events <- events[events$timestamp >= start.time & events$timestamp <= end.time, ] 
  #if(nrow(events) == 0) return(ggplot(data=data.frame(x=0, y=0), mapping=aes(x=x, y=y)) + geom_blank() + theme.black())

  td <- difftime(end.time, start.time, units="secs")
  td <- round(as.numeric(td))

  bw=1
 
  if(td > 10800)
    bw=10

  p <- ggplot(data=events, mapping=aes(x=price, fill=direction, colour=direction))
  p <- p + geom_bar(position="dodge", binwidth=bw)
  #p <- ggplot(data=pv, mapping=aes(x=price, y=volume, fill=direction, colour=direction))
  #p <- p + geom_bar(position="dodge", stat="identity")
  p <- p + scale_colour_manual(values=c("#ff0000", "#0000ff"))
  p <- p + scale_fill_manual(values=c("#ff0000", "#0000ff"))
  p <- p + ggtitle(paste("events price distribution"))  
  p + theme.black()
}

# x=volume vs event count. 
plot.volume.histogram <- function(events,
    start.time=default.start.time,
    end.time=default.end.time) {
  events <- events[events$timestamp >= start.time & events$timestamp <= end.time, ]
  #if(nrow(trades) == 0) return(ggplot(data=data.frame(x=0, y=0), mapping=aes(x=x, y=y)) + geom_blank() + theme.black())

  td <- difftime(end.time, start.time, units="secs")
  td <- round(as.numeric(td))


  bw=0.25

  if(td > 10800)
    bw=5

  p <- ggplot(data=events, mapping=aes(x=volume, fill=direction, colour=direction))
  p <- p + geom_bar(binwidth=bw, position="dodge")
  p <- p + scale_colour_manual(values=c("#0000ff", "#ff0000"))
  p <- p + scale_fill_manual(values=c("#0000ff", "#ff0000"))
  p <- p + ggtitle("events volume distribution")
  p + theme.black()
}

