## ---- include=FALSE------------------------------------------------------
library(obAnalytics)
knitr::opts_chunk$set(dpi=100, fig.width=10, fig.height=6, results="hide")

## -----------------------------------------------------------------------------
max.cols <- Sys.getenv("COLUMNS")
options(width=if(max.cols != "") max.cols else 80, scipen=999, digits.secs=3)

## ---- eval=F------------------------------------------------------------------
#  # load and process example csv data from the package inst/extdata directory.
#  csv.file <- system.file("extdata", "orders.csv.xz", package="obAnalytics")
#  lob.data <- processData(csv.file)

## -----------------------------------------------------------------------------
data(lob.data)

## -----------------------------------------------------------------------------
one.sec <- with(lob.data, {
  events[events$timestamp >= as.POSIXct("2015-05-01 04:55:10", tz="UTC") & 
         events$timestamp <= as.POSIXct("2015-05-01 04:55:11", tz="UTC"),  ]
})
one.sec$volume <- one.sec$volume*10^-8
one.sec$fill <- one.sec$fill*10^-8
one.sec$aggressiveness.bps <- round(one.sec$aggressiveness.bps, 2)
one.sec <- one.sec[, c("event.id", "id", "price", "volume", "action", 
    "direction", "fill", "matching.event", "type", "aggressiveness.bps")]
colnames(one.sec) <- c(c("event.id", "id", "price", "vol", "action", "dir", 
    "fill", "match", "type", "agg"))
print(one.sec, row.names=F)

## ---- echo=F, results="markup"------------------------------------------------
knitr::kable(one.sec, row.names=F)

## -----------------------------------------------------------------------------
trades.ex <- tail(lob.data$trades, 10)
trades.ex$volume <- round(trades.ex$volume*10^-8, 2)
print(trades.ex, row.names=F)

## ----example trades, echo=F, results="markup"---------------------------------
knitr::kable(trades.ex, digits=2, row.names=F)

## -----------------------------------------------------------------------------
# get a limit order book for a specific point in time, limited to +- 150bps
# above/below best bid/ask price.
lob <- orderBook(lob.data$events, 
    tp=as.POSIXct("2015-05-01 04:38:17.429", tz="UTC"), bps.range=150)

# visualise the order book liquidity.
plotCurrentDepth(lob, volume.scale=10^-8)

## ---- fig.cap="a"-------------------------------------------------------------
# plot all lob.data price level volume between $247 and $245 and overlay the 
# market midprice.
spread <- getSpread(lob.data$depth.summary)
plotPriceLevels(lob.data$depth, spread, price.from=227, price.to=245, 
    volume.scale=10^-8, col.bias=0.25, show.mp=T)

## -----------------------------------------------------------------------------
# plot 1 hour of trades centred around the bid/ask spread. 
plotPriceLevels(lob.data$depth, trades=lob.data$trades, 
    price.from=236, price.to=237.75, volume.scale=10^-8, col.bias=0.2,
    start.time=as.POSIXct("2015-05-01 01:00:00.000", tz="UTC"),
    end.time=as.POSIXct("2015-05-01 02:00:00.000", tz="UTC"))

## -----------------------------------------------------------------------------
# zoom in to 30 minutes of bid/ask quotes.
plotPriceLevels(lob.data$depth, spread, price.from=235.25, price.to=237,
    start.time=as.POSIXct("2015-05-01 00:45:00.000", tz="UTC"), 
    end.time=as.POSIXct("2015-05-01 01:15:00.000", tz="UTC"), 
    volume.scale=10^-8, col.bias=0.5, show.mp=F)

## -----------------------------------------------------------------------------
# zoom in to 4 minutes of bid/ask quotes.
plotPriceLevels(lob.data$depth, spread, price.from=235.90, price.to=236.25,
    start.time=as.POSIXct("2015-05-01 00:55:00.000", tz="UTC"), 
    end.time=as.POSIXct("2015-05-01 00:59:00.000", tz="UTC"), 
    volume.scale=10^-8, col.bias=0.5, show.mp=F)

## -----------------------------------------------------------------------------
# 
plotPriceLevels(lob.data$depth, spread, price.from=232.5, price.to=237.5,
    volume.scale=10^-8, col.bias=1, show.mp=T,
    end.time=as.POSIXct("2015-05-01 01:30:00.000", tz="UTC"),
    volume.from=8.59, volume.to=8.72)

## -----------------------------------------------------------------------------
#
plotPriceLevels(lob.data$depth, price.from=235.65, price.to=237.65,
    volume.scale=10^-8, col.bias=1,
    start.time=as.POSIXct("2015-05-01 01:00:00.000", tz="UTC"),
    end.time=as.POSIXct("2015-05-01 03:00:00.000", tz="UTC"),
    volume.from=3.63, volume.to=3.83)

## -----------------------------------------------------------------------------
plotVolumePercentiles(lob.data$depth.summary, volume.scale=10^-8, perc.line=F, start.time=as.POSIXct("2015-05-01 01:00:00.000", tz="UTC"),
    end.time=as.POSIXct("2015-05-01 04:00:00.000", tz="UTC"))

## -----------------------------------------------------------------------------
# visualise 15 minutes of order book liquidity.
# data will be aggregated to second-by-second resolution.
plotVolumePercentiles(lob.data$depth.summary,
start.time=as.POSIXct("2015-05-01 04:30:00.000", tz="UTC"),
end.time=as.POSIXct("2015-05-01 04:35:00.000", tz="UTC"),
volume.scale=10^-8)

## -----------------------------------------------------------------------------
plotVolumeMap(lob.data$events, volume.scale=10^-8, log.scale = T)

## -----------------------------------------------------------------------------
plotVolumeMap(lob.data$events, volume.scale=10^-8, volume.from=3.5, volume.to=4)

## -----------------------------------------------------------------------------
plotVolumeMap(lob.data$events, volume.scale=10^-8, volume.from=8.59, volume.to=8.72)

## -----------------------------------------------------------------------------
tp <- as.POSIXct("2015-05-01 04:25:15.342", tz="UTC")
ob <- orderBook(lob.data$events, max.levels=10)
print(ob)

## ----example order book, echo=F, results="markup"-----------------------------
with(ob, {
  asks$liquidity <- asks$liquidity*10^-8
  bids$liquidity <- bids$liquidity*10^-8
  cols <- c("id", "timestamp", "liquidity", "price")
  knitr::kable(cbind(bids[, cols], asks[order(asks$liquidity), rev(cols)]), 
      row.names=F, align=c("r","r","r","r","l","l","l","l"), digits=2)
})

## -----------------------------------------------------------------------------
impacts <- tradeImpacts(lob.data$trades)
impacts <- impacts[impacts$dir == "sell", ]
bps <- 10000 * (impacts$max.price - impacts$min.price) / impacts$max.price
types <- with(lob.data, events[match(impacts$id, events$id), ]$type)
impacts <- cbind(impacts, type=types, bps)
head(impacts[order(-impacts$bps), ], 10)

## ----impacts example, echo=F, results="markup"--------------------------------
impacts <- tradeImpacts(lob.data$trades)
impacts <- impacts[impacts$dir == "sell", ]
impacts <- impacts[, c("id", "max.price", "min.price", "vwap", "hits", "vol", 
    "end.time")]
impacts$vol <- impacts$vol*10^-8
bps <- 10000 * (impacts$max.price - impacts$min.price) / impacts$max.price
types <- with(lob.data, events[match(impacts$id, events$id), ]$type)
impacts <- cbind(impacts, type=types, bps)
knitr::kable(head(impacts[order(-impacts$bps), ], 10), row.names=F, digits=2)

## -----------------------------------------------------------------------------
impact <- with(lob.data, trades[trades$taker == 65596324, 
    c("timestamp", "price", "volume", "maker")])
makers <- with(lob.data, events[match(impact$maker, events$id), ])
makers <- makers[makers$action == "created", 
    c("id", "timestamp", "aggressiveness.bps")]
impact <- cbind(impact, maker=makers[match(impact$maker, makers$id), 
    c("timestamp", "aggressiveness.bps")])
age <- impact$timestamp - impact$maker.timestamp
impact <-  cbind(impact[!is.na(age), c("timestamp", "price", "volume", 
    "maker.aggressiveness.bps")], age[!is.na(age)])
colnames(impact) <- c("timestamp", "price", "volume", "maker.agg", "age")
impact$volume <- impact$volume*10^-8
print(impact)

## ----impact example, echo=F, results="markup"---------------------------------
knitr::kable(impact, row.names=F, digits=2)

