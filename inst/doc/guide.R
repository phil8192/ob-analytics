## ---- include=FALSE------------------------------------------------------
library(obAnalytics)
knitr::opts_chunk$set(dpi=100, fig.width=8, fig.height=6, results="hide") 

## ------------------------------------------------------------------------
with(lob.data, plotTrades(trades))

## ------------------------------------------------------------------------
spread <- with(lob.data, getSpread(depth.summary))
p1 <- with(lob.data, plotPriceLevels(depth, spread, col.bias=0.1, volume.scale=10^-8))
p2 <- with(lob.data, plotPriceLevels(depth, spread, col.bias=0.1, volume.scale=10^-8, volume.from=50))
library(grid)
pushViewport(viewport(layout=grid.layout(1, 2)))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))

## ------------------------------------------------------------------------
spread <- with(lob.data, getSpread(depth.summary))
with(lob.data, plotPriceLevels(depth, spread, trades,
   start.time=as.POSIXct("2015-05-01 03:00:00.000", tz="UTC"),
   end.time=as.POSIXct("2015-05-01 04:00:00.000", tz="UTC"),
   volume.scale=10^-8))

## ------------------------------------------------------------------------
spread <- with(lob.data, getSpread(depth.summary))
with(lob.data, plotPriceLevels(depth, spread,
    show.mp=FALSE,
    start.time=as.POSIXct("2015-05-01 03:30:00.000", tz="UTC"),
    end.time=as.POSIXct("2015-05-01 03:45:00.000", tz="UTC")))

