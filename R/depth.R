# calculate price level volume (depth)

price.level.volume <- function(events) {

  print("calculating price level volume from bid event deltas...")
  depth.bid <- directional.price.level.volume(events[events$direction == "bid", ])
  print("calculating price level volume from ask event deltas...")
  depth.ask <- directional.price.level.volume(events[events$direction == "ask", ])

  depth.data <- rbind(depth.bid, depth.ask)
  depth.data[order(depth.data$timestamp), ]
}

directional.price.level.volume <- function(events) {
  added.volume <- events[(events$action == "created" | (events$action == "changed" & events$fill == 0)) & events$type != "pacman" & events$type != "market",
      c("event.id", "id", "timestamp", "exchange.timestamp", "price", "volume", "direction", "action")]
  cancelled.volume <- events[(events$action == "deleted" & events$volume > 0) & events$type != "pacman" & events$type != "market",
      c("event.id", "id", "timestamp", "exchange.timestamp", "price", "volume", "direction", "action")]
  cancelled.volume$volume <- -cancelled.volume$volume
  # remove deletes with no previous add.
  cancelled.volume <- cancelled.volume[cancelled.volume$id %in% added.volume$id, ]
  filled.volume <- events[events$fill > 0 & events$type != "pacman" & events$type != "market",
      c("event.id", "id", "timestamp", "exchange.timestamp", "price", "fill", "direction", "action")]
  filled.volume$fill <- -filled.volume$fill
  # remove fills with no previous add.
  filled.volume <- filled.volume[filled.volume$id %in% added.volume$id, ]
  colnames(filled.volume) <- c("event.id", "id", "timestamp", "exchange.timestamp", "price", "volume", "direction", "action")
  volume.deltas <- rbind(added.volume, cancelled.volume, filled.volume)

  ## bug: (assumes 1 order per price level for a given time period...)
  #volume.deltas <- volume.deltas[order(volume.deltas$price, volume.deltas$event.id), ]
  # should be: (given that event timestamps have been set _in order_ during quotes processing stage:)
  volume.deltas <- volume.deltas[order(volume.deltas$price, volume.deltas$timestamp), ]
  # ^^-- so price level deltas are now in order and order life-cycles can overlap..

  cum.volume <- unlist(tapply(volume.deltas$volume, volume.deltas$price, function(volume) cumsum(volume)), use.names=F)

  # this can happen with missing data...
  cum.volume <- ifelse(cum.volume < 0, 0, cum.volume)

  # part of above^ bug: (incorrect ordering.) 
  # order by timestamp, then cbind cum.volume (cum.volume calculated with order: price, [create,update,delete], then timestamp. 
  # --- volume.deltas may not be in this order.. (deletes may come before creates)
  #cbind(volume.deltas[order(volume.deltas$price, volume.deltas$timestamp), c("timestamp", "price")], volume=cum.volume, side=volume.deltas$direction)

  cbind(volume.deltas[, c("timestamp", "price")], volume=cum.volume, side=volume.deltas$direction)
}

#83669     24126 39957185 2014-10-06 07:09:45.428 2014-10-06 01:01:33   300   24327946757 changed       ask  market-limit   1000000000          86511
#83671     24127 39957185 2014-10-06 07:09:45.476 2014-10-06 01:01:33   300             0 deleted       ask  market-limit  24327946757          86513
#18496     24206 39957225 2014-10-06 01:01:43.729 2014-10-06 01:01:43   300      11670142 created       bid       unknown            0             NA

#19990     25659 39957825 2014-10-06 01:03:38.134 2014-10-06 01:03:37   300   20871273804 created       ask flashed-limit            0             NA
#20255     25660 39957825 2014-10-06 01:04:01.131 2014-10-06 01:03:37   300   20871273804 deleted       ask flashed-limit            0             NA


#83669  2014-10-06 07:09:45.428   300   24327946757  ask
#83671  2014-10-06 07:09:45.476   300             0  ask
#18496  2014-10-06 01:01:43.729   300      11670142  bid
#18523  2014-10-06 01:01:45.463   300             0  bid


# given price level volume events, (output of price.level.volume function), 
# truncate to seconds, fill in gaps between volume events (like a run length encoding).
# very very slow. 
depth.levels <- function(price.level.events, from=as.POSIXct("1970-01-01 00:00:00.000", tz="UTC"), 
    progress=function(i) print(paste("processing", i))) {

  # align volume events to ticks
  price.series <- function(price.events, x.ticks) {
    #price.events$timestamp <- trunc(price.events$timestamp, "secs")
    # remove duplicate times (take last entry on close)
    #price.events <- price.events[!duplicated(price.events$timestamp, fromLast=T), ]
    price.events.zoo <- zoo(price.events[, c("price", "volume")], price.events$timestamp)

    # remove duplicate times (take last entry on close)
    #price.events.zoo <- aggregate(price.events.zoo, identity, tail, 1)

    filled <- na.locf(price.events.zoo, xout=x.ticks)
    filled <- filled[index(filled) >= first(price.events$timestamp)]

    # optional....
    filled <- filled[filled$volume > 0, ]

    data.frame(timestamp=index(filled), filled, row.names=NULL)
  }

  if(!is.null(price.level.events) && nrow(price.level.events) > 0) {
    y.level <- sort(unique(price.level.events$price))
    x.range <- trunc(range(price.level.events$timestamp), "secs")
    x.ticks <- seq(x.range[1], x.range[2], by="1 sec")

    price.level.events$timestamp <- trunc(price.level.events$timestamp, "secs")
    price.level.events <- price.level.events[!duplicated(price.level.events[, c("timestamp", "price")], fromLast=T), ]

    do.call(rbind, lapply(y.level, function(pl) {
      progress(pl)
      ps <- price.level.events[price.level.events$price == pl, ]
      ps <- price.series(ps, x.ticks)
      ps[ps$timestamp >= from, ]
    }))
  }
}

# depth level changes between a range.
# timestamp of last depth level change < begining of range shifted forward to edge of begining.
filter.depth <- function(depth, from, to) {
  print(paste("filter depth between", from, "and", to))
  pre <- depth[depth$timestamp <= from, ]
  print(paste("got", nrow(pre), "previous deltas"))
  pre <- pre[order(pre$price, pre$timestamp), ]
  print(paste("ordered", nrow(pre), "previous deltas"))
  # last update for each price level <= from. this becomes the starting point for all updates within the range.
  pre <- pre[!duplicated(pre$price, fromLast=T) & pre$volume > 0, ] 
  print(paste("extracted", nrow(pre), "previously updated deltas"))
  # clamp range
  if(nrow(pre) > 0) {
    pre$timestamp <- as.POSIXct(sapply(pre$timestamp, function(r) max(from, r)), origin="1970-01-01", tz="UTC") 
    print("clamped range.")
  }
  mid <- depth[depth$timestamp > from & depth$timestamp < to, ]
  print(paste("got", nrow(mid), "in range deltas"))
  range <- rbind(pre, mid)
  print(paste("appended range now contains", nrow(range), "deltas"))
  # close off loose ends.
  price.levels <- unique(range$price)
  # last side of each price level:
  range <- range[order(range$price, range$timestamp), ]
  last.sides <- range[!duplicated(range$price, fromLast=T), "side"]
  range <- rbind(range, data.frame(timestamp=to, price=price.levels, volume=0, side=last.sides))
  # ensure it is in order
  range <- range[order(range$price, range$timestamp), ]
  print(paste("closed range. depth filtering resulted in", length(unique(range$price)), "price levels."))
  range
}

# todo: this is dog slow. needs some work.

# pre-process order depth metrics.
# returns a matrix of the form:
# [timestamp, best bid price, best bid volume, volume at 20 percentile increments below best bid in 25bps bins: best.bid:(best.bid*0.95),
# vwap in 20 percentile bins, #price gaps in 20 percentile increments.. , same repeated for ask, except: best.ask:(best.ask*1.05). ]
depth.metrics <- function(depth) {
  pb <- txtProgressBar(1, nrow(depth), 0, style=3)
  pct.names <- function(pct.name) paste0(pct.name, seq(from=25, to=500, by=25), "bps")
  ordered.depth <- depth[order(depth$timestamp), ]
  ordered.depth$price <- as.integer(round(100*ordered.depth$price))
  depth.matrix <- cbind(ordered.depth$price, ordered.depth$volume, ifelse(ordered.depth$side == "bid", 0, 1))
  metrics <- matrix(0, ncol=124, nrow=nrow(ordered.depth), dimnames=list(1:nrow(ordered.depth), 
      c("best.bid.price", "best.bid.vol", pct.names("bid.vol"), pct.names("bid.vwap"), pct.names("bid.gap"),
        "best.ask.price", "best.ask.vol", pct.names("ask.vol"), pct.names("ask.vwap"), pct.names("ask.gap"))))  
  asks.state <- integer(length=1000000) # the volume state for all price level depths. (updated in loop)
  asks.state[1000000] <- 1 # trick (so there is an initial best ask)
  bids.state <- integer(length=1000000)
  bids.state[1] <- 1 # trick
  # initial best bid/ask
  best.ask <- max(ordered.depth[ordered.depth$side=="ask", ]$price)
  best.bid <- min(ordered.depth[ordered.depth$side=="bid", ]$price)
  best.ask.vol <- 0
  best.bid.vol <- 0
  for(i in 1:(nrow(ordered.depth))) {  
    depth.row <- depth.matrix[i, ]
    price <- depth.row[1]
    volume <- depth.row[2]
    side <- depth.row[3]
    if(side > 0) { # ask
      if(price > best.bid) {
        asks.state[price] <- volume
        if(volume > 0) {
          if(price < best.ask) {
            best.ask <- price
            best.ask.vol <- volume
          } else if(price == best.ask) {
            best.ask.vol <- volume
          }
        } else {
          if(price == best.ask) {
            best.ask <- head(which(asks.state > 0), 1)
            best.ask.vol <- asks.state[best.ask]
          }
        }
        price.range <- best.ask:round(1.05*best.ask)
        volume.range <- asks.state[price.range]
        breaks <- ceiling(cumsum(rep(length(price.range)/20, 20)))
        metrics[i, 63] <- best.ask
        metrics[i, 64] <- best.ask.vol
        metrics[i, 65:84] <- interval.sum.breaks(volume.range, breaks)
        metrics[i, 85:104] <- interval.vwap(price.range, volume.range, breaks)
        metrics[i, 105:124] <- interval.price.level.gaps(volume.range, breaks)
        # copy last bid data (no need to re-calculate it)
        if(i > 1) metrics[i, 1:62] <- metrics[i-1, 1:62]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i-1, ]
      }
    } else {
      if(price < best.ask) {
        bids.state[price] <- volume
        if(volume > 0) {
          if(price > best.bid) {
            best.bid <- price
          } else if(price == best.bid) {
            best.bid.vol <- volume
          }
        } else {
          if(price == best.bid) {
            best.bid <- tail(which(bids.state > 0), 1)
            best.bid.vol <- bids.state[best.bid]
          }
        }
        price.range <- best.bid:round(0.95*best.bid)
        volume.range <- bids.state[price.range]
        breaks <- ceiling(cumsum(rep(length(price.range)/20, 20)))
        metrics[i, 1] <- best.bid
        metrics[i, 2] <- best.bid.vol
        metrics[i, 3:22] <- interval.sum.breaks(volume.range, breaks)
        metrics[i, 23:42] <- interval.vwap(price.range, volume.range, breaks)
        metrics[i, 43:62] <- interval.price.level.gaps(volume.range, breaks)
        # copy last ask data (no need to re-calculate it)
        if(i > 1) metrics[i, 63:124] <- metrics[i-1, 63:124]
      } else {
        # copy last data (no change)
        if(i > 1) metrics[i, ] <- metrics[i-1, ]
      }
    }
    setTxtProgressBar(pb, i)
  }
  cbind(timestamp=ordered.depth$timestamp, data.frame(metrics))
}

if(F){
  plot(tail(depth.summary$best.bid.price, -1000), type="l")
  lines(tail(depth.summary$best.ask.price, -1000), col="red")
  dev.new()
  plot(trades$price, type="l")
}

test.depth___ <- function() {
  test[which((best.asks-best.bids)*0.01>6.06), ]
  test[which(10000*((best.asks-best.bids)/best.bid) > 72.3), ]
  order.book(events, ts=as.POSIXct("2014-08-01 23:21:49.201", tz="UTC"), max.price.levels=10)
}

if(F){
  head(reverse.matrix(cbind(0.01*which(bids.state > 0), bids.state[bids.state > 0])))
  tail(reverse.matrix(cbind(0.01*which(asks.state > 0), asks.state[asks.state > 0])))
  plot(to.zoo(trades[trades$timestamp >= as.POSIXct("2014-10-06 06:00:00.000", tz="UTC") & trades$timestamp <= as.POSIXct("2014-10-06 07:15:00.000", tz="UTC"), 1:2]))
  bb <- cbind(timestamp=ordered.depth$timestamp, data.frame(0.01*metrics[, "best.bid.price"]))
  bb <- bb[bb$timestamp >= as.POSIXct("2014-10-06 06:00:00.000", tz="UTC") & bb$timestamp <= as.POSIXct("2014-10-06 07:15:00.000", tz="UTC"), ]
  ba <- cbind(timestamp=ordered.depth$timestamp, data.frame(0.01*metrics[, "best.ask.price"]))
  ba <- ba[ba$timestamp >= as.POSIXct("2014-10-06 06:00:00.000", tz="UTC") & ba$timestamp <= as.POSIXct("2014-10-06 07:15:00.000", tz="UTC"), ]
  lines(to.zoo(bb), col="blue")
  lines(to.zoo(ba), col="red")
}


#57735     61354 39975007 2014-10-06 04:42:34.112 2014-10-06 04:42:16 297.71           0 deleted       bid resting-limit 60604500          61485
#57837     61591 39975123 2014-10-06 04:42:52.003 2014-10-06 04:42:51 297.71    65000000 created       bid resting-limit        0             NA

#58225     62001 39975321 2014-10-06 04:45:13.579 2014-10-06 04:45:13 297.71    65000000 created       ask flashed-limit        0             NA
#58231     62002 39975321 2014-10-06 04:45:14.033 2014-10-06 04:45:13 297.71    65000000 deleted       ask flashed-limit        0             NA

#58461     61592 39975123 2014-10-06 04:46:20.328 2014-10-06 04:42:51 297.71    53347191 changed       bid resting-limit 11652809          62233 <---


#57735  2014-10-06 04:42:34.112 297.71         0  bid
#57837  2014-10-06 04:42:52.003 297.71  65000000  bid
#58225  2014-10-06 04:45:13.579 297.71 130000000  ask
#58231  2014-10-06 04:45:14.033 297.71  65000000  ask










#order.book(events, ts=as.POSIXct("2014-10-06 23:33:42.733", tz="UTC"), max.price.levels=10)
#depth.summary[depth.summary$timestamp == as.POSIXct("2014-10-06 23:33:42.733", tz="UTC"), ]

#16817    19314 39956260 2014-10-06 00:58:42.757 2014-10-06 00:57:40   300     174397049 deleted       bid flashed-limit           0             NA
#17499    19276 39956241 2014-10-06 01:00:02.759 2014-10-06 00:57:35   300      34256666 deleted       bid flashed-limit           0             NA
#17650    20692 39956890 2014-10-06 01:00:20.447 2014-10-06 01:00:20   300     100000000 created       bid resting-limit           0             NA
#18236    21339 39957185 2014-10-06 01:01:33.845 2014-10-06 01:01:33   300 2473273289440 created       ask  market-limit           0             NA
#18241    21340 39957185 2014-10-06 01:01:35.003 2014-10-06 01:01:33   300 2471323289440 changed       ask  market-limit  1950000000          21330
#18243    21341 39957185 2014-10-06 01:01:35.075 2014-10-06 01:01:33   300 2471302289440 changed       ask  market-limit    21000000          21206


#15643    18362 39955870 2014-10-06 00:56:36.954 2014-10-06 00:56:13   300   6083719777 changed       ask market-limit     8435873          18392
#15644    18363 39955870 2014-10-06 00:56:36.996 2014-10-06 00:56:13   300   6075278097 changed       ask market-limit     8441680          18400
#15647    18364 39955870 2014-10-06 00:56:37.069 2014-10-06 00:56:13   300            0 deleted       ask market-limit  6075278097          18496

#15644 2014-10-06 00:56:36.996   300   11297825136  ask
#15647 2014-10-06 00:56:37.069   300    5222547039  ask
#15722 2014-10-06 00:56:42.063   300    5512547039  bid

#15510 2014-10-06 00:56:32.424   300   24242528922  ask
#15591 2014-10-06 00:56:34.838   300   24253805961  bid
#15592 2014-10-06 00:56:34.846   300   24265075961  bid
#15600 2014-10-06 00:56:34.930   300   29465075961  bid
#15628 2014-10-06 00:56:36.618   300   26773093913  ask


#16765  2014-10-06 00:58:36.908   300     221648888  bid
#16817  2014-10-06 00:58:42.757   300      47251839  bid
#17499  2014-10-06 01:00:02.759   300      12995173  bid
#17650  2014-10-06 01:00:20.447   300     112995173  bid
#18236  2014-10-06 01:01:33.845   300 2473386284613  ask
#18241  2014-10-06 01:01:35.003   300 2471436284613  ask
#18243  2014-10-06 01:01:35.075   300 2471415284613  ask

#> events[events$id == 39956890, ]
#      event.id       id               timestamp  exchange.timestamp price    volume  action direction          type      fill matching.event
#17650    20692 39956890 2014-10-06 01:00:20.447 2014-10-06 01:00:20   300 100000000 created       bid resting-limit         0             NA
#18418    20693 39956890 2014-10-06 01:01:39.960 2014-10-06 01:00:20   300         0 deleted       bid resting-limit 100000000          21426

#
# point at which order comes to rest in book:
#10 39957185 2014-10-06 01:01:39.959 2014-10-06 01:01:33 300.00 2456505583020 2456505583020 0.0000000



#39957185
#2014-10-06 01:01:33.845 2014-10-06 01:01:33
#2014-10-06 07:09:45.476 2014-10-06 01:01:33

#      event.id       id               timestamp  exchange.timestamp price  action direction         type       fill matching.event
#18236    21339 39957185 2014-10-06 01:01:33.845 2014-10-06 01:01:33   300 created       ask market-limit          0             NA                                 24732.73

#depth:
#18245  2014-10-06 01:01:35.115   300  ask                       24732.73300000

#whale <- to.zoo(events[events$id == 39957185, c("timestamp", "volume")])
#whale.depth <- to.zoo(depth[depth$price==300 & depth$timestamp >= head(index(whale), 1) & depth$timestamp <= tail(index(whale), 1), c("timestamp", "volume")])
#plot(whale.depth)
#lines(whale, col="blue")

#which(abs(vector.diff(as.vector(whale.depth))) %in% events[events$id == 39957185, "fill"] == F)
#> whale.depth[2793, ]
#2014-10-06 06:27:03.622 
#               11670142
#
#> events[events$fill == 24327946757, ]
#      event.id       id               timestamp  exchange.timestamp price      volume  action direction         type        fill matching.event
#83671    24127 39957185 2014-10-06 07:09:45.476 2014-10-06 01:01:33   300           0 deleted       ask market-limit 24327946757          86513
#83670    86513 39987358 2014-10-06 07:09:45.476 2014-10-06 07:09:43   300 46257796576 changed       bid       market 24327946757          24127


#40841 2014-10-06 02:49:46.917   300 1947856503360  ask
#40856 2014-10-06 02:49:51.881   300 1947556503360  ask
#40874 2014-10-06 02:49:56.848   300 1827623172250  ask
#40887 2014-10-06 02:49:59.381   300 1826123172250  ask
#40911 2014-10-06 02:50:06.642   300 1826121475580  ask


#> whale[which(whale[order(whale$timestamp), ] != whale), ]
#      event.id       id               timestamp  exchange.timestamp price    volume direction  action
#24678    21861 39957185 2014-10-06 01:14:33.476 2014-10-06 01:01:33   300  -4200000       ask changed
#24675    21862 39957185 2014-10-06 01:14:33.380 2014-10-06 01:01:33   300 -51800000       ask changed
#80706    23862 39957185 2014-10-06 06:57:05.266 2014-10-06 01:01:33   300 -26462100       ask changed
#80704    23863 39957185 2014-10-06 06:57:05.142 2014-10-06 01:01:33   300 -13537900       ask changed




#75326     23748 39957185 2014-10-06 06:26:20.831 2014-10-06 01:01:33   300 changed       ask  market-limit     20000010             NA                                 1.389874e+04
#75327     78698 39983508 2014-10-06 06:26:20.832 2014-10-06 06:26:19   300 deleted       bid       unknown     20000000             NA                                 0.000000e+00
#75426     23749 39957185 2014-10-06 06:26:51.219 2014-10-06 01:01:33   300 changed       ask  market-limit   4413331110             NA                                 1.385461e+04
#75458     23750 39957185 2014-10-06 06:27:01.105 2014-10-06 01:01:33   300 changed       ask  market-limit     50000000          78829                                 1.385411e+04
#75461     78834 39983575 2014-10-06 06:27:02.424 2014-10-06 06:27:02   300 created       bid       unknown            0             NA                                 5.354357e+01
#75464     23751 39957185 2014-10-06 06:27:03.568 2014-10-06 01:01:33   300 changed       ask  market-limit     20556000          78833                                 1.385390e+04

#75466     78835 39983575 2014-10-06 06:27:03.621 2014-10-06 06:27:02   300 deleted       bid       unknown   5354356662             NA                                 0.000000e+00

#75467     23752 39957185 2014-10-06 06:27:03.622 2014-10-06 01:01:33   300 changed       ask  market-limit   5354356660             NA                                 1.380036e+04
#75475     23753 39957185 2014-10-06 06:27:06.147 2014-10-06 01:01:33   300 changed       ask  market-limit     12800000          78845                                 1.380023e+04
#75486     23754 39957185 2014-10-06 06:27:08.709 2014-10-06 01:01:33   300 changed       ask  market-limit   1390000000          78861                                 1.378633e+04
#75539     23755 39957185 2014-10-06 06:27:23.328 2014-10-06 01:01:33   300 changed       ask  market-limit     50000000          78903                                 1.378583e+04
#75545     78912 39983614 2014-10-06 06:27:24.704 2014-10-06 06:27:24   300 created       bid        market            0             NA                                 3.984000e-01
#75549     78913 39983614 2014-10-06 06:27:25.838 2014-10-06 06:27:24   300 deleted       bid        market     39840000          23756                                 0.000000e+00


#18236     21339 39957185 2014-10-06 01:01:33.845 2014-10-06 01:01:33   300 2473273289440       ask created 2473273289440
#83671     24127 39957185 2014-10-06 07:09:45.476 2014-10-06 01:01:33   300  -24327946757       ask deleted             0

#19990     25659 39957825 2014-10-06 01:03:38.134 2014-10-06 01:03:37   300   20871273804       ask created   20871273804
#20255     25660 39957825 2014-10-06 01:04:01.131 2014-10-06 01:03:37   300  -20871273804       ask deleted             0

#26731     31970 39960712 2014-10-06 01:22:37.905 2014-10-06 01:22:37   300    1216000000       ask created    1216000000
#26882     31971 39960712 2014-10-06 01:23:20.835 2014-10-06 01:22:37   300   -1216000000       ask deleted             0

#27791     32987 39961154 2014-10-06 01:27:09.298 2014-10-06 01:27:08   300      26300000       ask created      26300000
#27850     32988 39961154 2014-10-06 01:27:24.792 2014-10-06 01:27:08   300     -26300000       ask deleted             0

#29022     34184 39961729 2014-10-06 01:34:42.457 2014-10-06 01:34:42   300    1265000000       ask created    1265000000
#83673     34185 39961729 2014-10-06 07:09:45.539 2014-10-06 01:34:42   300   -1265000000       ask deleted             0

#30294     35413 39962345 2014-10-06 01:42:34.961 2014-10-06 01:42:34   300      36000000       ask created      36000000
#39591     35414 39962345 2014-10-06 02:42:28.480 2014-10-06 01:42:34   300     -36000000       ask deleted             0

#30847     35983 39962621 2014-10-06 01:45:18.242 2014-10-06 01:45:17   300     370000000       ask created     370000000
#30976     35984 39962621 2014-10-06 01:46:01.729 2014-10-06 01:45:17   300    -370000000       ask deleted             0

#39621     44177 39966673 2014-10-06 02:42:35.443 2014-10-06 02:42:35   300      36000000       ask created      36000000
#47889     44178 39966673 2014-10-06 03:42:33.259 2014-10-06 02:42:35   300     -36000000       ask deleted             0

#42061     46493 39967832 2014-10-06 02:56:56.963 2014-10-06 02:56:56   300       2467438       ask created       2467438
#42141     46494 39967832 2014-10-06 02:57:18.759 2014-10-06 02:56:56   300      -2467438       ask deleted             0

#47924     52219 39970627 2014-10-06 03:42:40.811 2014-10-06 03:42:40   300      36000000       ask created      36000000
#57679     52220 39970627 2014-10-06 04:42:27.233 2014-10-06 03:42:40   300     -36000000       ask deleted             0

#48562     52836 39970935 2014-10-06 03:47:31.604 2014-10-06 03:47:31   300    3253000000       ask created    3253000000
#48591     52837 39970935 2014-10-06 03:47:39.423 2014-10-06 03:47:31   300   -3253000000       ask deleted             0

#49520     53760 39971367 2014-10-06 03:52:57.536 2014-10-06 03:52:57   300     300000000       ask created     300000000
#49837     53761 39971367 2014-10-06 03:54:32.108 2014-10-06 03:52:57   300    -300000000       ask deleted             0

#53519     57441 39973203 2014-10-06 04:18:00.970 2014-10-06 04:18:00   300     186000000       ask created     186000000
#83675     57442 39973203 2014-10-06 07:09:45.592 2014-10-06 04:18:00   300    -186000000       ask deleted             0

#65782     69523 39978960 2014-10-06 05:31:59.520 2014-10-06 05:31:59   300      20000000       ask created      20000000
#67332     69524 39978960 2014-10-06 05:40:55.109 2014-10-06 05:31:59   300     -20000000       ask deleted             0

#67648     71308 39979846 2014-10-06 05:42:39.457 2014-10-06 05:42:39   300      36000000       ask created      36000000
#78150     71309 39979846 2014-10-06 06:42:27.088 2014-10-06 05:42:39   300     -36000000       ask deleted             0

#68233     71907 39980141 2014-10-06 05:45:51.413 2014-10-06 05:45:51   300     155000000       ask created     155000000
#68245     71908 39980141 2014-10-06 05:45:55.345 2014-10-06 05:45:51   300    -155000000       ask deleted             0

#74193     77600 39982962 2014-10-06 06:20:21.529 2014-10-06 06:20:21   300      20000000       ask created      20000000
#79292     77601 39982962 2014-10-06 06:48:58.792 2014-10-06 06:20:21   300     -20000000       ask deleted             0

#78198     81466 39984867 2014-10-06 06:42:37.464 2014-10-06 06:42:37   300      36000000       ask created      36000000
#83676     81467 39984867 2014-10-06 07:09:45.660 2014-10-06 06:42:37   300     -36000000       ask deleted             0

#79614     82862 39985567 2014-10-06 06:50:44.648 2014-10-06 06:50:44   300      20000000       ask created      20000000
#83678     82863 39985567 2014-10-06 07:09:45.717 2014-10-06 06:50:44   300     -20000000       ask deleted             0



