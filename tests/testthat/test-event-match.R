context("event matching")

asTime <- function(s) as.POSIXct(s, tz="UTC")

test_that("simple event matching works", {
  events <- data.frame(timestamp=c(asTime("2015-10-10 21:32:00.000"),
                                   asTime("2015-10-10 21:32:00.010"),
                                   asTime("2015-10-10 21:32:10.000"),
                                   asTime("2015-10-10 21:32:10.010")),
                       direction=c("bid", "ask", "bid", "ask"),
                       event.id=c(1, 2, 3, 4),
                       fill=rep(1234, 4))
  matched <- eventMatch(events, cut.off.ms=1000)
  expect_that(matched$matching.event, equals(c(2, 1, 4, 3)))
})

test_that("left over event matching works", {
  events <- data.frame(timestamp=c(asTime("2015-10-10 21:32:00.000"), 
                                   asTime("2015-10-10 21:32:00.010"),
                                   asTime("2015-10-10 21:32:10.000"),
                                   asTime("2015-10-10 21:32:10.010"),
                                   asTime("2015-10-10 21:33:00.000")), 
                       direction=c("bid", "ask", "bid", "ask", "ask"),
                       event.id=c(1, 2, 3, 4, 5),
                       fill=rep(1234, 5))
  matched <- eventMatch(events, cut.off.ms=1000)
  expect_that(matched$matching.event, equals(c(2, 1, 4, 3, NA)))
})

test_that("conflicting event matching works", {
  events <- data.frame(timestamp=c(asTime("2015-10-10 21:32:00.000"), 
                                   asTime("2015-10-10 21:32:00.010"),
                                   asTime("2015-10-10 21:32:10.000"),
                                   asTime("2015-10-10 21:32:10.080"),
                                   asTime("2015-10-10 21:32:10.090")), 
                       direction=c("bid", "ask", "bid", "bid", "ask"),
                       event.id=c(1, 2, 3, 4, 5),
                       fill=rep(1234, 5))
  matched <- eventMatch(events, cut.off.ms=1000)
  expect_that(matched$matching.event, equals(c(2, 1, NA, 5, 4)))

  events <- data.frame(timestamp=c(asTime("2015-10-10 21:32:00.000"), 
                                   asTime("2015-10-10 21:32:00.010"),
                                   asTime("2015-10-10 21:32:01.000"),
                                   asTime("2015-10-10 21:32:01.010"),
                                   asTime("2015-10-10 21:32:01.090")), 
                       direction=c("bid", "ask", "ask", "bid", "bid"),
                       event.id=c(1, 2, 3, 4, 5),
                       fill=rep(1234, 5))
  matched <- eventMatch(events, cut.off.ms=1000)
  expect_that(matched$matching.event, equals(c(2, 1, 4, 3, NA)))
})
