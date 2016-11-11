# obAnalytics
[![Build Status](https://travis-ci.org/phil8192/ob-analytics.svg?branch=master)](https://travis-ci.org/phil8192/ob-analytics) 
[![Coverage Status](https://img.shields.io/codecov/c/github/phil8192/ob-analytics/master.svg)](https://codecov.io/github/phil8192/ob-analytics?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/obAnalytics)](https://cran.r-project.org/package=obAnalytics) 
[![Downloads](http://cranlogs.r-pkg.org/badges/obAnalytics?color=brightgreen)](http://www.r-pkg.org/pkg/obAnalytics)

Limit Order Book event processing and visualisation.

!["limit order book analytics"](https://raw.githubusercontent.com/phil8192/ob-analytics/master/ob-analytics.png "limit order book analytics") 

__obAnalytics__ is an R package intended for visualisation and analysis of limit
order data. The package is experimental and is based on the R code used to
create the visualisations in this [Limit Order Book Visualisation](http://parasec.net/transmission/order-book-visualisation/) 
article.

## Installation

### CRAN

```R
install.packages("obAnalytics")
```

### Github

```R
if(!require("devtools")) install.packages("devtools")
devtools::install_github("phil8192/ob-analytics")
```

## Environment settings
Due to the large number of columns in the example data, it is recommended to set 
the display width to make the most use of the display. It is also recommended to 
set digits.secs=3 and scipen=999 in order to display timestamps and fractions 
nicely. This can be achieved as follows:

```R
max.cols <- Sys.getenv("COLUMNS")
options(width=if(max.cols != "") max.cols else 80, scipen=999, digits.secs=3)
```

## Example use
Preprocessed limit order data from the inst/extdata directory has been included
in the package. The data, taken from a Bitcoin exchange on 2015-05-01, consists 
of 50,393 limit order events and 482 trades occuring from midnight up until 
~5am. To use the data, attach it to the environment:

```R
library(obAnalytics)
data(lob.data)
```

The lob.data data structure contains 4 data.frames describing limit order 
events, trades, depth and summary statistics. All of which are described in 
detail in the package documentation. To visualise all of the example order book
data, use the plotPriceLevels function:

```R
with(lob.data, {
  spread <- getSpread(depth.summary)
  plotPriceLevels(depth, spread, volume.scale=10^-8, show.mp=T)
})
```

## Web app
An interactive interface making use of this package is available in the
[shiny-ob-analytics](https://github.com/phil8192/shiny-ob-analytics) respository.

## Documentation

Example use documentation has been created in R Markdown (see vignettes) 
directory. [knitr](https://github.com/yihui/knitr) is used to generate 
vignettes. [roxygen2](https://github.com/klutometis/roxygen) is used to 
generate the pdf manual from code comments.

### Example use of obAnalytics package (html) 
An end-to-end walk-through to demonstrate the main features and functionality of 
the package is available here:
http://parasec.net/transmission/ob-analytics/guide.html

### Example use (pdf)
http://parasec.net/transmission/ob-analytics/guide.pdf

### Manual 
In addition to online ?help, package data and function documentation is 
available in the form of a manual: 
http://parasec.net/transmission/ob-analytics/obAnalytics-manual.pdf

## License

GPL (>= 2)

