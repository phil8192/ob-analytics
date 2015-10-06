# obAnalytics
[![Build Status](https://travis-ci.org/phil8192/ob-analytics.svg?branch=master)](https://travis-ci.org/phil8192/ob-analytics) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

Limit Order Book event processing and visualisation.

## Installation

```R
# install.packages("devtools")
devtools::install_github("phil8192/ob-analytics")
```

## Documentation

Example use documentation has been created in R Markdown (see vignettes) 
directory. [knitr](https://github.com/yihui/knitr) is used to generate 
vignettes. [roxygen2](https://github.com/klutometis/roxygen) is used to 
generate the pdf manual from code comments.

### Example use of obAnalytics package (html) 
http://parasec.net/transmission/ob-analytics/guide.html

### Example use (pdf)
http://parasec.net/transmission/ob-analytics/guide.pdf

### Manual 
http://parasec.net/transmission/ob-analytics/obAnalytics-manual.pdf

## Development notes

#### Building and checking src
```bash
cd ob-analytics
R -e 'devtools::document()' # roxygen2
cd ..
R CMD build ob-analytics
R CMD check --as-cran obAnalytics_0.1.0.tar.gz 
```

