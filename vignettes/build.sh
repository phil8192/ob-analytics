#!/bin/bash
R -e 'rmarkdown::render("guide.Rmd","html_document",output_dir="/tmp")'

