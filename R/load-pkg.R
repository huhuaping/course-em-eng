require(bookdown)
library("tidyverse")
library("openxlsx")
library("knitr")
require(here)
library(glue)
library("rmarkdown")
require("pagedown")
library("xaringan")
library("xaringanExtra")
#library("kableExtra")
library("ggplot2")
library("DT")
library("latex2exp")

# tidy
require(lubridate)
require(mgsub)
require(broom)

# github repo
#renv::install("jirilukavsky/pdf2pptx")
require(pdf2pptx)
#renv::install("KWB-R/kwb.utils")
#renv::install("huhuaping/xmerit")
require(kwb.utils)
require(xmerit)
#renv::install("jhelvy/xaringanBuilder")
#require(xaringanBuilder)

# econometrics test
library(lmtest)
library(sandwich)
library(systemfit)
library(gmm)

library("car")
library("pls")

# time series
library("lmtest")
library("foreign")

# dummy model
library("fastDummies")
library( "psych")

# endogenous x
library('forcats')
library("survival")

# text data set
library("wooldridge")
library("AER")

# public
require(bibtex)
#webshot::install_phantomjs(force = TRUE)
#webshot::is_phantomjs_installed()
library(webshot2)
library(htmlwidgets)
