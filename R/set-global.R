
# global options for R
options(
  htmltools.dir.version = FALSE, 
  formatR.indent = 2, width = 55, 
  digits = 2,tinytex.verbose = TRUE,
  knitr.kable.NA = '',
  echo=FALSE, warning=FALSE, message=FALSE)

# global options for knitr
knitr::opts_chunk$set(comment = "",fig.align='center',echo = FALSE,message = FALSE,
                      fig.width=11, fig.height=6) # Places figures on their own pages

# global options for DT
options(DT.options = list(dom ="t" ,  # pure table with no search blank
                          columnDefs = list(
                            list(className = "dt-center", targets = "_all"), # align center
                            list(visible=FALSE,targets=0) # hide index column
                            )
                          )
        )

# global options for servr pkg

options(servr.interval = 0.5) # control time to refresh the preview
options(servr.daemon = TRUE) # unlock thread when infinite moon render


# pkg needed
require('servr')
library('bookdown')
library('knitr')
#library('xlsx')
library("openxlsx")
#install.packages('tidyr')
#install.packages('plyr')
library('tidyr')
library('dplyr')
library("tibble")
library('stringr')
library('tidyverse')
library('ggplot2')
library("scales")
#install.packages("gridExtra")
library("gridExtra")
#install.packages("magrittr")
library("magrittr")
#install.packages("ggthemes")
#install.packages("ggrepel")
require("ggthemes")
require("ggrepel")
require("lubridate")
require("DT")
require("wooldridge")
require("png")
require("jpeg")
require("kableExtra")
require("xmerit")
