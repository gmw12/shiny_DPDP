cat(file = stderr(), "load R Libraries", "\n")

testit <- function() {
  message("testing package startup messages")
  packageStartupMessage("initializing ...", appendLF = FALSE)
  Sys.sleep(1)
  packageStartupMessage(" done")
}

suppressPackageStartupMessages(testit())

#shiny
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
#library(shinybrowser)
#library(shinydashboardPlus)
library(shinyFiles)
library(shinyalert)

#basic
library(tidyverse)
library(data.table)
library(fs)
library(miscTools)
library(stringr)

#read write
library(readxl)
library(openxlsx)

#tables
library(RSQLite)
library(rhandsontable)
library(DT)

#parallel and background processing
library(callr)

#graphics
library(gplots)
library(rgl)
library(colourpicker)
library(randomcoloR)



#not updated on CRAN anymore
#test_pca3d <- require(pca3d)
if (!require(pca3d)) {
  cat(file = stderr(), "Package pca3d not found, adding from project directory", "\n")
  library("pca3d", lib.loc = str_c(getwd(), '/Packages/'))
}
