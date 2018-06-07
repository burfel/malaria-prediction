# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# global.R

###############################################################################
#                         LOAD PACKAGES AND MODULES                          #
###############################################################################
library(shiny)
library(ggplot2)
library(shinydashboard)
library(grid)
library(markdown)
library(ggExtra)
library(shinyBS)
library(qtl)
library(rmarkdown)

# ################################################################################
# #                             GLOBAL VARIABLES                                 #
# ################################################################################
# #Default  Values
# dflt <- list(state = "", county = "", city = "", zip = "", model = "ARIMA", 
#              split = as.integer(2014), maxValue = as.integer(1000000), stringsAsFactors = FALSE)
# 
# ###############################################################################
# #                               LOAD DATA                                     #
# ###############################################################################
# #Delete the following line before deploying this to shiny.io
# home <- getwd()
# 
# setwd("processedData")
# 
# currentZip    = read.csv("currentZip.csv", header = TRUE, stringsAsFactors = FALSE)
# currentCity   = read.csv("currentCity.csv", header = TRUE, stringsAsFactors = FALSE)
# currentCounty = read.csv("currentCounty.csv", header = TRUE, stringsAsFactors = FALSE)
# currentState  = read.csv("currentState.csv", header = TRUE, stringsAsFactors = FALSE)
# hviAllZip     = read.csv("hviAllZip.csv", header = TRUE, stringsAsFactors = FALSE)
# hviAllCity    = read.csv("hviAllCity.csv", header = TRUE, stringsAsFactors = FALSE)
# hviAllCounty  = read.csv("hviAllCounty.csv", header = TRUE, stringsAsFactors = FALSE)
# hviAllState   = read.csv("hviAllState.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# # Read model data
# modelData <- read.xlsx("models.xlsx", sheetIndex = 1, header = TRUE)
# 
# # File containing unique geo codes, state,city, zip
# geo <- read.csv("geo.csv", header = TRUE)
# 
# #Set directory back to project home directory
# setwd(home)