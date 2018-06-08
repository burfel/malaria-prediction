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
library(shinythemes)
# library(grid)
library(markdown)
library(ggExtra)
# library(shinyBS)
# library(qtl)
library(rmarkdown)
# library(markdown)
library(plotly)
#library(Cairo)

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

#source('../src/databuilder.R', local = TRUE)
load(file = "Rdata/fit_nona_paras.rda")
load(file = "Rdata/dat_nona.rda")
load(file = "Rdata/fit_nona_paras.rda")
load(file = "Rdata/fit_nona_paras_dens.rda")
load(file = "Rdata/dummy.rda")
 
# # Read model data
# modelData <- read.xlsx("models.xlsx", sheetIndex = 1, header = TRUE)

# # File containing unique geo codes, state,city, zip
# dummy <- read.csv("Rdata/dummy.csv", header = TRUE)

# #Set directory back to project home directory
# setwd(home)

# ################################################################################
# #                             FUNCTIONS                                       #
# ################################################################################

ggplotRegression <- function (fit, constant) {  
  require(ggplot2)  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    # labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
    #                    "; Intercept =",signif(fit$coef[[1]],5 ),
    #                    "; Slope =",signif(fit$coef[[2]], 5),
    #                    "; P-value =",signif(summary(fit)$coef[2,4], 5))) + 
    geom_abline(intercept = constant, slope = 0)
}

enableBookmarking(store = "url")
