# read map prediction
# Predicts the percentage of reads that map to host and pathogen
# Felicia Burtscher
# Date: June 6, 2018

# global.R

###############################################################################
#                         LOAD PACKAGES AND MODULES                          #
###############################################################################
library(shiny)
library(shinythemes)
library(shinyalert)
# library(grid)
# library(markdown)
library(ggplot2)
library(ggExtra)
# library(shinyBS)
# library(qtl)
library(rmarkdown)
# library(markdown)
library(plotly)
library(knitr)
library(lmtest)
# library(scatterplot3d)
library(png)
# library(raster)

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

# # TO RUN IN ON MSC SERVER
# source('ui.R', local = TRUE)
# source('server.R', local = TRUE)

load(file = "Rdata/fit_nona_paras.rda")
load(file = "Rdata/dat_nona.rda")
load(file = "Rdata/dat_nc_nona.rda")
load(file = "Rdata/fit_nona_paras.rda")
load(file = "Rdata/fit_nona_paras_dens.rda")
load(file = "Rdata/dummy.rda")

load(file = "Rdata/glm_paras_logit.rda")
load(file = "Rdata/glm_paras_dens_logit.rda")
load(file = "Rdata/glm_total_logit.rda")
load(file = "Rdata/glm_total_dens_logit.rda")

load(file = "Rdata/outcome_prop.rda")
load(file = "Rdata/outcome_prop_nc_nona.rda")
load(file = "Rdata/outcome_prop_nona.rda")

load(file = "Rdata/references.rda")
 
# # Read model data
# modelData <- read.xlsx("models.xlsx", sheetIndex = 1, header = TRUE)

# # File containing unique geo codes, state,city, zip
# dummy <- read.csv("Rdata/dummy.csv", header = TRUE)

# #Set directory back to project home directory
# setwd(home)

# ################################################################################
# #                             FUNCTIONS                                       #
# ################################################################################

enableBookmarking(store = "url")

# ggplotRegression <- function (fit, constant) {  
#   require(ggplot2)  
#   ggplot(fit$model, 
#     aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     scale_x_continuous(name="", limits=c(0,100), breaks=NULL) + 
#     scale_y_continuous(limits=c(0,1)) + 
#     scale_size_identity() +
#     geom_point() +
#     # xlim(0,100) +
#     geom_smooth(method = "lm", fullrange=TRUE) +
#     
#     # labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
#     #                    "; Intercept =",signif(fit$coef[[1]],5 ),
#     #                    "; Slope =",signif(fit$coef[[2]], 5),
#     #                    "; P-value =",signif(summary(fit)$coef[2,4], 5))) + 
#     
#     geom_abline(intercept = constant, slope = 0)
# }

ggplotRegression <- function (fit, constant, limit, ptype) {  
  require(ggplot2)  
  if(ptype == "ppercentage" || ptype == "ppercentage2"){
    xname = "Percentage of parasitemia"
  } else if(ptype == "pdensity" || ptype == "pdensity2"){
    xname = "Parasitemia density [1/µl]"
  }
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    scale_x_continuous(name=xname, limits=c(0,limit)) +  ## -- with it pdenstiy does not show up
    scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) +
    # scale_size_identity() +
    geom_point(shape=1) +
    stat_smooth(method = "lm", fullrange=TRUE, se=TRUE) +
    labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "; Intercept =",signif(fit$coef[[1]],5 ),
                       "; \n Slope =",signif(fit$coef[[2]], 5),
                       "; P-value (F-test) =",signif(summary(fit)$coef[2,4], 5))) +
    # annotate("text", x=0.1, y=-0.05, label = "R^2 == 0.78", parse=T) +
    # annotate("text", x=0.1, y=-0.06, label = "alpha == 0.00", parse=T) +
    # annotate("text", x=0.1, y=-0.07, label = "beta == 0.67", parse=T) +
    geom_abline(intercept = constant, slope = 0) 
}

# # DEFINE NICE REGRESSION PLOT FUNCTION FOR COMPLEX MODEL, ie second variable
# ggplotRegression2 <- function (fit, constant, limit, ptype) {  
#   require(ggplot2)  
#   if(ptype == "ppercentage")
#   {
#     xname = "Percentage of parasitemia"
#   }
#   else if(ptype == "pdensity"){
#     xname = "Parasitemia density [1/µl]"
#   } 
#   ggplot(fit$model, aes_string(x = names(fit$model)[3], y = names(fit$model)[1])) + 
#     scale_x_continuous(name=xname, limits=c(0,limit)) +  ## -- with it pdenstiy does not show up
#     scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) + 
#     geom_point() +
#     stat_smooth(method = "lm", col = "blue", fullrange=TRUE) +
#     labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "; Intercept =",signif(fit$coef[[1]],5 ),
#                        "; \n Slope =",signif(fit$coef[[2]], 5),
#                        "; Slope2 =",signif(fit$coef[[3]], 5),
#                        "; P-value (F-test) =",signif(summary(fit)$coef[2,4], 5))) +
#     geom_abline(intercept = constant, slope = 0)
# }

# ggplotRegression2 <- function(fit){
#   ggplot(mtcars, aes(x=wt, y=mpg)) + 
#   geom_point()+
#   geom_smooth(method=lm)
# }

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      tags$i(class="icon-question-sign")
    )
  )
}

# # TO RUN IN ON MSC SERVER
# app <- shinyApp(ui = ui, server = server)
# runApp(app, port=20199, launch.browser = FALSE, host = getOption("shiny.host", "0.0.0.0"))
