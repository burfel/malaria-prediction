library(caret)
library(klaR)
library(doParallel)
library(tidyverse)
registerDoParallel(detectCores() - 1)

#setwd("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/src")


#===============================================================================
#                               READ IN DATA                                   #
#===============================================================================
# read in files
hg_pf <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/hg_pf_readcounts.csv")
supp <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/Supplementary_Dataset.csv", header=TRUE)
# dummy <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp4/Rdata/dummy.csv", header=TRUE)
# summary_simple <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp4/Rdata/summary_simple.csv", header=TRUE)
# summary_simple_dens <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp4/Rdata/summary_simple_dens.csv", header=TRUE)
# summary_complex <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp4/Rdata/summary_complex.csv", header=TRUE)
# summary_complex_dens <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp4/Rdata/summary_complex_dens.csv", header=TRUE)

# glossary <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/GITHUB/shinyapp4/Rdata/glossary.csv", header=TRUE)
# glossary <- glossary[,1:2]
# sub("[.]", " ", glossary) 
# glossary

# merge data sets (by samples/ subjectID)
dat <- merge(supp, hg_pf, by.y = "samples", by.x = "Subject.ID")

library(car)
# add column "outcome" which gives the proportion of pathogen reads
dat$total_reads <- dat$pf_count + dat$hg_count
dat$outcome <- dat$pf_count / (dat$hg_count + dat$pf_count)
dat$outcome.logit <-  logit(dat$outcome, percents=TRUE)
outcome_prop <- cbind(dat$pf_count, dat$hg_count)

# most important non-categorical variables 
#dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L, outcome))
#dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Parasite.density...µl., Total.White.Cell.Count..x109.L., Percentage.lymphocytes, Percentage.monocytes, Percentage.neutrophils, outcome))
#dat.nc.logit <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Parasite.density...µl., Total.White.Cell.Count..x109.L., Percentage.lymphocytes, Percentage.monocytes, Percentage.neutrophils, outcome.logit))
dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Parasite.density...µl., Total.White.Cell.Count..x109.L., Lymphocyte.count...x109.L., Monocyte.count...x109.L., Neutrophil.count...x109.L., Percentage.lymphocytes, Percentage.monocytes, Percentage.neutrophils, hg_count, pf_count, outcome))
dat.nc.logit <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Parasite.density...µl., Total.White.Cell.Count..x109.L., Lymphocyte.count...x109.L., Monocyte.count...x109.L., Neutrophil.count...x109.L., Percentage.lymphocytes, Percentage.monocytes, Percentage.neutrophils, hg_count, pf_count, outcome.logit))

# drop the samples that have blanks
dat.nona <- na.omit(dat) # 21x27 matrix
dat.nc.nona <- na.omit(dat.nc) # 40x13 matrix (added different white cell type counts, hg_count, pf_count)
dat.nc.nona.logit <- na.omit(dat.nc.logit) # 40x13 matrix
outcome_prop.nona <- cbind(dat.nona$pf_count, dat.nona$hg_count) # 21x2 matrix
outcome_prop.nc.nona <- cbind(dat.nc.nona$pf_count, dat.nc.nona$hg_count) # 40x2 matrix !!!!!!!!!!!!!

#===============================================================================
#                      PLOTS to explore relationships                          #
#===============================================================================
# PLOT PROPORTION OF PATHOGEN READS, BOTH AS NORMAL AND A LOG SCALE -- FOR WEBSITE
par(mfrow = c(1, 2))
#png("img/total_reads_outcome.png")
sp <- ggplot(dat, aes(total_reads, outcome)) + 
  geom_point() +
  geom_text(label=rownames(dat)) 
# Scatter plot with the 2d density estimation
sp + 
  # geom_density_2d() +
     labs(x = "Total number of reads", y = "Percentage of reads that map to pathogen")
#sp + stat_ellipse()
#sp + geom_bin2d()
#dev.off()

#png("img/total_reads_outcome_logit.png")
sl <- ggplot(dat, aes(total_reads, outcome.logit)) + 
  geom_point() +
  geom_text(label=rownames(dat)) 
sl + geom_density_2d() +
     labs(x = "Total number of reads", y = "logit(Percentage of reads that map to pathogen)")
sl + stat_ellipse()
sl + geom_bin2d()
#dev.off()
#plot(dat$total_reads, dat$outcome, xlab = "Total number of reads", ylab = "Proportion reads that map to pathogen")
#ggplot(log(dat$total_reads), dat$outcome, xlab = "Total number of reads", ylab = "Proportion reads that map to pathogen")


########

par(mfrow = c(1, 2))

png("img/parasitemia_outcome.png")
al <- ggplot(dat.nona, aes(Percentage.parasitemia, outcome)) + 
  geom_point() +
  geom_text(label=rownames(dat)) 
# Scatter plot with the 2d density estimation
al + geom_density_2d() +
  labs(x = "Percentage of parasitemia", y = "Percentage of reads mapping to pathogen")
#sp + stat_ellipse()
#sp + geom_bin2d()
#dev.off()
  
as <- ggplot(dat.nona, aes(Percentage.parasitemia, outcome.logit)) + 
    geom_point() +
    geom_text(label=rownames(dat)) 
  # Scatter plot with the 2d density estimation
  as + geom_density_2d() +
  labs(x = "Percentage of parasitemia", y = "logit(Percentage of reads mapping to pathogen)")
  #sp + stat_ellipse()
  #sp + geom_bin2d()
  dev.off()

#######

# PLOTS: IS RESPONSE VARIABLE CLOSE TO NORMALITY?
library(ggpubr)
#png("img/pathogen_read_density.png")
ggdensity(dat$outcome, 
          main = "Density plot of pathogen reads",
          xlab = "Percentage of reads that map to pathogen",
          ylab = "Density/ quantity of reads")
#dev.off()
# ggdensity(dat.nona$Percentage.parasitemia, 
#           main = "Density plot of Percentage of parasitemia",
#           xlab = "Percentage of parasitemia")
# ggdensity(dat.nona$Total.White.Cell.Count..x109.L., 
#           main = "Density plot of total white cell count",
#           xlab = "total white cell count")


# PLOT --- FOR WEBSITE
library(e1071)
#png("img/pathogen_read_density.png")
# plot(density(dat$outcome), main="Percentage of reads that map to pathogen", ylab="Density" #, sub=paste("Skewness:", round(e1071::skewness(dat$outcome), 2))
# )  # density plot for 'speed'
plot(density(dat$outcome), main="Distribution of samples whose reads map \n to the host with a certain percentage", ylab="Density of samples", xlab = "Percentage of reads that map to pathogen" #, sub=paste("Skewness:", round(e1071::skewness(dat$outcome), 2))
) 
polygon(density(dat$outcome), col="orange")
#dev.off()

#===============================================================================
#                      CORRELATIONS                                            #
#===============================================================================
# CORRELATION MATRIX
library(corrplot)
## corrplot 0.84 loaded
par(mfrow = c(1, 1))
my_matrix_nona <- dat.nona[,-c(1,2,16,23,24)]
my_matrix_nc <- dat.nc.nona[,-c(1,2,11,12)]
my_matrix <- dat[,-c(1,2,16,23,24)]
M_nona <- cor(my_matrix_nona, method="pearson")
M_nc <- cor(my_matrix_nc, method="pearson")
M <- cor(my_matrix, method="pearson")
#corrplot(M, method = "square") ### PLOT FOR WEBSITE
#corrplot(M, order = "AOE") 
#corrplot(M, order = "hclust") # FOR WEBSITE
#corrplot(M, order = "FPC")
#corrplot(M, order = "alphabet")
#png("shinyapp2/img/correlation.png")
corrplot(M_nona, order = "hclust", addrect = 2) # !!!!!!!!!
corrplot(M_nc, order = "hclust", addrect = 2) # !!!!!!!!!
corrplot(M, order = "hclust", addrect = 2) # !!!!!!!!!
#dev.off()


#===============================================================================
#                      SCALE, CENTER VARIABLES                                 #
#===============================================================================

dat$Parasite.density...µl. <- dat$Parasite.density...µl./1000000
dat.nona$Parasite.density...µl. <-dat.nona$Parasite.density...µl./1000000
dat.nc$Parasite.density...µl. <- dat.nc$Parasite.density...µl./1000000
dat.nc.nona$Parasite.density...µl. <- dat.nc.nona$Parasite.density...µl./1000000
dat.nc.logit$Parasite.density...µl. <- dat.nc$Parasite.density...µl./1000000
dat.nc.nona.logit$Parasite.density...µl. <- dat.nc.nona.logit$Parasite.density...µl./1000000
# maybe scale by /100,000?

#===============================================================================
#                      EXCLUDE OUTLIERS 11, 10, 19,                            #
#===============================================================================

#===============================================================================
#                      SIMPLE REGRESSION MODELS                                #
#===============================================================================
# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITHOUT TRANSFORMATION, ON WHOLE DATASET
set.seed(1800)
fit.paras <- lm(dat$outcome ~ dat$Percentage.parasitemia, data=dat)
summary(fit.paras) # # show results: R^2: 0.245, F-stats: 15.28, p-value: 0.0003238 ***
# MODEL: 0.151312 + 0.010838*dat.nona$Percentage.parasitemia

#summary(fit.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
par(mfrow = c(2, 2))  
plot(fit.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1)) 
hist(fit.paras$res, main="Residuals") # residuals not really Gaussian


# FOR WEBSITE --- ALSO PLOT
# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITHOUT TRANSFORMATION, ON COMPLETE SAMPLES (on chosen variables)
set.seed(1800)
fit.nona.paras <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, data=dat.nona)
fit.nc.nona.paras <- lm(dat.nc.nona$outcome ~ dat.nc.nona$Percentage.parasitemia, data=dat.nc.nona)

summary(fit.nona.paras) # # show results: R^2: 0.3767, F-stats: 13.09, p-value: 0.001834 **
# MODEL: 0.090267 + 0.013339*dat.nona$Percentage.parasitemia

summary(fit.nc.nona.paras) # # show results: R^2: 0.2569, F-stats: 14.49, p-value: 0.0004999 ***
# MODEL: 0.151629 + 0.009935*dat.nc.nona$Percentage.parasitemia

#summary(fit.nona.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
# png("../shinyapp4/img/fit_nona_paras.png")
par(mfrow = c(2, 2))  
plot(fit.nona.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
# dev.off()
hist(fit.nona.paras$res,main="Residuals") # residuals not really Gaussian
# png("../shinyapp4/img/fit_nc_nona_paras.png")
par(mfrow = c(2, 2))  
plot(fit.nc.nona.paras)
# dev.off()


#===============================================================================
#                      SAVE MODELS                                             #
#===============================================================================
# # save references R object
# save(references, file = "../shinyapp4/Rdata/references.rda")
# save(glossary, file = "../shinyapp4/Rdata/glossary.rda")
#
# # data files
# save(dat, file = "../shinyapp4/Rdata/dat.rda")
# save(dat.nc, file = "../shinyapp4/Rdata/dat_nc.rda")
# save(dat.nc.logit, file = "../shinyapp4/Rdata/dat_nc_logit.rda")
#
# save(dat.nona, file = "../shinyapp4/Rdata/dat_nona.rda")
# # dat.nona.logit ?
# save(dat.nc.nona, file = "../shinyapp4/Rdata/dat_nc_nona.rda")
# save(dat.nc.nona.log, file = "../shinyapp4/Rdata/dat_nc_nona_log.rda")
#
# # linear models: simple models
# save(fit.nona.paras, file = "../shinyapp4/Rdata/fit_nona_paras.rda")
# save(fit.nona.paras.dens.new, file = "../shinyapp4/Rdata/fit_nona_paras_dens_new.rda")
# save(fit.nona.paras.log, file = "../shinyapp4/Rdata/fit_nona_paras_log.rda")
# save(fit.nona.paras.dens.log, file = "../shinyapp4/Rdata/fit_nona_paras_dens_log.rda")
#
# save(fit.nc.nona.paras, file = "../shinyapp4/Rdata/fit_nc_nona_paras.rda")
# save(fit.nc.nona.paras.dens, file = "../shinyapp4/Rdata/fit_nc_nona_paras_dens.rda")
# save(fit.nc.nona.paras.log, file = "../shinyapp4/Rdata/fit_nc_nona_paras_log.rda")
# save(fit.nc.nona.paras.dens.log, file = "../shinyapp4/Rdata/fit_nc_nona_paras_dens_log.rda")
#
# # generalized linear models: simple models, complex models
# save(glm.paras.logit, file = "../shinyapp4/Rdata/glm_paras_logit.rda")
# save(glm.paras.dens.logit, file = "../shinyapp4/Rdata/glm_paras_dens_logit.rda")
# save(glm.total.logit, file = "../shinyapp4/Rdata/glm_total_logit.rda")
# save(glm.total.dens.logit, file = "../shinyapp4/Rdata/glm_total_dens_logit.rda")
#
# save(outcome_prop, file = "../shinyapp4/Rdata/outcome_prop.rda")
# save(outcome_prop.nc.nona, file = "../shinyapp4/Rdata/outcome_prop_nc_nona.rda")
# save(outcome_prop.nona, file = "../shinyapp4/Rdata/outcome_prop_nona.rda")

# save(dummy, file = "../shinyapp4/Rdata/dummy.rda")
# save(summary_simple, file = "../shinyapp4/Rdata/summary_simple.rda")
# save(summary_simple_dens, file = "../shinyapp4/Rdata/summary_simple_dens.rda")
# save(summary_complex, file = "../shinyapp4/Rdata/summary_complex.rda")
# save(summary_complex_dens, file = "../shinyapp4/Rdata/summary_complex_dens.rda")

#===============================================================================
#                      REGRESSION PLOTS                                        #
#===============================================================================
png("img/paras_outcome_logit.png")
ggplot(dat.nc.nona.logit, aes(Percentage.parasitemia, y=outcome.logit)) +
  geom_point() + 
  geom_text(label=rownames(dat.nc.nona)) + 
  scale_x_continuous(name="Percentage of parasitemia", limits=c(0,50)) +  ## -- with it pdenstiy does not show up
  scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1))
dev.off()

# DEFINE NICE REGRESSION PLOT FUNCTION FOR SIMPLE MODEL
ggplotRegression <- function (fit) {  
  require(ggplot2)  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "; Intercept =",signif(fit$coef[[1]],5 ),
                       "; Slope =",signif(fit$coef[[2]], 5),
                       "; P-value =",signif(summary(fit)$coef[2,4], 5)))
}

# DEFINE NICE REGRESSION PLOT FUNCTION FOR COMPLEX MODEL, ie second variable
ggplotRegression2 <- function (fit) {  
  require(ggplot2)  
  ggplot(fit$model, aes_string(x = names(fit$model)[3], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(title = paste("Adj R^2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "; Intercept =",signif(fit$coef[[1]],5 ),
                       "; Slope1 =",signif(fit$coef[[2]], 5),
                       "; Slope2 =",signif(fit$coef[[3]], 5),
                       "; P-value =",signif(summary(fit)$coef[2,4], 5)))
}

#png("shinyapp2/img/fit_nona_paras_regression.png")
ggplotRegression(fit.nona.paras)
#dev.off()

# # not necessary for website
# library(gvlma)
# gvmodel <- gvlma(fit.nona.paras)
# summary(gvmodel)

#===============================================================================
#                      PLOTS ON ONE EXEMPLARY MODEL to test assumptions        #
#===============================================================================
## display first x entries of dataset
#library(dplyr)
#set.#seed(1234)
#dplyr::sample_n(dat, 10)
#
#library(MASS)
#summary(dat)

# (1i) plot of observed vs predicted values
# The points should be symmetrically distributed around a diagonal line, with a roughly constant variance.
# Here, the observed (dat$Head) on predicted (or fitted: m$fitted.values)
#png("shinyapp2/img/test1_fitted_outcome.png")
plot(dat.nona$outcome ~ fit.nona.paras$fitted)
# add 1:1 line
abline(0, 1) 
#dev.off()
#...not really the case.

# (1ii) Plot of residuals versus predicted values.
# The points should be symmetrically distributed around a horizontal line, with a roughly constant variance.
# Here, the residuals (m$residuals) on predicted (or fitted: m$fitted.values)
#png("shinyapp2/img/test2_fitted_residual.png")
plot(fit.nona.paras$residual ~ fit.nona.paras$fitted)
# add horizontal line at 0
abline(h = 0)
#dev.off()

# (1iii) Plots of the residuals versus individual independent variables.
#par(mfrow = c(1,2))
#png("shinyapp2/img/test3_residual_paras.png")
plot(fit.nona.paras$residual ~ dat.nona$Percentage.parasitemia)
#dev.off()
#plot(fit.nona.paras$residual ~ dat.nona$Total.White.Cell.Count..x109.L.)


# (2) Statistical independence of the errors (in particular, no correlation between consecutive errors in the case of time series data)
# This assumption is particularly important for data that are known to be autocorrelated (time series, spatial data). 
# However, it is possible that the predictor variables account for the autocorrelation.

# (2i) Plot of residuals versus time series/row number.
plot(fit.nona.paras$residuals)

# (2ii) Plot estimates of the autocorrelation function
#png("shinyapp2/img/test4_autocorrelation.png")
acf(fit.nona.paras$residuals) # residuals are not auto-correlated as expected since no time-series data
#dev.off()
# The X axis corresponds to the lags of the residual, increasing in steps of 1. 
# The very first line (to the left) shows the correlation of residual with itself (Lag0), 
# therefore, it will always be equal to 1.
# If the residuals were not autocorrelated, the correlation (Y-axis) from the immediate 
# next line onwards will drop to a near zero value below the dashed blue line (significance level).

# (2) Homoscedasticity (constant variance) of the errors
# Violations of constant variance make it hard to estimate the standard deviation of the coefficient estimates.
# (i) Plots residuals versus the predicted values.
plot(fit.nona.paras$residual ~ fit.nona.paras$fitted)
# (ii) Plot residuals versus independent variables.
par(mfrow = c(1,2))
plot(fit.nona.paras$residual ~ dat.nona$Percentage.parasitemia)
#plot(fit.nona.paras$residual ~ dat.nona$Total.White.Cell.Count..x109.L.)

# (3) Normality of the error distribution.
# Violations of normality create problems for determining whether model coefficients are significantly different from zero and for calculating confidence intervals. It is not required for estimating the coefficients.
# Outliers Parameter estimation is based on the minimization of squared error, thus a few extreme observations can exert a disproportionate influence on parameter estimates.
#png("shinyapp2/img/test5_hist_residuals.png")
hist(fit.nona.paras$residuals)
#dev.off()
# (i) Normal probability plot or normal quantile plot of the residuals.
# These are plots of the fractiles of error distribution versus the fractiles of a normal distribution with the same mean and variance.
qqnorm(fit.nona.paras$residuals)
qqline(fit.nona.paras$residuals)


#===============================================================================
#                      MORE PLOTS & STATISTICAL TESTS ON ASSUMPTIONS           #
#===============================================================================
## PLOT -- FOR WEBSITE
library(ggpubr)
# ggqqplot(fit.nona.paras$residuals, main = "Normal Q-Q",
#          xlab = "Theoretical Quantiles", ylab = "Standardized residuals")
library(car)
#png("shinyapp2/img/test6_qq_residuals.png")
qqPlot(fit.nona.paras$residuals, main = "Normal Q-Q",
       xlab = "Theoretical Quantiles", ylab = "Standardized residuals")
#dev.off()

# (ii) Outliers
#stripchart(dat$outcome~dat$Percentage.parasitemia, vertical = TRUE, method = 'jitter', col = c('darkgrey', 'red'), pch = 21)
# (iii) Statistical tests for normality
# Kolmogorov-Smirnov test
ks.test(fit.nona.paras$residuals, pnorm) # D = 0.42605, p-value = 0.0005636
# Shapiro-Wilk test
shapiro.test(fit.nona.paras$residuals) # not good! W = 0.8791, p-value = 0.01406
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly 
# different from normal distribution. In other words, we can assume the normality.

# data is not sufficiently inconsistent with a normal that you would reject the null
#qqnorm(dat$Percentage.parasitemia)

# library(MASS)
# vif(fit.nona.paras)

# test whether parasitemia percentage is normally distributed
# - visual inspection (q-q plot)
# - statistical test 

# SKEWNESS AND KURTOSIS response variable
library(moments)
skewness(dat.nona$outcome) # 1.071187
kurtosis(dat.nona$outcome) # 3.373337
#Histogram -- far from normally distributed
library(ggplot2)
datasim <- data.frame(dat.nona$outcome)
ggplot(datasim, aes(x = dat.nona$outcome), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + 
  ylab(expression(bold('Density')))


#===============================================================================
#                      SIMPLE REGRESSION MODELS CONT'D                         #
#===============================================================================
# (1) SIMPLEST MODEL (JUST PARASITEMIA), DENSITY, WITHOUT TRANSFORMATION, ON WHOLE DATASET
set.seed(1800)
fit.paras.dens <- lm(dat$outcome ~ dat$Parasite.density...µl., data=dat)

summary(fit.paras.dens) # # show results: R^2: 0.3673, F-stats: 25.38, p-value: 9.932e-06 ***
# MODEL: 0.1120166 + 0.0041995 * dat$Parasite.density...µl

#summary(fit.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
par(mfrow = c(2, 2))
plot(fit.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1))
#hist(fit.paras$res, main="Residuals") # residuals not really Gaussian


# (1) SIMPLEST MODEL (JUST PARASITEMIA), DENSITY, WITHOUT TRANSFORMATION, ON COMPLETE SAMPLES
set.seed(1800)
fit.nona.paras.dens <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl., data=dat.nona)
#dat.nona$Parasite.density...µl. <- dat.nona$Parasite.density...µl.*100000

summary(fit.nona.paras.dens) # # show results: R^2: 0.3812, F-stats: 13.32, p-value: 0.001703 **
# MODEL: 0.080372 + 0.004186 * dat.nona$Parasite.density...µl

#summary(fit.nona.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
#png("shinyapp2/img/fit_nona_paras_dens.png")
par(mfrow = c(2, 2))
plot(fit.nona.paras.dens) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#dev.off()
#par(mfrow = c(1, 1))
#hist(fit.nona.paras$res,main="Residuals") # residuals not really Gaussian

# #png("shinyapp2/img/fit_nona_paras_dens_regression.png")
# ggplotRegression(fit.nona.paras.dens)
# #dev.off()


fit.nc.nona.paras.dens <- lm(dat.nc.nona$outcome ~ dat.nc.nona$Parasite.density...µl., data=dat.nc.nona)
summary(fit.nc.nona.paras.dens) # # show results: R^2: 0.3541, F-stats: 22.38, p-value: 3.062e-05
# MODEL: 0.1155227 + 0.0037557 * dat.nc.nona$Parasite.density...µl


# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON WHOLE DATASET
set.seed(1800)
fit.paras.log <- lm(dat$outcome.logit ~ dat$Percentage.parasitemia, data=dat)

summary(fit.paras.log) # # show results: R^2: 0.1733, F-stats: 10.22, p-value: 0.0026 ***
# MODEL.logit: -6.93173 + 0.05786 * dat$Percentage.parasitemia

#summary(fit.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
par(mfrow = c(2, 2))
plot(fit.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1))
#hist(fit.paras.log$res,main="Residuals") # residuals not really Gaussian


# FOR WEBSITE --- ALSO PLOT!!!!!!!! -- to compare to non-logit transformation
# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON COMPLETE SAMPLES
set.seed(1800)
fit.nona.paras.log <- lm(dat.nona$outcome.logit ~ dat.nona$Percentage.parasitemia, data=dat.nona)

summary(fit.nona.paras.log) # # show results: R^2: 0.2065, F-stats: 6.205, p-value: 0.02215 ***
#summary(fit.nona.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# MODEL.logit: -7.50325 + 0.08221*dat.nona$Percentage.parasitemia

# plot the statistics
#png("shinyapp2/img/fit_nona_paras_logit.png")
par(mfrow = c(2, 2))
plot(fit.nona.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#dev.off()
#par(mfrow = c(1, 1))
#hist(fit.nona.paras.log$res,main="Residuals") # residuals not really Gaussian

# #png("shinyapp2/img/fit_nona_paras_log_regression.png")
# ggplotRegression(fit.nona.paras.log)
# #dev.off()


set.seed(1800)

fit.nc.nona.paras.log <- lm(dat.nc.nona.logit$outcome.logit ~ dat.nc.nona.logit$Percentage.parasitemia, data=dat.nc.nona.logit)
summary(fit.nc.nona.paras.log) # # show results: R^2: 0.1593, F-stats: 8.39, p-value: 0.006227 **
# MODEL.logit: -6.94397 + 0.05520*dat.nc.nona$Percentage.parasitemia

plot(fit.nc.nona.paras.log)


# (1) SIMPLEST MODEL (JUST PARASITEMIA), DENSITY, WITH TRANSFORMATION, ON WHOLE DATASET --- NICE
set.seed(1800)
fit.paras.dens.log <- lm(dat$outcome.logit ~ dat$Parasite.density...µl., data=dat)

summary(fit.paras.dens.log) # # show results: R^2: 0.2671, F-stats: 16.31, p-value: 0.0002301 ***
# MODEL.logit: -7.172925 + 0.022715*dat$Parasite.density...µl.

#summary(fit.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.0002301
# plot the statistics
par(mfrow = c(2, 2))
plot(fit.paras.dens.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1))
#hist(fit.paras.log$res,main="Residuals") # residuals not really Gaussian


# FOR WEBSITE --- ALSO PLOT -- to compare to non-logit transformation
# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON COMPLETE SAMPLES -- NICE
set.seed(1800)

fit.nona.paras.log <- lm(dat.nona$outcome.logit ~ dat.nona$Percentage.parasitemia, data=dat.nona)
summary(fit.nona.paras.log) # # show results: R^2: 0.2065, F-stats: 6.205, p-value: 0.02215 *
# MODEL.logit: -7.50325 + 0.08221*dat.nona$Percentage.parasitemia

summary(fit.nona.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
#png("shinyapp2/img/fit_nona_paras_log.png")
par(mfrow = c(2, 2))
plot(fit.nona.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#dev.off()
#par(mfrow = c(1, 1))
#hist(fit.nona.paras.log$res,main="Residuals") # residuals not really Gaussian


# FOR WEBSITE --- ALSO PLOT -- to compare to non-logit transformation
# (1) SIMPLEST MODEL (JUST PARASITEMIA), DENSITY, WITH TRANSFORMATION, ON COMPLETE SAMPLES
set.seed(1800)

fit.nona.paras.dens.log <- lm(dat.nona$outcome.logit ~ dat.nona$Parasite.density...µl., data=dat.nona)
summary(fit.nona.paras.dens.log) # # show results: R^2: 0.2509, F-stats: 7.7, p-value: 0.01207
# MODEL.logit: -7.633e+00 + 2.778e-06*dat.nona$Parasite.density...µl.

summary(fit.nona.paras.dens.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
#png("shinyapp2/img/fit_nona_paras_dens_log.png")
par(mfrow = c(2, 2))
plot(fit.nona.paras.dens.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#dev.off()
#par(mfrow = c(1, 1))
#hist(fit.nona.paras.log$res,main="Residuals") # residuals not really Gaussian



set.seed(1800)

fit.nc.nona.paras.log <- lm(dat.nc.nona.logit$outcome.logit ~ dat.nc.nona.logit$Percentage.parasitemia, data=dat.nc.nona.logit)
summary(fit.nc.nona.paras.log) # # show results: R^2: 0.1593, F-stats: 8.39, p-value: 0.006227 ***
# MODEL.logit: -6.94397 + 0.05520*dat.nc.nona.logit$Percentage.parasitemia

par(mfrow = c(2, 2))
plot(fit.nc.nona.paras.log)


set.seed(1800)

fit.nc.nona.paras.dens.log <- lm(dat.nc.nona.logit$outcome.logit ~ dat.nc.nona.logit$Parasite.density...µl., data=dat.nc.nona.logit)
summary(fit.nc.nona.paras.dens.log) # # show results: R^2: 0.2509, F-stats: 7.7, p-value: 0.01207 ***
# MODEL.logit: -7.175796 + 0.021743*dat.nc.nona.logit$Parasite.density...µl.

par(mfrow = c(2, 2))
plot(fit.nc.nona.paras.dens.log)


#===============================================================================
#                      COMPLEX REGRESSION MODELS                                #
#===============================================================================

# (2A) MORE COMPLEX MODEL ---- PLOT FOR WEBSITE
set.seed(1800)

fit.nona.total <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.total) # show results: R^2: 0.38, F-stats: 7.13, p-value: 0.005242
# MODEL: 0.204181 + 0.011821*dat.nona$Percentage.parasitemia + (-0.010121)*dat.nona$Total.White.Cell.Count..x109.L.

#summary(fit.nona.total)$sigma^2 # estimated variance of residuals around a fitted line: 0.02268394
# plot the statistics, OUTLIERS 35, 39 -- both in UM group? -- kept them -- BUT MIGHT BE WORTH TRYING WITHOUT THEM
#png("shinyapp2/img/fit_nona_total.png")
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.nona.total)  # Plot the model information
#dev.off()
# diagnostic plots to CHECK ASSUMPTIONS FOR LINEAR REGRESSION:
# - residuals do not have non-linear patterns
# - residuals about Normally distributed (except for 35, 39)
# - residuals about homoscedastic
# - residuals still within Cook distance (but you can see the dotted lines)
#par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#hist(fit.nona.total$res,main="Residuals") # residuals not really Gaussian

#png("shinyapp2/img/fit_nona_total_regression.png")
ggplotRegression2(fit.nona.total) #---NEED 3-DIM PLOT
#dev.off()

# gvmodel_total <- gvlma(fit.nona.total)
# summary(gvmodel_total) # STILL SKEWED, BUT KURTOSIS ACCEPTABLE

# likelihood ratio test of nested models
#lrtest(fit.nona.paras, fit.nona.total)

# # (2) MORE COMPLEX MODEL, WITH LOGIT TRANSFORMATION --- NOT ANY GOOD
# fit.nona.total.logit <- lm(dat.nona$outcome.logit ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# summary(fit.nona.total.logit) # show results: R^2: 0.33, F-stats: 5.976, p-value: 0.01022
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.total.logit)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.total.logit$res,main="Residuals") # residuals not really Gaussian

# (2) MORE COMPLEX MODEL, DENSITY, WITHOUT LOGIT TRANSFORMATION
set.seed(1800)

fit.nona.total.dens <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.total.dens) # show results: R^2: 0.3702, F-stats: 6.878, p-value: 0.006039
# MODEL: 1.754e-01 + 3.739e-07*dat.nona$Percentage.parasitemia + (-8.169e-03)*dat.nona$Total.White.Cell.Count..x109.L.

#png("shinyapp2/img/fit_nona_total_dens.png")
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.nona.total.dens)  # Plot the model information
#dev.off()
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#hist(fit.nona.total.logit$res,main="Residuals") # residuals not really Gaussian

# # (2) MORE COMPLEX MODEL, DENSITY, WITH LOGIT TRANSFORMATION --- NOT ANY GOOD
# set.seed(1800)
# fit.nona.total.dens.logit <- lm(dat.nona$outcome.logit ~ dat.nona$Parasite.density...µl. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# summary(fit.nona.total.dens.logit) # show results: R^2: 0.33, F-stats: 5.976, p-value: 0.01022
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.total.dens.logit)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.total.logit$res,main="Residuals") # residuals not really Gaussian


#########################################################
# # (2B) MORE COMPLEX MODEL --- just with lymphocytes ----FOR WEBSITE
# set.seed(1800)
# fit.nona.L <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, data=dat.nona)
# summary(fit.nona.L) # show results: R^2: 0.3425, F-stats: 6.209, p-value: 0.008899
# #png("shinyapp2/img/fit_nona_L.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.L)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.L$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 0.0829492 + 0.0130663*dat.nona$Percentage.parasitemia + 0.0003475*dat.nona$Percentage.lymphocytes
# 
# #png("shinyapp2/img/fit_nona_L_regression.png")
# ggplotRegression2(fit.nona.L) #---NEED 3-DIM PLOT
# #dev.off()

# # (2B) MORE COMPLEX MODEL, DENSITY --- just with lymphocytes ----FOR WEBSITE
# set.seed(1800)# fit.nona.dens.L <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.lymphocytes, data=dat.nona)
# summary(fit.nona.dens.L) # show results: R^2: 0.3425, F-stats: 6.209, p-value: 0.008899
# #png("shinyapp2/img/fit_nona_dens_L.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.dens.L)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.dens.L$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 5.265e-02 + 3.924e-07*dat.nona$Parasite.density...µl. + 1.269e-03*dat.nona$Percentage.lymphocytes


# # (2B) MORE COMPLEX MODEL --- just with monocytes ----FOR WEBSITE
# set.seed(1800)
# fit.nona.M <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.monocytes, data=dat.nona)
# summary(fit.nona.M) # show results: R^2: 0.3421, F-stats: 6.199, p-value: 0.00895
# #png("shinyapp2/img/fit_nona_M.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.M)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.M$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 0.0867926 + 0.0132895*dat.nona$Percentage.parasitemia + 0.0006445*dat.nona$Percentage.monocytes
# 
# #png("shinyapp2/img/fit_nona_M_regression.png")
# ggplotRegression2(fit.nona.M) #---NEED 3-DIM PLOT
# #dev.off()

# # (2B) MORE COMPLEX MODEL, DENSITY --- just with monocytes ----FOR WEBSITE
# set.seed(1800)
# fit.nona.dens.M <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.monocytes, data=dat.nona)
# summary(fit.nona.dens.M) # show results: R^2: 0.3421, F-stats: 6.199, p-value: 0.00895
# #png("shinyapp2/img/fit_nona_dens_M.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.dens.M)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.dens.M$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 4.141e-02 + 4.051e-07*dat.nona$Parasite.density...µl. + 7.085e-03*dat.nona$Percentage.monocytes


# # (2B) MORE COMPLEX MODEL --- just with neutrophils ----FOR WEBSITE
# set.seed(1800)
# fit.nona.N <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils, data=dat.nona)
# summary(fit.nona.N) # show results: R^2: 0.3423, F-stats: 6.204, p-value: 0.008925
# #png("shinyapp2/img/fit_nona_N.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.N)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.N$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 0.1053476 + 0.0131751*dat.nona$Percentage.parasitemia + (-0.0002101)*dat.nona$Percentage.neutrophils
# 
# #png("shinyapp2/img/fit_nona_N_regression.png")
# ggplotRegression2(fit.nona.N) #---NEED 3-DIM PLOT
# #dev.off()


# # (2B) MORE COMPLEX MODEL, DENSITY --- just with neutrophils ----FOR WEBSITE
# set.seed(1800)
# fit.nona.dens.N <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.neutrophils, data=dat.nona)
# summary(fit.nona.dens.N) # show results: R^2: 0.3423, F-stats: 6.204, p-value: 0.008925
# #png("shinyapp2/img/fit_nona_dens.N.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.dens.N)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.dens.N$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 1.485e-01 + 3.993e-07*dat.nona$Parasite.density...µl. + (-9.623e-04)*dat.nona$Percentage.neutrophils


# # (2B) MORE COMPLEX MODEL --- just with neutrophils ----FOR WEBSITE
# set.seed(1800)
# fit.nona.N <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils, data=dat.nona)
# summary(fit.nona.N) # show results: R^2: 0.3423, F-stats: 6.204, p-value: 0.008925
# #png("shinyapp2/img/fit_nona_N.png")
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.N)  # Plot the model information
# #dev.off()
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.N$res,main="Residuals") # residuals not really Gaussian
# # MODEL: 0.1053476 + 0.0131751*dat.nona$Percentage.parasitemia + (-0.0002101)*dat.nona$Percentage.neutrophils


#===============================================================================
#                      EXPLORING NON-LINEAR RELATIONSHIPS                      #
#===============================================================================

# INCLUDING NON-LINEAR TERMS
fit.nona.mul <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia * dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.mul)
fit.nona.mul2 <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl. * dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.mul2)
fit.nona.mul3 <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia * dat.nona$Parasite.density...µl., data=dat.nona)
summary(fit.nona.mul3)


#===============================================================================
#                      DRAWING MULTIPLE REGRESSION LINES                       #
#===============================================================================

# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
# library(ggpubr)
# sp <- ggscatter(mtcars, x = "wt", y = "mpg",
#                 add = "reg.line",               # Add regression line
#                 conf.int = TRUE,                # Add confidence interval
#                 color = "cyl", palette = "jco" # Color by groups "cyl"
#                 #shape = "cyl"                   # Change point shape by groups "cyl"
# )+
#   stat_cor(aes(color = cyl), label.x = 3)       # Add correlation coefficient
# sp

#===============================================================================
#                      MULTIPLE REGRESSION LINES                               #
#===============================================================================

library(quantreg)
model.rq <- rq(dat.nc.nona$outcome ~ dat.nc.nona$Percentage.parasitemia * dat.nc.nona$Total.White.Cell.Count..x109.L., tau=c(0.25, 0.5, 0.75))
quantile.regressions <- data.frame(t(coef(model.rq)))
colnames(quantile.regressions) <- c("intercept", "slope")
quantile.regressions$quantile <- rownames(quantile.regressions)
quantile.regressions

library(ggplot2)
scatterplot <- qplot(x=Percentage.parasitemia, y=outcome, data=dat.nc.nona)
scatterplot + geom_abline(aes(intercept=intercept, slope=slope,
                              colour=quantile), data=quantile.regressions)
scatterplot1 <- qplot(x=Percentage.parasitemia, y=outcome, data=dat.nona) 
scatterplot1

#===============================================================================
#                      GENERALIZED LINEAR REGRESSION MODELS                    #
#===============================================================================
# # https://stats.stackexchange.com/questions/38201/problems-plotting-glm-data-of-binomial-proportional-data?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# # GLM
# #GM1<-glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=binomial (logit), data=dat.nona)
# # plot(BPT,p1,col="black",pch=1,main="Relationship a",xlab="Browsing pressure", ylab="Moose Damage Survey")
# # range(log(density))
# # #[1] 0.000000 6.095825
# # xv<-seq(0,6,0.1)
# # range(xv)
# # #[1] 0 6
# # lines(BPT,predict(GM1,type="response"))
# # xv<-seq(0,0.7,length.out = length(BPT))
# # lines(xv,predict(GM1, list(BPT = xv), type="response"))
# #predict(GM1,list(density=exp(xv)),type="response")
# 
# # glm_paras <- glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, family=quasibinomial, data=dat.nona)
# # #glm_paras_logit <- glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, family=quasibinomial (logit), data=dat.nona)
# # glm_total <-glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nona)
# # #glm_total_logit <-glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial (logit), data=dat.nona)
# # summary(glm_paras)
# # # MODEL1: -1.97632 + 0.06533*dat.nona$Percentage.parasitemia
# # #summary(glm_paras_logit)
# # summary(glm_total)
# # # MODEL2: -1.13941 + 0.05487*dat.nona$Percentage.parasitemia + (-0.07721)*dat.nona$Total.White.Cell.Count..x109.L.
# # #summary(glm_total_logit)
# 
# # DO WE NEED LOGIT TRANSFORMATION?
# model <- glm(outcome_prop ~ dat$total_reads, family=binomial, data=dat.nona)
# summary(model)
# 
# model_log <- glm(outcome_prop ~ log(dat$total_reads), family=binomial, data=dat.nona)
# summary(model_log) ## ---> LOGIT TRANSFORMATION / LOGIT LINK NOT NECESSARY
# 
# # (1) GLM SIMPLE | PERCENTAGE
# set.seed(1800)
# glm.paras <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia, family=quasibinomial, data=dat.nona)
# summary(glm.paras)
# par(mfrow = c(2, 2))
# plot(glm.paras)
# # MODEL: -1.96396 + 0.06550*dat.nona$Percentage.parasitemia
# glm.paras.logit <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.paras.logit)
# par(mfrow = c(2, 2))
# plot(glm.paras.logit)
# #predict(glm_paras2.logit,type='response')
# # MODEL.log: -1.964e+00 + 6.550e-02*dat.nona$Percentage.parasitemia
# 
# # png("shinyapp2/img/glm_paras_regression.png")
# # #predict gives the predicted value in terms of logits
# # plot.glm.paras.logit <- data.frame(outcome = dat.nona$outcome,
# #                                    Percentage.parasitemia = dat.nona$Percentage.parasitemia,
# #                                    fit = predict(glm.paras.logit, dat.nona))
# # #convert those logit values to probabilities
# # plot.glm.paras$fit_prob <- plot.glm.paras$fit
# # 
# # library(ggplot2)
# # ggplot(plot.glm, aes(x=Percentage.parasitemia, y=outcome)) + 
# #   geom_point() +
# #   geom_line(aes(x=Percentage.parasitemia, y=fit_prob))
# # dev.off()
# 
# # png("shinyapp2/img/glm_paras_logit_regression.png")
# # #predict gives the predicted value in terms of logits
# # plot.glm.paras.logit <- data.frame(outcome = dat.nona$outcome,
# #                        Percentage.parasitemia = dat.nona$Percentage.parasitemia,
# #                        fit = predict(glm.paras.logit, dat.nona))
# # #convert those logit values to probabilities
# # plot.glm.paras.logit$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))
# # 
# # library(ggplot2)
# # ggplot(plot.glm.paras.logit, aes(x=Percentage.parasitemia, y=outcome)) + 
# #   geom_point() +
# #   geom_line(aes(x=Percentage.parasitemia, y=fit_prob))
# # dev.off()
# 
# # (1) GLM SIMPLE | DENSITY
# set.seed(1800)
# glm.paras.dens <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl., family=quasibinomial, data=dat.nona)
# summary(glm.paras.dens)
# par(mfrow = c(2, 2))
# plot(glm.paras.dens)
# # MODEL: -2.012500 + 0.020313*dat.nona$Parasite.density...µl.
# set.seed(1800)glm.paras.dens.logit <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl., family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.paras.dens.logit)
# par(mfrow = c(2, 2))
# plot(glm.paras.dens.logit)
# #predict(glm_paras.dens.logit,type='response')
# # MODEL.log: -2.012e+00 + 2.031e-02*dat.nona$Parasite.density...µl.
# 
# # png("shinyapp2/img/glm_paras_dens_logit_regression.png")
# # #predict gives the predicted value in terms of logits
# # plot.glm.paras.dens.logit <- data.frame(outcome = dat.nona$outcome,
# #                                    Parasitemia.density = dat.nona$Parasite.density...µl.,
# #                                    fit = predict(glm.paras.dens.logit, dat.nona))
# # #convert those logit values to probabilities
# # plot.glm.paras.dens.logit$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))
# # 
# # library(ggplot2)
# # ggplot(plot.glm.paras.dens.logit, aes(x=Parasitemia.density, y=outcome)) + 
# #   geom_point() +
# #   geom_line(aes(x=Parasitemia.density, y=fit_prob))
# # dev.off()
# 
# # png("shinyapp2/img/glm_paras_dens_regression.png")
# # #predict gives the predicted value in terms of logits
# # plot.glm.paras.dens <- data.frame(outcome = dat.nona$outcome,
# #                                         Parasitemia.density = dat.nona$Parasite.density...µl.,
# #                                         fit = predict(glm.paras.dens, dat.nona))
# # #convert those logit values to probabilities
# # plot.glm.paras.dens$fit_prob <- plot.dat$fit
# # 
# # library(ggplot2)
# # ggplot(plot.glm.paras.dens, aes(x=Parasitemia.density, y=outcome)) + 
# #   geom_point() +
# #   geom_line(aes(x=Parasitemia.density, y=fit_prob))
# # dev.off()
# 
# 
# # (2a) GLM COMPLEX | PERCENTAGE | TOTAL WHITE BLOOD CELLS
# set.seed(1800)
# glm.total <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nona)
# summary(glm.total)
# par(mfrow = c(2, 2))
# plot(glm.total)
# # MODEL: -1.11189 + 0.05324*dat.nona$Percentage.parasitemia + (-0.07415)*dat.nona$Total.White.Cell.Count..x109.L.
# set.seed(1800)
# glm.total.logit <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.total.logit)
# par(mfrow = c(2, 2))
# plot(glm.total.logit)
# # MODEL.log: -1.112e+00 + 5.324e-02*dat.nona$Percentage.parasitemia + (-7.415e-02)*dat.nona$Total.White.Cell.Count..x109.L.
# 
# 
# # (2a) GLM COMPLEX | DENSITY | TOTAL WHITE BLOOD CELLS
# set.seed(1800)
# glm.total.dens <-glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nona)
# summary(glm.total.dens)
# par(mfrow = c(2, 2))
# plot(glm.total.dens)
# # MODEL: -1.2376972 + 0.016756*dat.nona$Parasite.density...µl. + (-0.0663774)*dat.nona$Total.White.Cell.Count..x109.L.
# set.seed(1800)
# glm.total.dens.logit <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Total.White.Cell.Count..x109.L., family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.total.dens.logit)
# par(mfrow = c(2, 2))
# plot(glm.total.dens.logit)
# # MODEL.log: -1.238e+00 + 1.676e-02*dat.nona$Parasite.density...µl. + (-6.638e-02)*dat.nona$Total.White.Cell.Count..x109.L.
# 
# 
# # (2a) GLM COMPLEX | PERCENTAGE | DIFFERENT TYPES OF WHITE BLOOD CELLS COUNTS
# set.seed(1800)
# glm.total.counts <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Lymphocyte.count...x109.L. + dat.nona$Monocyte.count...x109.L. + dat.nona$Neutrophil.count...x109.L., family=quasibinomial, data=dat.nona)
# summary(glm.total.counts)
# par(mfrow = c(2, 2))
# plot(glm.total.counts)
# # MODEL: -1.06792 + 0.06155*dat.nona$Percentage.parasitemia + (-0.58256)*dat.nona$Lymphocyte.count...x109.L. + 2.89776*dat.nona$Monocyte.count...x109.L. + (-0.16340)*dat.nona$Neutrophil.count...x109.L.
# set.seed(1800)
# glm.total.counts.logit <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Lymphocyte.count...x109.L. + dat.nona$Monocyte.count...x109.L. + dat.nona$Neutrophil.count...x109.L., family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.total.counts.logit)
# par(mfrow = c(2, 2))
# plot(glm.total.counts.logit)
# # MODEL.log: -1.068e+00 + 6.155e-02*dat.nona$Percentage.parasitemia + (-5.826e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.898e+00)*dat.nona$Monocyte.count...x109.L. + (-1.634e-01)*dat.nona$Neutrophil.count...x109.L.
# 
# 
# # (2a) GLM COMPLEX | DENSITY | DIFFERENT TYPES OF WHITE BLOOD CELLS COUNTS
# set.seed(1800)
# glm.total.counts.dens <-glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Lymphocyte.count...x109.L. + dat.nona$Monocyte.count...x109.L. + dat.nona$Neutrophil.count...x109.L., family=quasibinomial, data=dat.nona)
# summary(glm.total.counts.dens)
# par(mfrow = c(2, 2))
# plot(glm.total.counts.dens)
# # MODEL: -1.129e+00 + 0.01744*dat.nona$Parasite.density...µl. + (-0.48763)*dat.nona$Lymphocyte.count...x109.L. + 2.63863*dat.nona$Monocyte.count...x109.L. + (-0.16659)*dat.nona$Neutrophil.count...x109.L.
# set.seed(1800)
# glm.total.counts.dens.logit <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Lymphocyte.count...x109.L. + dat.nona$Monocyte.count...x109.L. + dat.nona$Neutrophil.count...x109.L., family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.total.counts.dens.logit)
# par(mfrow = c(2, 2))
# plot(glm.total.counts.dens.logit)
# # MODEL.log: -1.129e+00 + 1.744e-06*dat.nona$Parasite.density...µl. + (-4.876e-01)*dat.nona$Lymphocyte.count...x109.L. + (2.639e+00)*dat.nona$Monocyte.count...x109.L. + (-1.666e-01)*dat.nona$Neutrophil.count...x109.L.
# 

#===============================================================================
#                      GLMs on data.nc.nona (only complete on selected vars)   #
#===============================================================================

# (1) GLM SIMPLE | PERCENTAGE
set.seed(1800)
glm.paras <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Percentage.parasitemia, family=quasibinomial, data=dat.nc.nona)
summary(glm.paras)
par(mfrow = c(2, 2))
plot(glm.paras)
# MODEL: -1.61320 + 0.04321*dat.nona$Percentage.parasitemia
glm.paras.logit <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Percentage.parasitemia, family=binomial(link = 'logit'), data=dat.nc.nona)
summary(glm.paras.logit)
par(mfrow = c(2, 2))
plot(glm.paras.logit)
#predict(glm_paras2.logit,type='response')
# MODEL.log: -1.613e+00 + 4.321e-02*dat.nona$Percentage.parasitemia

# png("shinyapp2/img/glm_paras_regression.png")
# #predict gives the predicted value in terms of logits
# plot.glm.paras.logit <- data.frame(outcome = dat.nona$outcome,
#                                    Percentage.parasitemia = dat.nona$Percentage.parasitemia,
#                                    fit = predict(glm.paras.logit, dat.nona))
# #convert those logit values to probabilities
# plot.glm.paras$fit_prob <- plot.glm.paras$fit
# 
# library(ggplot2)
# ggplot(plot.glm, aes(x=Percentage.parasitemia, y=outcome)) + 
#   geom_point() +
#   geom_line(aes(x=Percentage.parasitemia, y=fit_prob))
# dev.off()

# png("shinyapp2/img/glm_paras_logit_regression.png")
# #predict gives the predicted value in terms of logits
# plot.glm.paras.logit <- data.frame(outcome = dat.nona$outcome,
#                        Percentage.parasitemia = dat.nona$Percentage.parasitemia,
#                        fit = predict(glm.paras.logit, dat.nona))
# #convert those logit values to probabilities
# plot.glm.paras.logit$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))
# 
# library(ggplot2)
# ggplot(plot.glm.paras.logit, aes(x=Percentage.parasitemia, y=outcome)) + 
#   geom_point() +
#   geom_line(aes(x=Percentage.parasitemia, y=fit_prob))
# dev.off()

# (1) GLM SIMPLE | DENSITY
set.seed(1800)
glm.paras.dens <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Parasite.density...µl., family=quasibinomial, data=dat.nc.nona)
summary(glm.paras.dens)
par(mfrow = c(2, 2))
plot(glm.paras.dens)
# MODEL: -1.83306 + 0.01809*dat.nona$Parasite.density...µl.
set.seed(1800)
glm.paras.dens.logit <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Parasite.density...µl., family=binomial(link = 'logit'), data=dat.nc.nona)
summary(glm.paras.dens.logit)
par(mfrow = c(2, 2))
plot(glm.paras.dens.logit)
#predict(glm_paras.dens.logit,type='response')
# MODEL.log: -1.833e+00 + 1.809e-02*dat.nona$Parasite.density...µl.

# png("shinyapp2/img/glm_paras_dens_logit_regression.png")
# #predict gives the predicted value in terms of logits
# plot.glm.paras.dens.logit <- data.frame(outcome = dat.nona$outcome,
#                                    Parasitemia.density = dat.nona$Parasite.density...µl.,
#                                    fit = predict(glm.paras.dens.logit, dat.nona))
# #convert those logit values to probabilities
# plot.glm.paras.dens.logit$fit_prob <- exp(plot.dat$fit)/(1+exp(plot.dat$fit))
# 
# library(ggplot2)
# ggplot(plot.glm.paras.dens.logit, aes(x=Parasitemia.density, y=outcome)) + 
#   geom_point() +
#   geom_line(aes(x=Parasitemia.density, y=fit_prob))
# dev.off()

# png("shinyapp2/img/glm_paras_dens_regression.png")
# #predict gives the predicted value in terms of logits
# plot.glm.paras.dens <- data.frame(outcome = dat.nona$outcome,
#                                         Parasitemia.density = dat.nona$Parasite.density...µl.,
#                                         fit = predict(glm.paras.dens, dat.nona))
# #convert those logit values to probabilities
# plot.glm.paras.dens$fit_prob <- plot.dat$fit
# 
# library(ggplot2)
# ggplot(plot.glm.paras.dens, aes(x=Parasitemia.density, y=outcome)) + 
#   geom_point() +
#   geom_line(aes(x=Parasitemia.density, y=fit_prob))
# dev.off()


# (2a) GLM COMPLEX | PERCENTAGE | TOTAL WHITE BLOOD CELLS
set.seed(1800)
glm.total <-glm(outcome_prop.nc.nona ~ dat.nc.nona$Percentage.parasitemia + dat.nc.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nc.nona)
summary(glm.total)
par(mfrow = c(2, 2))
plot(glm.total)
# MODEL: -0.92665 + 0.04200*dat.nona$Percentage.parasitemia + (-0.06456)*dat.nona$Total.White.Cell.Count..x109.L.
set.seed(1800)
glm.total.logit <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Percentage.parasitemia + dat.nc.nona$Total.White.Cell.Count..x109.L., family=binomial(link = 'logit'), data=dat.nc.nona)
summary(glm.total.logit)
par(mfrow = c(2, 2))
plot(glm.total.logit)
# MODEL.log: -9.266e-01 + 4.200e-02*dat.nona$Percentage.parasitemia + (-6.456e-02)*dat.nona$Total.White.Cell.Count..x109.L.


# (2a) GLM COMPLEX | DENSITY | TOTAL WHITE BLOOD CELLS
set.seed(1800)
glm.total.dens <-glm(outcome_prop.nc.nona ~ dat.nc.nona$Parasite.density...µl. + dat.nc.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nc.nona)
summary(glm.total.dens)
par(mfrow = c(2, 2))
plot(glm.total.dens)
# MODEL: -1.357823 + 0.017015*dat.nona$Parasite.density...µl. + (-0.041395)*dat.nona$Total.White.Cell.Count..x109.L.
set.seed(1800)
glm.total.dens.logit <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Parasite.density...µl. + dat.nc.nona$Total.White.Cell.Count..x109.L., family=binomial(link = 'logit'), data=dat.nc.nona)
summary(glm.total.dens.logit)
par(mfrow = c(2, 2))
plot(glm.total.dens.logit)
# MODEL.log: -1.358e+00 + 1.701e-02*dat.nona$Parasite.density...µl. + (-4.139e-02)*dat.nona$Total.White.Cell.Count..x109.L.


# (2a) GLM COMPLEX | PERCENTAGE | DIFFERENT TYPES OF WHITE BLOOD CELLS COUNTS
set.seed(1800)
glm.total.counts <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Percentage.parasitemia + dat.nc.nona$Lymphocyte.count...x109.L. + dat.nc.nona$Monocyte.count...x109.L. + dat.nc.nona$Neutrophil.count...x109.L., family=quasibinomial, data=dat.nc.nona)
summary(glm.total.counts)
par(mfrow = c(2, 2))
plot(glm.total.counts)
# MODEL: -1.16788 + 0.04861*dat.nona$Percentage.parasitemia + (-0.19762)*dat.nona$Lymphocyte.count...x109.L. + 0.73308*dat.nona$Monocyte.count...x109.L. + (-0.06992)*dat.nona$Neutrophil.count...x109.L.
set.seed(1800)
glm.total.counts.logit <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Percentage.parasitemia + dat.nc.nona$Lymphocyte.count...x109.L. + dat.nc.nona$Monocyte.count...x109.L. + dat.nc.nona$Neutrophil.count...x109.L., family=binomial(link = 'logit'), data=dat.nc.nona)
summary(glm.total.counts.logit)
par(mfrow = c(2, 2))
plot(glm.total.counts.logit)
# MODEL.log: -1.168e+00 + 4.861e-02*dat.nona$Percentage.parasitemia + (-1.976e-01)*dat.nona$Lymphocyte.count...x109.L. + (7.331e-01)*dat.nona$Monocyte.count...x109.L. + (-6.992e-02)*dat.nona$Neutrophil.count...x109.L.


# (2a) GLM COMPLEX | DENSITY | DIFFERENT TYPES OF WHITE BLOOD CELLS COUNTS
set.seed(1800)
glm.total.counts.dens <-glm(outcome_prop.nc.nona ~ dat.nc.nona$Parasite.density...µl. + dat.nc.nona$Lymphocyte.count...x109.L. + dat.nc.nona$Monocyte.count...x109.L. + dat.nc.nona$Neutrophil.count...x109.L., family=quasibinomial, data=dat.nc.nona)
summary(glm.total.counts.dens)
par(mfrow = c(2, 2))
plot(glm.total.counts.dens)
# MODEL: -1.55608 + 0.01770*dat.nona$Parasite.density...µl. + (-0.13421)*dat.nona$Lymphocyte.count...x109.L. + 0.75485*dat.nona$Monocyte.count...x109.L. + (-0.06598)*dat.nona$Neutrophil.count...x109.L.
set.seed(1800)
glm.total.counts.dens.logit <- glm(outcome_prop.nc.nona ~ dat.nc.nona$Parasite.density...µl. + dat.nc.nona$Lymphocyte.count...x109.L. + dat.nc.nona$Monocyte.count...x109.L. + dat.nc.nona$Neutrophil.count...x109.L., family=binomial(link = 'logit'), data=dat.nc.nona)
summary(glm.total.counts.dens.logit)
par(mfrow = c(2, 2))
plot(glm.total.counts.dens.logit)
# MODEL.log: -1.556e+00 + 1.770e-02*dat.nona$Parasite.density...µl. + (-1.342e-01)*dat.nona$Lymphocyte.count...x109.L. + (7.549e-01)*dat.nona$Monocyte.count...x109.L. + (-6.598e-02)*dat.nona$Neutrophil.count...x109.L.


#===============================================================================
#                      PLOTS GLMs                                              #
#===============================================================================
############
ggplotRegression3 <- function (fit, limit) {  
  require(ggplot2)  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    # scale_x_continuous(name="Percentage of parasitemia", limits=c(0,limit)) +  ## -- with it pdenstiy does not show up
    scale_x_continuous(name="Parasitemia density [1/µl]", limits=c(0,limit)) +  ## -- with it pdenstiy does not show up
    scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) + 
    geom_point() +
    # xlab("Percentage of parasitemia") + 
    # ylab("Percentage of reads mapping to pathogen") + 
    stat_smooth(method = "lm", col = "blue", fullrange=TRUE) +
    geom_text(label=rownames(fit$model)) 
    # axis(2, at=y, labels=formatC(y,big.mark=",",format="fg"),las=2,cex=0.1); 
    # labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
    #                    "; Intercept =",signif(fit$coef[[1]],5 ),
    #                    "; Slope =",signif(fit$coef[[2]], 5),
    #                    "; P-value =",signif(summary(fit)$coef[2,4], 5))) + 
    # annotate("text", x=0.1, y=-0.05, label = "R^2 == 0.78", parse=T) +
    # annotate("text", x=0.1, y=-0.06, label = "alpha == 0.00", parse=T) +
    # annotate("text", x=0.1, y=-0.07, label = "beta == 0.67", parse=T) +
    # theme(plot.margin = margin(10, 10, 10, 100))
}
###############

# two regression lines on top of each other
ggplotRegressionLayered <- function (fit1, fit2) {  
  require(ggplot2)  
  dfc <- rbind(fit1$model, fit2$model)
  ggplot(dfc, aes(x,y, group=model)) + 
        # scale_x_continuous(name="Percentage of parasitemia", limits=c(0,limit)) +  ## -- with it pdenstiy does not show up
        scale_x_continuous(name="Parasitemia density [1/µl]", limits=c(0,limit)) +  ## -- with it pdenstiy does not show up
        scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) + 
        geom_point() +
        # xlab("Percentage of parasitemia") + 
        # ylab("Percentage of reads mapping to pathogen") + 
        stat_smooth(method = "lm", col = "blue", fullrange=TRUE) 
        # geom_text(label=rownames(fit$model)) 
      # axis(2, at=y, labels=formatC(y,big.mark=",",format="fg"),las=2,cex=0.1); 
      # labs(title = paste("Adjusted R^2 = ",signif(summary(fit)$adj.r.squared, 5),
      #                    "; Intercept =",signif(fit$coef[[1]],5 ),
      #                    "; Slope =",signif(fit$coef[[2]], 5),
      #                    "; P-value =",signif(summary(fit)$coef[2,4], 5))) + 
      # annotate("text", x=0.1, y=-0.05, label = "R^2 == 0.78", parse=T) +
      # annotate("text", x=0.1, y=-0.06, label = "alpha == 0.00", parse=T) +
      # annotate("text", x=0.1, y=-0.07, label = "beta == 0.67", parse=T) +
      # theme(plot.margin = margin(10, 10, 10, 100))
    }

#ggplotRegressionLayered(fit.nc.nona.paras, fit.nona.paras)


ggplot(dat.nc.nona, aes(Percentage.parasitemia, outcome), color=cyl) + 
  scale_x_continuous(name="Percentage of parasitemia", limits=c(0,100)) +  ## -- with it pdenstiy does not show up
  scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) + 
  geom_text(label=rownames(dat.nc.nona)) +
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  geom_point(color='darkred') +
  geom_smooth(data = dat.nc.nona[c(7,10,11,19),], method="lm", se=F, color="darkred", linetype = "dashed") +
  scale_colour_manual(name="lines", values=c("red", "blue")) 
  # guides(colour = guide_legend(override.aes = list(alpha = 0)))


###################----PLOTS--------!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#png("../shinyapp4/img/both_paras_regr_better_outlier_4.png", width = 1000, height = 600)
ggplot(dat.nc.nona, aes(Percentage.parasitemia, outcome)) + 
  scale_x_continuous(name="Percentage of parasitemia", limits=c(0,50)) +  ## -- with it pdenstiy does not show up
  scale_y_continuous(name="Percentage of reads mapping to pathogen", limits=c(0,1)) + 
  geom_text(label=rownames(dat.nc.nona)) +
  geom_point(colour="dodgerblue",alpha=0.75) +
  geom_abline(aes(colour="linear model on complete \n samples (21) \n", intercept=0.090267, slope=0.013339), alpha=1, size=1) +
  geom_smooth(aes(colour="linear model on samples that complete \n after variable selection (40) \n"), method = "lm", linetype="dashed", se=FALSE) + 
  geom_smooth(data = dat.nc.nona[-c(29,33),], aes(colour="linear model without \n potential outliers 35,39 \n"), method="lm", se=F, linetype = "dashed") +
  geom_smooth(data = dat.nc.nona[-c(5,8,9,14),], aes(colour="linear model without \n potential outliers 7,10,11,19 \n"), method="lm", se=F, linetype = "dashed") +
  geom_smooth(data = dat.nc.nona[-c(5,8,9,14,29,33),], aes(colour="linear model without \n potential outliers 7,10,11,19,35,39 \n"), method="lm", se=F, linetype = "dashed") +
  #geom_smooth(data = dat.nc.nona[c(7,10,11,19),], aes(colour="linear model without potential outliers 7,10,11,19 \n"), method="lm", se=F, linetype = "dashed") +
  
  scale_colour_manual(name="Linear regression lines \n", values=c("red", "blue", "orange", "green", "brown")) + 
  guides(colour = guide_legend(override.aes = list(alpha = 0))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=13))
#dev.off()


# ##########-----3D plot----------------------------------------------------------------------------
# # 3D Scatterplot with Coloring and Vertical Lines
# # and Regression Plane 
# library(scatterplot3d) 
# attach(dat.nona) 
# s3d <-scatterplot3d(dat.nona$outcome, dat.nona$Percentage.parasitemia, dat.nona$Total.White.Cell.Count..x109.L., pch=16, highlight.3d=TRUE,
#                     type="h", main="3D Scatterplot")
# fit_3d <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L.) 
# s3d$plane3d(fit_3d)
# 
# 
# # 3D scatter plot
# #s3d <- scatterplot(dat.nona$outcome, dat.nona$Percentage.parasitemia, dat.nona$Total.White.Cell.Count..x109.L.)
# s3d <- scatterplot3d(dat.nona[,c(5,26,17)],
#                      main = "3D scatter plot with regression plane",
#                      pch = 16, color="steelblue"
#                      )
# # s3d <- scatterplot3d(trees, type = "h", color = "blue",
# #                      angle=55, pch = 16)
# # Add regression plane
# s3d$plane3d(fit.nona.total)
# # # Add supplementary points
# # s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
# #              col = "red", type = "h", pch = 8)
# 
# library(Rcmdr)
# library(rgl)
# attach(dat.nona) 
# scatter3d(dat.nona$outcome, dat.nona$Percentage.parasitemia, dat.nona$Total.White.Cell.Count..x109.L.)
# 
# 
# s3d <- scatterplot3d(dat.nona$Percentage.parasitemia, dat.nona$outcome, dat.nona$Total.White.Cell.Count..x109.L., pch=16, highlight.3d = TRUE, type = "h", main = "3D Scatterplot")
# s3d$plane3d(glm.total)
# fit.nona.total
# 
# 
# # fit model
# 
# # predict over sensible grid of values
# parasitemia <- unique(dat.nona$Percentage.parasitemia)
# white_cell <- unique(dat$Total.White.Cell.Count..x109.L.)
# grid <- with(dat.nona, expand.grid(parasitemia, white_cell))
# d <- setNames(data.frame(grid), c("Percentage of parsitemia", "Total white cell count"))
# vals <- predict(fit.nona.total, newdata = d)
# 
# # form matrix and give to plotly
# m <- matrix(vals, nrow = length(unique(d$wt)), ncol = length(unique(d$disp)))
# 
# library(plotly)
# plot_ly() %>% add_surface(x = ~white_cell, y = ~parasitemia, z = ~m)
# 
# oneplane <- expand.grid(x = 1:6, y = 1:6)
# oneplane$z <- 1:6
# oneplane.m <- as.matrix(spread(oneplane, key = x, value = z)[, -1])
# plot_ly() %>% add_trace(x = 1:6, 
#                         y = 1:6, 
#                         z = oneplane.m, 
#                         type = "surface", 
#                         opacity = .5,
#                         cauto = FALSE,
#                         cmax = 1,
#                         cmin = 0,
#                         colorscale = list(c(0,'#d1d1d1'),c(1,'#000000')))
# 
# plot_ly() %>% add_surface(x = 1:6, 
#                           y = 1:6, 
#                           z = dancplane.m, 
#                           type = "surface", 
#                           opacity = 1,
#                           colors = c('#d1d1d1','#000000'))
# 
# 
# library(reshape)
# #Graph Resolution (more important for more complex shapes)
# graph_reso <- 0.05
# 
# #Setup Axis
# axis_x <- seq(min(dat.nona$Percentage.parasitemia), max(dat.nona$Percentage.parasitemia), by = graph_reso)
# axis_y <- seq(min(dat.nona$Total.White.Cell.Count..x109.L.), max(dat.nona$Total.White.Cell.Count..x109.L.), by = graph_reso)
# 
# #Sample points
# petal_lm_surface <- expand.grid(Percentage.parasitemia = axis_x, Total.White.Cell.Count..x109.L. = axis_y,KEEP.OUT.ATTRS = F)
# petal_lm_surface$outcome <- predict.lm(fit.nona.total, newdata = petal_lm_surface[1:21,])
# petal_lm_surface <- acast(petal_lm_surface, Total.White.Cell.Count..x109.L. ~ Percentage.parasitemia, value.var = "outcome") #y ~ x

#########

# png("../shinyapp4/img/fit_paras_regr.png")
# ggplotRegression3(fit.paras, 100)
# dev.off()

#png("../shinyapp4/img/fit_nona_paras_regr.png")
ggplotRegression3(fit.nona.paras, 100)
#dev.off()

# png("../shinyapp4/img/fit_nc_nona_paras_regr.png")
# ggplotRegression3(fit.nc.nona.paras, 100)
# dev.off()
# 
# 
# ## relabel axes
# png("../shinyapp4/img/fit_paras_dens_regr.png")
# ggplotRegression3(fit.paras.dens, 100)
# dev.off()
# 
# png("../shinyapp4/img/fit_nona_paras_dens_regr.png")
# ggplotRegression3(fit.nona.paras.dens, 100)
# dev.off()
# 
# png("../shinyapp4/img/fit_paras_dens_regr.png")
# ggplotRegression3(fit.paras.dens, 100)
# dev.off()
# 
# .
# .
# .


# (1) GLM SIMPLE | PERCENTAGE
par(mfrow = c(1, 1))
plot(glm.paras)
ggplotRegression3(glm.paras, 0.5, 100)

plot(dat.nona$outcome, dat.nona$Percentage.parasitemia)
lines(log(dat.nona$outcome),glm.paras$fitted.values)

# (1) GLM SIMPLE | DENSITY


# (2) GLM COMPLEX | PERCENTAGE | TOTAL WHITE BLOOD CELLS

# (2) GLM COMPLEX | DENSITY | TOTAL WHITE BLOOD CELLS


# (2) GLM COMPLEX | PERCENTAGE | DIFFERENT TYPES OF WHITE BLOOD CELLS COUNTS

# (2) GLM COMPLEX | DENSITY | DIFFERENT TYPES OF WHITE BLOOD CELLS COUNTS


## regression plots l.130

#===============================================================================
#                      CROSSVALIDATION                                         #
#===============================================================================

# # train a naive bayes -- only for classification problems
# model <- NaiveBayes(outcome_prop ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data = dat.nc.nona)
# 
# # make predictions
# x_test <- completeDat[,1:6]
# y_test <- completeDat[,7]
# predictions <- predict(model, x_test)
# 
# # summarise results
# confusionMatrix(predictions$class, y_test)

# # BOOTSTRAP
# # define training control
# train_control <- trainControl(method = "boot", number = 50)
# # train the model
# model <- train(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data = dat.nc.nona, trControl=train_control, method="nb")
# # summarise results
# print(model)

library(bootstrap)
# bootstrapping with 1000 replications
results <- boot(data=dat.nc.nona, statistic)

## TO CONTINUE


# # K-FOLD CROSS VALIDATION
# # define training control
# train_control <- trainControl(method = "cv", number = 10)
# # fix parameters of the algorithm
# grid <- expand.grid(.fL=c(0), usekernel=c(FALSE))
# # train the model
# model <- train(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data = dat.nc.nona, trControl=train_control, method="nb", tuneGrid=grid)
# print(model)

library(DAAG)
cv.lm(completeDat[1:43,], fit.paras, m=3) # 3-fold cross-validation

#===============================================================================#
#                      P-VALUES FOR GLMs                                        #
#===============================================================================#

null <- glm(outcome_prop.nc.nona ~ 1, family=binomial(link = 'logit'), data=dat.nc.nona)
anova(glm.paras, glm.paras.dens, null, test = "Chisq")

anova(null, null, glm.paras)
anova(glm.paras, null)

anova(glm.paras, glm.paras.dens)
  
# analyse linear model
summary(fit.nona.paras)
drop1(fit.nona.paras, test="F")
anova(fit.nona.paras)

library(ERSA)
plottStats(fit.nona.paras)

cols <- termColours(fit.nona.paras)
plottStats(fit.nona.paras, cols)



# GLM REGRESSION LINE
ggplot(dat.nona,aes(Percentage.parasitemia,outcome))+xlab("Parasitemia percentage")+ylab("Pathogen reads")+geom_point()+geom_line(aes(y=fitted(glm.paras.logit)))
#ggplot(dat.nona,aes(dat$Parasite.density...µl.,outcome))+geom_point()+geom_line(aes(y=fitted(glm.paras.dens)))
abline(h=0.6)
#points(10,0.6)
#lines(10,0.6, type=o)


# #### NOT NEEDED ANYMORE
# # (2b) GLM COMPLEX | PERCENTAGE | LYMPHO
# glm.lympho <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, family=quasibinomial, data=dat.nona)
# summary(glm.lympho)
# par(mfrow = c(2, 2))
# plot(glm.lympho)
# # MODEL: -1.77229 + 0.07545*dat.nona$Percentage.parasitemia + (-0.01056)*dat.nona$Percentage.lymphocytes
# glm.lympho.logit <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.lympho.logit)
# par(mfrow = c(2, 2))
# plot(glm.lympho.logit)
# # MODEL.log: -1.772e+00 + 7.545e-02*dat.nona$Percentage.parasitemia + (-1.056e-02)*dat.nona$Percentage.lymphocytes
# 
# 
# # (2b) GLM COMPLEX | DENSITY | LYMPHO
# glm.lympho.dens <-glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.lymphocytes, family=quasibinomial, data=dat.nona)
# summary(glm.lympho.dens)
# par(mfrow = c(2, 2))
# plot(glm.lympho.dens)
# # MODEL: -1.939e+00 + 2.125e-06*dat.nona$Parasite.density...µl. + (-3.870e-03)*dat.nona$Percentage.lymphocytes
# glm.lympho.dens.logit <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.lymphocytes, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.lympho.dens.logit)
# par(mfrow = c(2, 2))
# plot(glm.lympho.dens.logit)
# # MODEL.log: -1.939e+00 + 2.125e-06*dat.nona$Parasite.density...µl. + (-3.870e-03)*dat.nona$Percentage.lymphocytes
# 
# 
# # (2b) GLM COMPLEX | PERCENTAGE | MONO
# glm.mono <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.monocytes, family=quasibinomial, data=dat.nona)
# summary(glm.mono)
# par(mfrow = c(2, 2))
# plot(glm.mono)
# # MODEL: -2.02525 + 0.06461*dat.nona$Percentage.parasitemia + 0.01143*dat.nona$Percentage.monocytes
# glm.mono.logit <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.monocytes, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.mono.logit)
# par(mfrow = c(2, 2))
# plot(glm.mono.logit)
# # MODEL.logL: -2.025e+00 + 6.461e-02*dat.nona$Percentage.parasitemia + 1.143e-02*dat.nona$Percentage.monocytes
# 
# # (2b) GLM COMPLEX | DENSITY | MONO
# glm.mono.dens <-glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.monocytes, family=quasibinomial, data=dat.nona)
# summary(glm.mono.dens)
# par(mfrow = c(2, 2))
# plot(glm.mono.dens)
# # MODEL: -2.226e+00 + 1.950e-06*dat.nona$Parasite.density...µl. + 3.953e-02*dat.nona$Percentage.monocytes
# glm.mono.dens.logit <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.monocytes, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.mono.dens.logit)
# par(mfrow = c(2, 2))
# plot(glm.mono.dens.logit)
# # MODEL.logL: -2.226e+00 + 1.951e-06*dat.nona$Parasite.density...µl. + 3.953e-02*dat.nona$Percentage.monocytes
# 
# 
# # (2b) GLM COMPLEX | PERCENTAGE | NEUTRO
# glm.neutro <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils, family=quasibinomial, data=dat.nona)
# summary(glm.neutro)
# par(mfrow = c(2, 2))
# plot(glm.neutro)
# # MODEL: -2.581276 + 0.073414*dat.nona$Percentage.parasitemia + 0.008204*dat.nona$Percentage.neutrophils
# glm.neutro.logit <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.neutro.logit)
# par(mfrow = c(2, 2))
# plot(glm.neutro.logit)
# # MODEL.log: -2.581e+00 + 7.341e-02*dat.nona$Percentage.parasitemia + 8.204e-03*dat.nona$Percentage.neutrophils
# 
# # (2b) GLM COMPLEX | DENSITY | NEUTRO
# glm.neutro.dens <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.neutrophils, family=quasibinomial, data=dat.nona)
# summary(glm.neutro.dens)
# par(mfrow = c(2, 2))
# plot(glm.neutro.dens)
# # MODEL: -2.238e+00 + 2.106e-06*dat.nona$Parasite.density...µl. + 3.036e-03*dat.nona$Percentage.neutrophils
# glm.neutro.dens.logit <- glm(outcome_prop.nona ~ dat.nona$Parasite.density...µl. + dat.nona$Percentage.neutrophils, family=binomial(link = 'logit'), data=dat.nona)
# summary(glm.neutro.dens.logit)
# par(mfrow = c(2, 2))
# plot(glm.neutro.dens.logit)
# # MODEL.log: -2.238e+00 + 2.106e-06*dat.nona$Parasite.density...µl. + 3.036e-03*dat.nona$Percentage.neutrophils

# 
# #===============================================================================
# #                      MODEL COMPARISON                                        #
# #===============================================================================
# # MODEL COMPARISON
# # We now can compare the two models as before using ANOVA. In the case of binary data, we need to do a Chi-squared test.
# anova(glm_paras2, glm_total2, test = "Chi")
# #anova(glm_paras_logit, glm_total_logit, test = "Chi")
# anova(glm_paras2, glm_total2, test = "Chi")
# 
# 
# # # run the two models MODEL1, MODEL2
# # # set up a two-panel plotting area
# # par(mfrow = c(1, 2))
# # # use the predict() function to create the fitted y values
# # ## 1. parsitemia
# # # create a sequence of x values
# # xv <- seq(0, 9, 0.01)
# # # y values
# # yv <- predict(glm_paras, list(area = xv), type = "response")
# # # plot the raw data
# # plot(dat.nona$Percentage.parasitemia, dat.nona$outcome)
# # # use the lines function to add the fitted lines
# # lines(xv, yv)
# # ## 2. parasitemia and white blood cells
# # xv2 <- seq(0, 10, 0.1)
# # yv2 <- predict(glm_total, list(isolation = xv2), type = "response")
# # plot(dat.nona$Total.White.Cell.Count..x109.L., dat.nona$outcome)
# # lines(xv2, yv2)
# 
# 
# # https://www.theanalysisfactor.com/r-tutorial-glm1/
# # http://www.simonqueenborough.info/R/stats-basic/glm.html
# # http://www.simonqueenborough.info/R/statistics/glm-binomial 
# 
# 
# 
# # ------ BETA FUNCTIONS
# # Zeileis, Kleiber, and Jackman (2018): generalized count data regression
# # model shares some properties with generalized linear models (GLMs, McCullagh and Nelder 1989) like linear predictor, link function, dispersion parameter, 
# # but it is not a special case of this framework. 
# 
# library(betareg)
# set.seed(123)
# 
# beta_paras <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia |  dat.nona$Percentage.parasitemia, data=dat.nona)
# beta_total <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L. |  dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# #beta_total.log <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L. |  dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona, link = "log")
# beta_total.logit <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L. |  dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona, link = "logit")
# beta_total.loglog <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L. |  dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona, link = "loglog")
# summary(beta_paras)
# # MODEL: -1.86818 + 0.05910*dat.nona$Percentage.parasitemia, Pseudo R-squared: 0.2972
# #summary(beta_total.log)
# summary(beta_total)
# # MODEL: -1.20699 + 0.04832*dat.nona$Percentage.parasitemia + (-0.05682)*dat.nona$Total.White.Cell.Count..x109.L.
# # Pseudo R-squared: 0.4065
# summary(beta_total.logit)
# summary(beta_total.loglog) # improves pseudo R^2 of the model
# #summary(beta_total.loglog)$pseudo.r.squared
# # CONCLUSION: THERE ARE A FEW OBSERVATIONS ABOVE THE DIAGONAL WHERE THE LOG-LOG LINK FITS BETTER THAN THE LOGIT LINK..
# # WHEREAS THERE ARE FEWER SUCH OBSERVATIONS BELOW THE DIAGONAL.
# 
# # # HOW WELL ON NEW/ IMPUTED DATA? -- ERROR BARS TO BIG!! BETA DOES NOT PERFORM WELL --> TUNE HYPERPARAM link.phi!
# # GTest <- completeDat[1:21,]
# # model <- predict(beta_paras,newdata=GTest)
# # library(ggplot2)
# # ggplot(data=GTest,aes(x=model,y=outcome)) + 
# #   geom_point() + geom_abline(slope=1)
# # 
# # GTest$modelErr <- sqrt(predict(beta_paras,newdata=GTest,
# #                                type='variance'))
# # ggplot(data=GTest,aes(x=model,y=outcome)) +
# #   geom_point() +
# #   geom_errorbarh(aes(xmin=model-modelErr,xmax=model+modelErr)) +
# #   geom_abline(slope=1)
# 
# 
# # NOT USEFUL, DOES NOT REDUCE NUMBER OF ITERATIONS
# #beta_loglog2 <- update(beta_loglog, link.phi = "log")
# #summary(beta_loglog2)
# #summary(beta_loglog)
# 
# # NOT USEFUL
# #plot(beta, type="pearson")
# #plot(beta, which = 5, type = "deviance", sub.caption = "")
# 
# # AIC(beta_paras, beta_paras_logit, beta_paras_loglog) # beta_loglog not much better
# # BIC(beta_paras, beta_paras_logit, beta__paras_loglog)
# 
# # # Compare means of a likelihood-ratio test to confirm results
# # # ie testing H_0 of equidispersion against a specific alternative of variable dispersion
# # lrtest(beta_paras, beta_paras_logit, beta_paras_loglog)
# 
# # NOT IMPORTANT
# # # DOES MODEL EXHIBIT HETEROSKEDASTICITY? use studentised Breusch and Pagan (1979) test of Koenker (1981)
# # library(lmtest)
# # bptest(fit.nona.paras)
# # bptest(fit.nona.total)
# 
# #####-------- MULTINOMIAL MODEL
# library(nnet)
# #multinom
# 
# # ##########################################
# # 
# # # PLOT the best linear regression model
# # library(ggplot2)
# # ggplot(dat.nona, aes(x = dat.nona$Percentage.parasitemia, y = dat.nona$Total.White.Cell.Count..x109.L.)) + 
# #   geom_point() +
# #   stat_smooth(method = "lm", col = "red")
# # 
# # # ADJUST NAMES OF AXES!!!!
# # # Write a function that also returns statistics
# 
# # 
# # # ggplotRegression2 <- function (fit) {
# # #   
# # #   require(ggplot2)
# # #   
# # #   ggplot(fit$model, aes_string(x = names(fit$model)[3], y = names(fit$model)[1])) + 
# # #            geom_point() +
# # #            stat_smooth(method = "lm", col = "red") +
# # #            labs(title = paste("Adj R^2 = ",signif(summary(fit)$adj.r.squared, 5),
# # #                               #"Mult R^2 = ",signif(summary(fit)$mult.r.squared, 5),
# # #                               "Intercept =",signif(fit$coef[[1]],5 ),
# # #                               " Slope =",signif(fit$coef[[3]], 5),
# # #                               " P =",signif(summary(fit)$coef[2,4], 5)))
# # #   }
# # 
# # 
# # ggplotRegression(fit.nona.paras) # for most simple model (parasitemia percentage)
# # #par(new=TRUE)
# # #ggplotRegression(fit.nona.total) # the same plot
# # 
# # # Are the residuals of the linear regression model homoscedastic, ie the RVs have the same finite variance?
# # # SIZE AND COLOR #https://drsimonj.svbtle.com/visualising-residuals
# # # Same coloring as above, size corresponding as well
# # 
# # 
# # d <- dat.nona
# # d$predicted <- predict(fit.nona.total) 
# # d$residuals <- residuals(fit.nona.total)
# # library(dplyr)
# # d %>% select(outcome, predicted, residuals) %>% head() # quick look at the actual, predicted and residual values
# # #head(d)
# # # ADD LEGEND HERE
# # ggplot(d, aes(x = Percentage.parasitemia, y = outcome)) +
# #   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
# #   geom_segment(aes(xend = Percentage.parasitemia, yend = predicted), alpha = .2) +
# #   
# #   # > Color AND size adjustments made here...
# #   geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
# #   scale_color_continuous(low = "black", high = "red") +
# #   guides(color = FALSE, size = FALSE) +  # Size legend also removed
# #   # <
# #   geom_point(aes(y = predicted), shape = 1) +
# #   theme_bw()
# # 
# # # MORE COMPLEX MODEL
# # d <- dat.nona
# # d$predicted <- predict(fit.nona.total) 
# # d$residuals <- residuals(fit.nona.total)
# # library(dplyr)
# # d %>% select(outcome, predicted, residuals) %>% head() # quick look at the actual, predicted and residual values
# # # Let's crank up the complexity! PLOTTING a multiple linear regression model
# # library(tidyr)
# # #d2 <- subset(d, select = c(Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L, total.number.of.cells, outcome, predicted, residuals))
# # # just percentage parasetemia, total number of white blood cells
# # d2 <- subset(d, select = c(d$Percentage.parasitemia, d$Total.White.Cell.Count..x109.L., d$outcome, d$predicted, d$residuals))
# # d2 %>% 
# #   gather(key = "iv", value = "x", -outcome, -predicted, -residuals) %>%  # Get data into shape
# #   ggplot(aes(x = x, y = outcome)) +  # Note use of `x` here and next line
# #   geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
# #   geom_point(aes(color = residuals)) +
# #   scale_color_gradient2(low = "blue", mid = "white", high = "red") +
# #   guides(color = FALSE) +
# #   geom_point(aes(y = predicted), shape = 1) +
# #   facet_grid(~ iv, scales = "free_x") +  # Split panels here by `iv`
# #   theme_bw()
# # 
# # # ADDING POINTS TO PLOT
# # # ggplot(d, aes(x = Percentage.parasitemia, y = outcome)) +
# # #   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
# # #   geom_segment(aes(xend = Percentage.parasitemia, yend = predicted), alpha = .2) +
# # #   
# # #   # > Color AND size adjustments made here...
# # #   geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
# # #   scale_color_continuous(low = "black", high = "red") +
# # #   guides(color = FALSE, size = FALSE) +  # Size legend also removed
# # #   # <
# # #   #geom_point(size = 3, aes(colour = highlight)) +
# # #   #scale_color_manual("Status", values = mycolours) +
# # #   geom_text(data = d, aes(x = 2 * 5.05, y = 1.0, label = "prediction")) 
# # #   #geom_point(shape=23, fill="blue", color="darkred", size=3)
# # #   geom_point(aes(y = predicted), shape = 1) +
# # #   theme_bw()
# # 
# # 
# # # TO MAKE PLOT INTERACTIVE. PACKAGES NOT COMPATIBLE -- NEEDS FIXING!
# # # require(ggiraph)
# # # require(ggiraphExtra)
# # # require(plyr)
# # # ggPredict(fit.nona.total,se=TRUE,interactive=TRUE)
# # 
# # # (2b)
# # fit.nona.white <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# # summary(fit.nona.white) # show results: R^2: 0.442
# # par(mfrow = c(2, 2))  
# # plot(fit.nona.white)  
# # 
# # # (3a)
# # fit.nona.red <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Red.blood.cell.count..x1012.L + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# # summary(fit.nona.red) # show results: R^2: 0.4671
# # par(mfrow = c(2, 2))  
# # plot(fit.nona.red) 
# # 
# # # (3b)
# # fit.nona.hemo <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Hemoglobin.concentration..g.dL. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# # summary(fit.nona.hemo) # show results: R^2: 0.4527
# # par(mfrow = c(2, 2))  
# # plot(fit.nona.hemo) 
# # 
# # 
# # # # on original data set, 46 samples
# # # # ---------WHAT HAPPENS TO MISSING VALUES HERE? HOW ARE THEY IMPLICITLY IMPUTED??
# # # fit <- lm(dat$outcome ~ dat$Percentage.parasitemia + dat$Hemoglobin.concentration..g.dL. + dat$Total.White.Cell.Count..x109.L., data=dat)
# # # summary(fit) # show results: R^2: 0.37
# # 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # # Other useful functions
# # coefficients(fit) # model coefficients
# # confint(fit, level=0.95) # CIs for model parameters
# # fitted(fit) # predicted values
# # residuals(fit) # residuals
# # anova(fit) # anova table
# # vcov(fit) # covariance matrix for model parameters
# # influence(fit) # regression diagnostics
# # 
# # #---DIAGNOSTIC PLOTS---------------
# # # to check for heteroscedasticity, normality, and influential observerations 
# # layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
# # plot(fit)
# # 
# # #---COMPARING MODELS---------------
# # # nested models --> anova function
# # # What does Hemoglobin concentration and total white cell count to linear prediction?
# # fit1 <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Hemoglobin.concentration..g.dL. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# # fit2 <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, data=dat.nona)
# # anova(fit1, fit2) 
# # 
# # #---COMPARING MODELS---------------
# # #---CROSSVALIDATION---------------

# CROSSVALIDATION FROM CARET PACKAGE
# libraries needed
library(caret)
library(psych)



# # 
# # # 1. Validation set approach -- NOT SUITABLE HERE
# # # # Split the data into training and test set
# # # set.#seed(123)
# # # training.samples <- swiss$Fertility %>%
# # #   createDataPartition(p = 0.8, list = FALSE)
# # # train.data  <- swiss[training.samples, ]
# # # test.data <- swiss[-training.samples, ]
# # # # Build the model
# # # model <- lm(Fertility ~., data = train.data)
# # # Make predictions and compute the R2, RMSE and MAE
# # predictions <- fit.nona.paras %>% predict(completeDat[1:21,])
# # data.frame( R2 = R2(predictions, completeDat[1:21,]$outcome),
# #             RMSE = RMSE(predictions, completeDat[1:21,]$outcome),
# #             MAE = MAE(predictions, completeDat[1:21,]$outcome))
# # # the model with the lowest test sample RMSE is the preferred one. 
# # # prediction error rate should also be as small as possible:
# # RMSE(predictions, completeDat[1:21,]$outcome)/mean(completeDat[1:21,]$outcome) # 0.7742042
# # 
# # # # 2. Leave one out cross-validation
# # # library(caret)
# # # # Define training control
# # # train.control <- trainControl(method = "LOOCV")
# # # # Train the model
# # # model <- train(outcome ~., data = dat.nona, method = "lm",
# # #                trControl = train.control)
# # # # Summarize the results
# # # print(model)
# # 
# # # 3. k-fold cross validation
# # # Define training control
# # set.#seed(123) 
# # train.control <- trainControl(method = "cv", number = 10)
# # # Train the model
# # model1 <- train(outcome ~ Percentage.parasitemia, data=dat.nona, method = "lm",
# #                trControl = train.control)
# # # Summarize the results
# # print(model1)
# # # RMSE       Rsquared   MAE      
# # # 0.1395187  0.9950257  0.1265135
# # 
# # # Define training control
# # set.#seed(123) 
# # train.control <- trainControl(method = "cv", number = 10)
# # # Train the model
# # model1B <- train(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data=dat.nona, method = "lm",
# #                 trControl = train.control)
# # # Summarize the results
# # print(model1B)
# # #   RMSE       Rsquared   MAE      
# # # 0.1351248  0.9444301  0.1183391
# # 
# # # 4. Repeated K-fold cross-validation
# # # Define training control
# # set.#seed(123)
# # train.control <- trainControl(method = "repeatedcv", 
# #                               number = 10, repeats = 3)
# # # Train the model
# # model2 <- train(outcome ~ Percentage.parasitemia, data=dat.nona, method = "lm",
# #                trControl = train.control)
# # # Summarize the results
# # print(model2)
# # #   RMSE       Rsquared  MAE      
# # # 0.1433029  0.958348  0.1277027
# # 
# # # Define training control
# # set.#seed(123)
# # train.control <- trainControl(method = "repeatedcv", 
# #                               number = 10, repeats = 3)
# # # Train the model
# # model2B <- train(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data=dat.nona, method = "lm",
# #                 trControl = train.control)
# # # Summarize the results
# # print(model2B)
# # # RMSE       Rsquared   MAE      
# # # 0.1381558  0.9442862  0.1193008
# # 
# # 
# # #cv.lm(df = houseprices, form.lm = formula(sale.price ~ area), m=3, dots = 
# # #        FALSE, #seed=29, plotit=TRUE, printit=TRUE)
# #   
# # # # K-fold cross-validation
# # # library(DAAG)
# # # cv.lm(df=dat.nona, fit, m=3) # 3 fold cross-validation ----UNUSED ARGUMENT?
# # # # cv.lm(df = mydata, form.lm = formula(y ~ .))
# # # reg<-lm(logWet.weight~logAverageBL)
# # # cv.lm(mtross, reg, m=5)
# # # 
# # # library(DAAG)
# # # cvResults <- suppressWarnings(CVlm(df=cars, fit.nona.paras, m=5, dots=FALSE, #seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
# # # attr(cvResults, 'ms')  # => 251.2783 mean squared error
# # 
# # # Sum the MSE for each fold, 
# # # divide by the number of observations, 
# # # and take the square root to get the 
# # # cross-validated standard error of estimate.
# # 
# # # Assess R2 shrinkage via K-fold cross-validation. 
# # # Assessing R2 shrinkage using 10-Fold Cross-Validation
# # 
# # fit <- lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat.nona)
# # 
# # # library(bootstrap)
# # # # define functions
# # # theta.fit <- function(x,y){lsfit(x,y)}
# # # theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
# # # 
# # # # matrix of predictors
# # # X <- as.matrix(mydata[c("x1","x2","x3")])
# # # # vector of predicted values
# # # y <- as.matrix(mydata[c("y")])
# # # 
# # # results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
# # # cor(y, fit$fitted.values)**2 # raw R2
# # # cor(y,results$cv.fit)**2 # cross-validated R2 
# # 
# # #---VARIABLE SELECTION---------------
# # # Selecting a subset of predictor variables from a larger set 
# # # (e.g., stepwise selection) is a controversial topic. 
# # # Perform stepwise selection (forward, backward, both) using the 
# # # stepAIC( ) function from the MASS package. 
# # # stepAIC( ) performs stepwise model selection by exact AIC. 
# # 
# # # Stepwise Regression
# # library(MASS)
# # fit <- lm(y~x1+x2+x3,data=mydata)
# # step <- stepAIC(fit, direction="both")
# # step$anova # display results 
# # 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # # Alternatively: perform all-subsets regression using the leaps( ) function 
# # # from the leaps package. 
# # # In the following code nbest indicates the number of subsets of each 
# # # size to report. Here, the ten best models will be reported for each 
# # # subset size (1 predictor, 2 predictors, etc.).
# # 
# # # All Subsets Regression
# # library(leaps)
# # attach(mydata)
# # leaps<-regsubsets(y~x1+x2+x3+x4,data=mydata,nbest=10)
# # # view results
# # summary(leaps)
# # # plot a table of models showing variables in each model.
# # # models are ordered by the selection statistic.
# # plot(leaps,scale="r2")
# # # plot statistic by subset size
# # library(car)
# # subsets(leaps, statistic="rsq") 
# # 
# # # Other options for plot( ) are bic, Cp, and adjr2. 
# # # Other options for plotting with subset( ) are bic, cp, adjr2, and rss.
# # 
# # #---RELATIVE IMPORTANCE---------------
# # # The relaimpo package provides measures of relative importance for 
# # # each of the predictors in the model. See help(calc.relimp) for details 
# # # on the four measures of relative importance provided.
# # 
# # # Calculate Relative Importance for Each Predictor
# # library(relaimpo)
# # calc.relimp(fit,type=c("lmg","last","first","pratt"),
# #             rela=TRUE)
# # 
# # # Bootstrap Measures of Relative Importance (1000 samples)
# # boot <- boot.relimp(fit, b = 1000, type = c("lmg",
# #                                             "last", "first", "pratt"), rank = TRUE,
# #                     diff = TRUE, rela = TRUE)
# # booteval.relimp(boot) # print result
# # plot(booteval.relimp(boot,sort=TRUE)) # plot result 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # 
# # 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # #----https://www.statmethods.net/stats/rdiagnostics.html-------
# # #-----------REGRESSION DIAGNOSTICS-----------------------------
# # 
# # # An excellent review of regression diagnostics is provided in 
# # # John Fox's aptly named Overview of Regression Diagnostics. 
# # # Dr. Fox's car package provides advanced utilities for regression modeling. 
# # 
# # # Assume that we are fitting a multiple linear regression
# # # on the MTCARS data
# # library(car)
# # fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
# # 
# # # This example is for exposition only. 
# # # We will ignore the fact that this may not be a great way of 
# # # modeling the this particular set of data!
# # 
# # #---OUTLIERS----------------------------------------------
# # # Assessing Outliers
# # outlierTest(fit) # Bonferonni p-value for most extreme obs
# # qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
# # leveragePlots(fit) # leverage plots 
# # 
# # #---INFLUENTIAL OBERSVATIONS------------------------------
# # # Influential Observations
# # # added variable plots
# # av.Plots(fit)
# # # Cook's D plot
# # # identify D values > 4/(n-k-1)
# # cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
# # plot(fit, which=4, cook.levels=cutoff)
# # # Influence Plot
# # influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# # 
# # #---NON-NORMALITY------------------------------------------
# # # Normality of Residuals
# # # qq plot for studentised residual
# # qqPlot(fit, main="QQ Plot")
# # # distribution of studentized residuals
# # library(MASS)
# # sresid <- studres(fit)
# # hist(sresid, freq=FALSE,
# #      main="Distribution of Studentized Residuals")
# # xfit<-seq(min(sresid),max(sresid),length=40)
# # yfit<-dnorm(xfit)
# # lines(xfit, yfit) 
# # 
# # #---NON-CONSTANT ERROR VARIANCE-----------------------------
# # # Evaluate homoscedasticity
# # # non-constant error variance test
# # ncvTest(fit)
# # # plot studentized residuals vs. fitted values
# # spreadLevelPlot(fit)
# # 
# # #---MULTI-COLLINEARITY--------------------------------------
# # # Evaluate Collinearity
# # vif(fit) # variance inflation factors
# # sqrt(vif(fit)) > 2 # problem?
# # 
# # #---NONLINEARITY--------------------------------------------
# # # Evaluate Nonlinearity
# # # component + residual plot
# # crPlots(fit)
# # # Ceres plots
# # ceresPlots(fit)
# # 
# # #---NON-INDEPENDENCE OF ERRORS------------------------------
# # # Test for Autocorrelated Errors
# # durbinWatsonTest(fit)
# # 
# # #---ADDITIONAL DIAGNOSTIC HELP------------------------------
# # # The gvlma( ) function in the gvlma package, performs a global validation 
# # # of linear model assumptions as well separate evaluations of skewness, 
# # # kurtosis, and heteroscedasticity.
# # 
# # # Global test of model assumptions
# # library(gvlma)
# # gvmodel <- gvlma(fit)
# # summary(gvmodel) 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # #--------------------------------------------------------------
# # #--------------------------------------------------------------
# # #histogram(dat.nona$outcome ~ dat.nona$Percentage.parasitemia)
# # barplot(dat$Percentage.parasitemia/100, dat$outcome, xlab = "percentage of parasitemia", ylab = "proportion of parasite reads")
# # 
# # library(ggplot2)
# # theme_set(theme_bw())
# # 
# # # Draw plot
# # ggplot(dat, aes(x=Percentage.parasitemia/100, y=outcome)) + 
# #   geom_bar(stat="identity", width=.01, fill="tomato3") + 
# #   labs(title="Ordered Bar Chart", 
# #        subtitle="Percentage parasitemia vs outcome", 
# #        caption="source: mpg") + 
# #   theme(axis.text.x = element_text(angle=65, vjust=0.6))
# # 
# # #--------------------------------------------------------------
# # #--------------------------------------------------------------
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # 
# # 
# # # PREPROCESSING
# # ##--- TODO: VISUALISE IT NICELY FOR WEBSITE, TO SHOW ON WHICH DATA THE MODEL HAS BEEN TRAINED
# # summary(dat) 
# # 
# ##---MISSING VALUES #--analyticsvidhya.com
# #install.packages("missForest")
# #library(missForest)
# #dat.mis <- prodNA(dat, noNA = 0.1) # generate 10% missing values at random
# #summary(dat.mis) # check missing values introduced in the data
# 
# library(mice)
# par(las=2) # make label text perpendicular to axis
# #text(quantile(resS2$dif, 0.005), 5, "0.5% FP rate", pos = 2, cex = 0.6, srt = 90)
# md.pattern(dat) # returns a tabular form of missing value present in each variable in a dataset
# # 21 observations with no missing values
# # (5 missing values in mean cell volume
# # 8 missing values in parasite clones
# # 3 missing values in lactate
# # 2 missing values in mean cell and parasite clones..)
# 
# library(visdat)
# png("img/missingData_2.png", width = 3000, height = 1500)
# vis_miss(dat[,-c(23,24,25,27)])
# dev.off()
# 
# png("shinyapp2/img/missingData2.png")
# gg_miss_var(dat[,-c(23,24,25,27)])
# dev.off
# 
# library(naniar)
# ##png("shinyapp2/img/parasitemia.#png")
# ggplot(dat,
#        aes(x = Percentage.parasitemia,
#            y = outcome)) +
#   geom_point() 
# #dev.off()
# 
# library(ggplot2)
# gg_miss_fct(x = dat, fct = Subject.ID) + labs(title = "Missing values")
# 
# library(VIM)
# mice_plot <- aggr(dat, col=c('navyblue', 'yellow'), numbers=TRUE, sortVars=TRUE,
#                   labels=names(dat), cex.axis=.7, gap=3, ylab=c("Missing data", "Pattern"))
# # There are 45.7% values in the data set with no missing values
# # 17.4% missing values in parasite clone...

##--- 1. Impute missing values

#imputed_dat <- mice(dat, m=5, maxit=50, method='pmm', #seed=500)
## TODO: TRY VARIOUS NUMBER OF ITERATIONS (MAXIT) AND METHODS
## Note: multiple imputation helps to reduce bias and increase efficiency

# problem here: singular matrix (some variables highly collinear)
# --> exclude some variables that are not essential to all the analyses we are going to conduct

#===============================================================================
#                     IMPUING MISSING VALUES FOR CROSSVALIDATION               #
#===============================================================================

##--- remove categorical variables
#dat.nc <- subset(dat, select = -c(Sex, Ethnicity, Sickle.cell.screen))
# SINGULAR MATRIX --> CHOOSE LESS VARIABLES
#dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, total.number.of.cells, Total.White.Cell.Count..x109.L., Monocyte.count...x109.L., Lymphocyte.count...x109.L., Red.blood.cell.count..x1012.L, Hemoglobin.concentration..g.dL., Parasite.density...µl., Parasite.clones, outcome))
dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Lymphocyte.count...x109.L., Monocyte.count...x109.L., Neutrophil.count...x109.L., outcome))

library(mice)
imputed_dat <- mice(dat.nc, m=5, maxit=50, method='pmm', seed=500)
#imputed_dat <- mice(dat.nc, m=5, maxit=50, method='cart', seed=500)

summary(imputed_dat)
# missing cells per column: 0 | 1 | 3 | 2

# get complete data (2nd out of 5 imputed data sets)
completeDat <- complete(imputed_dat, 2)
completeDat

## To score better accuracy in building predictive models
##--alternative packages to impute data:
## - Amelia: uses bootstrap based EMB algoithm which makes it faster and robust to impute many variables incl cross sectional, time series data etc;
##           also, it is enabled with parallel imputation feature using multicore CPUs
##    works best when data has multivariate normal distribution, otherwise: transform data!
## - missForest: uses random forest -- a non-parametric imputation method applicable to various variable types,
##                ie it makes no explicit assumptions about functional form of f
## - Hmisc: automatically recognises the variable types and uses bootstrap sample and predictive mean matching to impute missing values,
##          ie no need to separate or treat categorical variables
## - mi

# # 
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # #---2. PREDICTIVE / LINEAR REGRESSION MODEL
# # # build models on all 5 datasets
# # fit <- with(data = dat, exp = lm(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L. + Red.blood.cell.count..x1012.L))
# # summary(fit)
# # 
# # # combine results of all 5 models:
# # #combine <- pool(fit)
# # 
# # #---see also: https://www.r-bloggers.com/correlation-and-linear-regression/
# # #-- linear candidate models for the data (see Ada's slides):
# # fit.0 = lm(outcome ~ 1, data=dat) # just intercept
# # summary(fit.0)
# # 
# # fit.c = lm(outcome ~ Percentage.parasitemia, data=dat)
# # summary(fit.c) # R^2 = 0.26
# # fit.d = lm(outcome ~ Hemoglobin.concentration..g.dL., data=dat)
# # summary(fit.d) # R^2 = 0.26
# # fit.w = lm(outcome ~ Total.White.Cell.Count..x109.L., data=dat)
# # summary(fit.w) # R^2 = 0.06
# # 
# # fit.cd = lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL., data=dat)
# # summary(fit.cd) # R^2 = 0.35
# # fit.cw = lm(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data=dat)
# # summary(fit.cw) # R^2 = 0.30
# # fit.dw = lm(outcome ~ Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat)
# # summary(fit.dw) # R^2 = 0.078
# # 
# # fit.cdw = lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat)
# # summary(fit.cdw) # R^2 = 0.37
# # 
# # # with parasite clones instead of hemoglobin concentration
# # fit.p = lm(outcome ~ Parasite.clones, data=dat)
# # summary(fit.p) # R^2 = 0.086
# # fit.pc = lm(outcome ~ Percentage.parasitemia + Parasite.clones, data=dat)
# # summary(fit.pc) # R^2 = 0.43
# # fit.pw = lm(outcome ~ Parasite.clones + Total.White.Cell.Count..x109.L., data=dat)
# # summary(fit.pw) # R^2 = 0.158
# # 
# # fit.pcw = lm(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., data=dat)
# # summary(fit.pcw) # R^2 = 0.48
# # ######--------------------------------NO LONGER NECESSARY--------------------------------------------------
# # 
# # # check that the models assumptions are met, 
# # # indeed linear models make a few assumptions on the data, 
# # # the first one is that your data are normally distributed, 
# # # the second one is that the variance in y is homogeneous over 
# # # all x values (sometimes called homoscedasticity) and 
# # # independence which means that a y value at a certain x value 
# # # should not influence other y values.
# # 
# # # ---plotting three best models
# # # * graphs first column: look at variance homogeneity amongst others
# # #   (we see no pattern in dots but random clouds of points 
# # #   --> homogeneity assumption not violated)
# # # * graph top right: checks normality assumption
# # #   (if data normally distributed, points should fall in straight line)
# # # * graph bottom right: shows how each y influences the model,
# # #   each point is removed at a time and the new model is compared to the one with the point,
# # #   if the point is very influential then it will have a high leverage value.
# # #   POINTS WITH TO HIGH LEVERAGE VALUE SHOULD BE REMOVED FROM THE DATASET 
# # #   (to remove their outlying effect on the model).
# # # here: remove 39, maybe 3, 2.
# # par(mfrow=c(2,2))
# # plot(fit.cdw)
# # 
# # par(mfrow=c(2,2))
# # plot(fit.pc)
# # 
# # par(mfrow=c(2,2))
# # plot(fit.pcw)
# # 
# # ######--------------------------------CONTINUE ON FROM HERE--------------------------------------------------
# # 
# # # 3. TRANSFORMING AND REMOVING OUTLIERS FROM THE DATA
# # #----Transforming the data (for non-normal or heterogeneous data)
# # #-----usually a trial and error process
# # 
# # library(gtools)
# # library(car)
# # completeData$outcome <- completeDat$outcome
# # completeData$outcome
# # #completeData$outcome<-log(completeData$outcome)
# # completeData$outcomeLogit <- logit(completeData$outcome, min=0, max=0)
# # completeData$outcomeLogit
# # cbind(completeData$outcome, completeData$outcomeLogit)
# # plot(outcome~Percentage.parasitemia,completeData)
# # 
# # #-- Comparing models using anova
# # # used to compare multiple models
# # # does an f-test to compare 2 or more nested models
# # #anova(fit.c, fit.d)
# # #anova(fit.c, fit.w)
# # #anova(fit.d, fit.w)
# # #anova(fit.c, fit.cd)
# # #anova(fit.c, fit.cw)
# # #anova(fit.d, fit.cd)
# # #anova(fit.d, fit.dw)
# # #anova(fit.w, fit.cw)
# # anova(fit.w, fit.dw) # only one that has same size of dataset
# # 
# # library(car)
# # Anova(fit.w)
# # Anova(fit.w, fit.dw)
# # 
# # drop1(fit.c, test="F")
# # drop1(fit.d, test="F")
# # drop1(fit.w, test="F")
# # drop1(fit.cd, test="F")
# # drop1(fit.cw, test="F")
# # drop1(fit.dw, test="F")
# # drop1(fit.cdw, test="F")
# # 
# # ## logistic regression 
# # ## GENERALISED LINEAR MODEL, ie robust regression method on the raw, untransformed values
# # library(ISLR)
# # glm.fit <- glm(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., completeDat, family=binomial)
# # summary(glm.fit)
# # glm.fit2 <- glm(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., completeDat, family=quasibinomial())
# # summary(glm.fit2)
# # # null deviance (the deviance just for the mean) and 
# # # the residual deviance (the deviance for the model with all the predictors)
# # 
# # # make predictions on the training data that we use to fit the model
# # # returns vector of fitted probabilities
# # glm.probs <- predict(glm.fit, type = "response")
# # glm.probs[1:5] # look at the first 5 probabilities
# # 
# # # now define what coverage is enough, eg elem [0.2, 0.8], st we have enough host and pathogen maps
# # #glm.pred <- ifelse((glm.probs > 0.2 && glm.probs < 0.8) , "Enough", "Not enough")
# # 
# # #attach(completeDat)
# # #table(glm.pred, outcome)
# # 
# # #mean(glm.pred == outcome)
# # 
# # # GENERALISED LINEAR MODEL (extension of the general linear model) ON BEST MODEL FROM ABOVE
# # # unlike general linear model does not require that response variable is normally distributed
# # # --> try different link functions (~transformation of response variable) for different 
# # #     relationships between linear model and response variable (eg inverse, logit, log etc)
# # 
# # library(nnet)
# # #completeData$Percentage.parasitemia <- relevel(completeData$Percentage.parasitemia, ref = "parasetemia")
# # test <- multinom(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., data=completeDat)
# # summary(test)
# # 
# # z <- summary(test)$coefficients/summary(test)$standard.errors
# # z
# # 
# # # 2-tailed z test
# # p <- (1 - pnorm(abs(z), 0, 1)) * 2
# # p
# # 
# # ## extract the coefficients from the model and exponentiate
# # exp(coef(test))
# # 
# # head(pp <- fitted(test))
# # 
# # #dses <- data.frame(ses = c("low", "middle", "high"), write = mean(completeDat$outcome))
# # #predict(test, newdata = dses, "probs")
# # 
