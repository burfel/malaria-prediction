library(caret)
library(doParallel)
registerDoParallel(detectCores() - 1)

setwd("~/Documents/IMPERIAL/PROJECTS/project2")

# read in files
hg_pf <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/hg_pf_readcounts.csv")
supp <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/Supplementary_Dataset.csv", header=TRUE)

# merge data sets (by samples/ subjectID)
dat <- merge(supp, hg_pf, by.y = "samples", by.x = "Subject.ID")

# add column "outcome" which gives the proportion of pathogen reads
dat$outcome <- dat$pf_count / (dat$hg_count + dat$pf_count)

#reg.ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE)

# HOW TO LOOK AT DITRIBUTION OF VALUES?

#library(mice)
#tempData <- mice(dat,m=5)

# drop the samples that have blanks
dat.nona <- na.omit(dat)

#set.seed(1895) # to reproduce same result, for every model set new seed, different seed functions?
#lm.fit <- train(outcome ~ ., data = dat, trControl = reg.ctrl, method = "lm")
#lm.fit

##:::::::TODO: IMPUTE VALUES
#set.seed(1896) # to reproduce same result, for every model set new seed, different seed functions?
#lm.fit <- train(outcome ~ ., data = dat, trControl = reg.ctrl, method = "lm")
##lm.fit <- train(outcome ~ ., data = dat, trControl = reg.ctrl, method = "lm")
#lm.fit

#lm(formula, dat, weights, na.action,
#   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#   singular.ok = TRUE, contrasts = NULL, offset)


#--------------------------------------------------------------
#---------------------------------------------------
#---https://www.statmethods.net/stats/regression.html
#---FITTING THE MODEL-------------------------------
# Multiple Linear Regression Example
# on complete data set (samples with missing values removed), 42 samples
fit <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Hemoglobin.concentration..g.dL. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit) # show results

# on original data set, 46 samples
# ---------WHAT HAPPENS TO MISSING VALUES HERE? HOW ARE THEY IMPLICITLY IMPUTED??
fit <- lm(dat$outcome ~ dat$Percentage.parasitemia + dat$Hemoglobin.concentration..g.dL. + dat$Total.White.Cell.Count..x109.L., data=dat)
summary(fit) # show results

# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

#---DIAGNOSTIC PLOTS---------------
# to check for heteroscedasticity, normality, and influential observerations 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

#---COMPARING MODELS---------------
# nested models --> anova function
# What does Hemoglobin concentration and total white cell count to linear prediction?
fit1 <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Hemoglobin.concentration..g.dL. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
fit2 <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, data=dat.nona)
anova(fit1, fit2) 

#---COMPARING MODELS---------------
# K-fold cross-validation
library(DAAG)
cv.lm(df=dat.nona, fit, m=3) # 3 fold cross-validation ----UNUSED ARGUMENT?

# Sum the MSE for each fold, 
# divide by the number of observations, 
# and take the square root to get the 
# cross-validated standard error of estimate.

# Assess R2 shrinkage via K-fold cross-validation. 
# Assessing R2 shrinkage using 10-Fold Cross-Validation

fit <- lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat.nona)

# library(bootstrap)
# # define functions
# theta.fit <- function(x,y){lsfit(x,y)}
# theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
# 
# # matrix of predictors
# X <- as.matrix(mydata[c("x1","x2","x3")])
# # vector of predicted values
# y <- as.matrix(mydata[c("y")])
# 
# results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
# cor(y, fit$fitted.values)**2 # raw R2
# cor(y,results$cv.fit)**2 # cross-validated R2 

#---VARIABLE SELECTION---------------
# Selecting a subset of predictor variables from a larger set 
# (e.g., stepwise selection) is a controversial topic. 
# Perform stepwise selection (forward, backward, both) using the 
# stepAIC( ) function from the MASS package. 
# stepAIC( ) performs stepwise model selection by exact AIC. 

# Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
step <- stepAIC(fit, direction="both")
step$anova # display results 


# Alternatively: perform all-subsets regression using the leaps( ) function 
# from the leaps package. 
# In the following code nbest indicates the number of subsets of each 
# size to report. Here, the ten best models will be reported for each 
# subset size (1 predictor, 2 predictors, etc.).

# All Subsets Regression
library(leaps)
attach(mydata)
leaps<-regsubsets(y~x1+x2+x3+x4,data=mydata,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
library(car)
subsets(leaps, statistic="rsq") 

# Other options for plot( ) are bic, Cp, and adjr2. 
# Other options for plotting with subset( ) are bic, cp, adjr2, and rss.

#---RELATIVE IMPORTANCE---------------
# The relaimpo package provides measures of relative importance for 
# each of the predictors in the model. See help(calc.relimp) for details 
# on the four measures of relative importance provided.

# Calculate Relative Importance for Each Predictor
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

#----https://www.statmethods.net/stats/rdiagnostics.html-------
#-----------REGRESSION DIAGNOSTICS-----------------------------

# An excellent review of regression diagnostics is provided in 
# John Fox's aptly named Overview of Regression Diagnostics. 
# Dr. Fox's car package provides advanced utilities for regression modeling. 

# Assume that we are fitting a multiple linear regression
# on the MTCARS data
library(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 

# This example is for exposition only. 
# We will ignore the fact that this may not be a great way of 
# modeling the this particular set of data!

#---OUTLIERS----------------------------------------------
# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit) # leverage plots 

#---INFLUENTIAL OBERSVATIONS------------------------------
# Influential Observations
# added variable plots
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#---NON-NORMALITY------------------------------------------
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

#---NON-CONSTANT ERROR VARIANCE-----------------------------
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

#---MULTI-COLLINEARITY--------------------------------------
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

#---NONLINEARITY--------------------------------------------
# Evaluate Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
ceresPlots(fit)

#---NON-INDEPENDENCE OF ERRORS------------------------------
# Test for Autocorrelated Errors
durbinWatsonTest(fit)

#---ADDITIONAL DIAGNOSTIC HELP------------------------------
# The gvlma( ) function in the gvlma package, performs a global validation 
# of linear model assumptions as well separate evaluations of skewness, 
# kurtosis, and heteroscedasticity.

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel) 

#--------------------------------------------------------------
#--------------------------------------------------------------
#histogram(dat.nona$outcome ~ dat.nona$Percentage.parasitemia)
barplot(dat$Percentage.parasitemia/100, dat$outcome, xlab = "percentage of parasitemia", ylab = "proportion of parasite reads")

library(ggplot2)
theme_set(theme_bw())

# Draw plot
ggplot(dat, aes(x=Percentage.parasitemia/100, y=outcome)) + 
  geom_bar(stat="identity", width=.01, fill="tomato3") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#--------------------------------------------------------------
#--------------------------------------------------------------

# PREPROCESSING
##--- TODO: VISUALISE IT NICELY FOR WEBSITE, TO SHOW ON WHICH DATA THE MODEL HAS BEEN TRAINED
summary(dat) 

##---MISSING VALUES #--analyticsvidhya.com
#install.packages("missForest")
#library(missForest)
#dat.mis <- prodNA(dat, noNA = 0.1) # generate 10% missing values at random
#summary(dat.mis) # check missing values introduced in the data

install.packages("mice")
library(mice)

md.pattern(dat) # returns a tabular form of missing value present in each variable in a dataset
# 21 observations with no missing values
# (5 missing values in mean cell volume 
# 8 missing values in parasite clones
# 3 missing values in lactate
# 2 missing values in mean cell and parasite clones..)

installed.packages("VIM")
library(VIM)
mice_plot <- aggr(dat, col=c('navyblue', 'yellow'), numbers=TRUE, sortVars=TRUE,
                  labels=names(dat), cex.axis=.7, gap=3, ylab=c("Missing data", "Pattern"))
# There are 45.7% values in the data set with no missing values 
# 17.4% missing values in parasite clone...

##--- 1. Impute missing values

imputed_dat <- mice(dat, m=5, maxit=50, method='pmm', seed=500)
## TODO: TRY VARIOUS NUMBER OF ITERATIONS (MAXIT) AND METHODS
## Note: multiple imputation helps to reduce bias and increase efficiency

# problem here: singular matrix (some variables highly collinear)
# --> exclude some variables that are not essential to all the analyses we are going to conduct

##--- remove categorical variables
#dat.nc <- subset(dat, select = -c(Sex, Ethnicity, Sickle.cell.screen))
dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L))

imputed_dat <- mice(dat.nc, m=5, maxit=50, method='pmm', seed=500)

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

#---2. Build predictive model
# build models on all 5 datasets
fit <- with(data = dat, exp = lm(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L. + Red.blood.cell.count..x1012.L))
summary(fit)

# combine results of all 5 models:
#combine <- pool(fit)


#-- 4 linear candidate models for the data (see Ada's slides):
fit.0 = lm(outcome ~ 1, data=dat) # just intercept

fit.c = lm(outcome ~ Percentage.parasitemia, data=dat)
fit.d = lm(outcome ~ Hemoglobin.concentration..g.dL., data=dat)
fit.w = lm(outcome ~ Total.White.Cell.Count..x109.L., data=dat)

fit.cd = lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL., data=dat)
fit.cw = lm(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data=dat)
fit.dw = lm(outcome ~ Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat)

fit.cdw = lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat)
fit.cdw

#-- Comparing models using anova
#anova(fit.c, fit.d)
#anova(fit.c, fit.w)
#anova(fit.d, fit.w)
anova(fit.c, fit.cd)
anova(fit.c, fit.cw)
anova(fit.d, fit.cd)
anova(fit.d, fit.dw)
anova(fit.w, fit.cw)
anova(fit.w, fit.dw) # only one that has same size of dataset
