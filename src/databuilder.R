library(caret)
library(doParallel)
registerDoParallel(detectCores() - 1)

setwd("~/Documents/IMPERIAL/PROJECTS/project2")

# read in files
hg_pf <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/hg_pf_readcounts.csv")
supp <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/Supplementary_Dataset.csv", header=TRUE)

# merge data sets (by samples/ subjectID)
dat <- merge(supp, hg_pf, by.y = "samples", by.x = "Subject.ID")

library(car)
# add column "outcome" which gives the proportion of pathogen reads
dat$total_reads <- dat$pf_count + dat$hg_count
dat$outcome <- dat$pf_count / (dat$hg_count + dat$pf_count)
dat$outcome.logit <-  logit(dat$outcome, percents=TRUE)
#outcome_prop <- cbind(dat$pf_count, dat$hg_count)

# PLOT PROPORTION OF PATHOGEN READS, BOTH AS NORMAL AND A LOG SCALE -- FOR WEBSITE
par(mfrow = c(1, 2))
plot(dat$total_reads, dat$outcome, xlab = "Total number of reads", ylab = "Proportion reads that map to pathogen")
plot(log(dat$total_reads), dat$outcome, xlab = "Total number of reads", ylab = "Proportion reads that map to pathogen")

# NOT MEANINGFUL
# # add column "total.number.of.cells" 
# dat$total.number.of.cells <- dat$Total.White.Cell.Count..x109.L. + (1000 * dat$Red.blood.cell.count..x1012.L)

####---- do correlation first ------

# most important non-categorical variables 
#dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L, outcome))
dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Percentage.lymphocytes, Percentage.monocytes, outcome))
dat.nc.logit <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Percentage.lymphocytes, Percentage.monocytes, outcome.logit))

#reg.ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE)

#library(mice)
#tempData <- mice(dat,m=5)

# drop the samples that have blanks
dat.nona <- na.omit(dat) # ONLY 21 OBSERVATIONS ANYMORE
dat.nc.nona <- na.omit(dat.nc)
dat.nc.nona.logit <- na.omit(dat.nc.logit)
outcome_prop.nona <- cbind(dat.nona$pf_count, dat.nona$hg_count) # 21x2 matrix

# CORRELATION MATRIX
library(corrplot)
## corrplot 0.84 loaded
my_matrix <- dat.nona[,-c(1,2,16,23,24)]
M <- cor(my_matrix, method="pearson")
corrplot(M, method = "square") ### PLOT FOR WEBSITE
corrplot(M, order = "AOE") 
corrplot(M, order = "hclust") # FOR WEBSITE
corrplot(M, order = "FPC")
corrplot(M, order = "alphabet")
corrplot(M, order = "hclust", addrect = 2) # !!!!!!!!!

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
#--------------------------------------------------------------
#---see also: https://www.statmethods.net/stats/regression.html
#---FITTING THE MODEL-----------------------------------------
# Multiple Linear Regression Example
# on complete data set (samples with missing values removed), 42 samples

# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITHOUT TRANSFORMATION, ON WHOLE DATASET
fit.paras <- lm(dat$outcome ~ dat$Percentage.parasitemia, data=dat)
summary(fit.paras) # # show results: R^2: 0.245, F-stats: 15.28, p-value: 0.0003238
#summary(fit.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
par(mfrow = c(2, 2))  
plot(fit.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1)) 
hist(fit.paras$res, main="Residuals") # residuals not really Gaussian

# FOR WEBSITE --- ALSO PLOT!!!!!!!!
# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITHOUT TRANSFORMATION, ON COMPLETE SAMPLES
fit.nona.paras <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, data=dat.nona)
summary(fit.nona.paras) # # show results: R^2: 0.4078, F-stats: 13.09, p-value: 0.001834
#summary(fit.nona.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
par(mfrow = c(2, 2))  
plot(fit.nona.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1)) 
hist(fit.nona.paras$res,main="Residuals") # residuals not really Gaussian
# MODEL: 0.090267 + 0.013339*dat.nona$Percentage.parasitemia
ggplotRegression(fit.nona.paras)
# coef(fit.nona.paras)
# vcov(fit.nona.paras)
# predict(fit.nona.paras)
# fitted(fit.nona.paras)
# residuals(fit.nona.paras)


# # (1) SIMPLEST MODEL (JUST PARASITEMIA), WITHOUT TRANSFORMATION, ON WHOLE DATASET
# fit.paras <- lm(dat$outcome ~ dat$Parasite.density...µl., data=dat)
# summary(fit.paras) # # show results: R^2: 0.367, F-stats: 25.38
# #summary(fit.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# # plot the statistics
# par(mfrow = c(2, 2))  
# plot(fit.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
# #par(mfrow = c(1, 1)) 
# #hist(fit.paras$res, main="Residuals") # residuals not really Gaussian
#
# # (1) SIMPLEST MODEL (JUST PARASITEMIA), WITHOUT TRANSFORMATION, ON COMPLETE SAMPLES
# fit.nona.paras <- lm(dat.nona$outcome ~ dat.nona$Parasite.density...µl., data=dat.nona)
# summary(fit.nona.paras) # # show results: R^2: 0.3812, F-stats: 13.32
# #summary(fit.nona.paras)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# # plot the statistics
# par(mfrow = c(2, 2))  
# plot(fit.nona.paras) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
# #par(mfrow = c(1, 1)) 
# #hist(fit.nona.paras$res,main="Residuals") # residuals not really Gaussian
# MODEL: 8.037e-02 + 4.186e-07 * dat.nona$Parasite.density...µl


# # (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON WHOLE DATASET
# fit.paras.log <- lm(dat$outcome.logit ~ dat$Percentage.parasitemia, data=dat)
# summary(fit.paras.log) # # show results: R^2: 0.1733, F-stats: 10.22, p-value: 0.0026
# #summary(fit.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# # plot the statistics
# par(mfrow = c(2, 2))  
# plot(fit.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
# #par(mfrow = c(1, 1)) 
# #hist(fit.paras.log$res,main="Residuals") # residuals not really Gaussian

# FOR WEBSITE --- ALSO PLOT!!!!!!!! -- to compare to non-logit transformation
# (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON COMPLETE SAMPLES
fit.nona.paras.log <- lm(dat.nona$outcome.logit ~ dat.nona$Percentage.parasitemia, data=dat.nona)
summary(fit.nona.paras.log) # # show results: R^2: 0.2065, F-stats: 6.205, p-value: 0.02215 
summary(fit.nona.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# plot the statistics
par(mfrow = c(2, 2))  
plot(fit.nona.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
#par(mfrow = c(1, 1)) 
#hist(fit.nona.paras.log$res,main="Residuals") # residuals not really Gaussian
# MODEL: -7.50325 + 0.08221*dat.nona$Percentage.parasitemia
ggplotRegression(fit.nona.paras.log)


# # (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON WHOLE DATASET
# fit.paras.log <- lm(dat$outcome.logit ~ dat$Parasite.density...µl., data=dat)
# summary(fit.paras.log) # # show results: R^2: 0.2671, F-stats: 16.31, p-value: 0.0026
# #summary(fit.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.0002301
# # plot the statistics
# par(mfrow = c(2, 2))  
# plot(fit.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
# #par(mfrow = c(1, 1)) 
# #hist(fit.paras.log$res,main="Residuals") # residuals not really Gaussian
# 
# # FOR WEBSITE --- ALSO PLOT!!!!!!!! -- to compare to non-logit transformation
# # (1) SIMPLEST MODEL (JUST PARASITEMIA), WITH TRANSFORMATION, ON COMPLETE SAMPLES
# fit.nona.paras.log <- lm(dat.nona$outcome.logit ~ dat.nona$Parasite.density...µl., data=dat.nona)
# summary(fit.nona.paras.log) # # show results: R^2: 0.2509, F-stats: 7.7, p-value: 0.01207 
# summary(fit.nona.paras.log)$sigma^2 # estimated variance of residuals around a fitted line: 0.02366394
# # plot the statistics
# par(mfrow = c(2, 2))  
# plot(fit.nona.paras.log) # diagnostic plots: residuals do not have non-linear patterns, about Normally distributed (except for 35, 39)
# #par(mfrow = c(1, 1)) 
# #hist(fit.nona.paras.log$res,main="Residuals") # residuals not really Gaussian


# (2A) MORE COMPLEX MODEL ---- PLOT FOR WEBSITE
fit.nona.total <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.total) # show results: R^2: 0.38, F-stats: 7.13, p-value: 0.005242
#summary(fit.nona.total)$sigma^2 # estimated variance of residuals around a fitted line: 0.02268394

# plot the statistics, OUTLIERS 35, 39 -- both in UM group? -- kept them -- BUT MIGHT BE WORTH TRYING WITHOUT THEM
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.nona.total)  # Plot the model information/ 
# diagnostic plots to CHECK ASSUMPTIONS FOR LINEAR REGRESSION:
# - residuals do not have non-linear patterns
# - residuals about Normally distributed (except for 35, 39)
# - residuals about homoscedastic
# - residuals still within Cook distance (but you can see the dotted lines)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#hist(fit.nona.total$res,main="Residuals") # residuals not really Gaussian
# MODEL: 0.204181 + 0.011821*dat.nona$Percentage.parasitemia + (-0.010121)*dat.nona$Total.White.Cell.Count..x109.L.
ggplotRegression(fit.nona.total) #---NEED 3-DIM PLOT
#ggplotRegression2(fit.nona.total) ## NEEDS FIXING!!!!!!

# likelihood ratio test of nested models
#lrtest(fit.nona.paras, fit.nona.total)

# # (2) MORE COMPLEX MODEL, WITH LOGIT TRANSFORMATION --- NOT ANY GOOD
# fit.nona.total.logit <- lm(dat.nona$outcome.logit ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
# summary(fit.nona.total.logit) # show results: R^2: 0.33, F-stats: 5.976, p-value: 0.01022
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.total.logit)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.total.logit$res,main="Residuals") # residuals not really Gaussian

# (2B) MORE COMPLEX MODEL --- just with lymphocytes ----FOR WEBSITE
fit.nona.L <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, data=dat.nona)
summary(fit.nona.L) # show results: R^2: 0.3425, F-stats: 6.209, p-value: 0.008899
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.nona.L)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#hist(fit.nona.L$res,main="Residuals") # residuals not really Gaussian
# MODEL: 0.0829492 + 0.0130663*dat.nona$Percentage.parasitemia + 0.0003475*dat.nona$Percentage.lymphocytes
ggplotRegression(fit.nona.L)

# (2B) MORE COMPLEX MODEL --- just with monocytes ----FOR WEBSITE
fit.nona.M <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.monocytes, data=dat.nona)
summary(fit.nona.M) # show results: R^2: 0.3421, F-stats: 6.199, p-value: 0.00895
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.nona.M)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#hist(fit.nona.M$res,main="Residuals") # residuals not really Gaussian
# MODEL: 0.0867926 + 0.0132895*dat.nona$Percentage.parasitemia + 0.0006445*dat.nona$Percentage.monocytes
ggplotRegression(fit.nona.M)

# (2B) MORE COMPLEX MODEL --- just with neutrophils ----FOR WEBSITE
fit.nona.N <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils, data=dat.nona)
summary(fit.nona.N) # show results: R^2: 0.3423, F-stats: 6.204, p-value: 0.008925
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(fit.nona.N)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#hist(fit.nona.N$res,main="Residuals") # residuals not really Gaussian
# MODEL: 0.1053476 + 0.0131751*dat.nona$Percentage.parasitemia + (-0.0002101)*dat.nona$Percentage.neutrophils
ggplotRegression(fit.nona.N)

# # (2B) MORE COMPLEX MODEL -- with Neutrophil and monocyte percentages ---- BEST --- FOR WEBSITE
# fit.nona.NM <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils + dat.nona$Percentage.monocytes, data=dat.nona)
# summary(fit.nona.NM) # show results: R^2: 0.3037, F-stats: 3.908, p-value: 0.02723
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.NM)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.NM$res,main="Residuals") # residuals not really Gaussian
# 
# 
# # (2) MORE COMPLEX MODEL -- WHY SO INCREDIBLY GOOD???? -- only a few samples
# fit.nona.LM <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, + dat.nona$Percentage.monocytes, data=dat.nona)
# summary(fit.nona.LM) # show results: R^2: 0.8912 , F-stats: 82.88, p-value: 8.299e-10
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.LM)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.LM$res,main="Residuals") # residuals not really Gaussian
# 
# # (2) MORE COMPLEX MODEL
# fit.nona.LN <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, + dat.nona$Percentage.neutrophils, data=dat.nona)
# summary(fit.nona.LN) # show results: R^2: 0.8772, F-stats: 72.44, p-value: 2.458e-09
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.LN)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.LN$res,main="Residuals") # residuals not really Gaussian
# 
# 
# # (2B) MORE COMPLEX MODEL, LOGIT TRANSFORMATION -- with Neutrophil and monocyte percentages ---- BEST --- FOR WEBSITE
# fit.nona.NM.logit <- lm(dat.nona$outcome.logit ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils + dat.nona$Percentage.monocytes, data=dat.nona)
# summary(fit.nona.NM.logit) # show results: R^2:0.1133, F-stats: 1.852, p-value: 0.1761
# par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# plot(fit.nona.NM.logit)  # Plot the model information
# par(mfrow = c(1, 1))  # Return plotting panel to 1 section
# #hist(fit.nona.NM.logit$res,main="Residuals") # residuals not really Gaussian


#plot(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, data = dat.nona)
#abline(fit.nona.total)


####----- GLM
# https://stats.stackexchange.com/questions/38201/problems-plotting-glm-data-of-binomial-proportional-data?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# GLM
#GM1<-glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=binomial (logit), data=dat.nona)
# plot(BPT,p1,col="black",pch=1,main="Relationship a",xlab="Browsing pressure", ylab="Moose Damage Survey")
# range(log(density))
# #[1] 0.000000 6.095825
# xv<-seq(0,6,0.1)
# range(xv)
# #[1] 0 6
# lines(BPT,predict(GM1,type="response"))
# xv<-seq(0,0.7,length.out = length(BPT))
# lines(xv,predict(GM1, list(BPT = xv), type="response"))
#predict(GM1,list(density=exp(xv)),type="response")

# glm_paras <- glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, family=quasibinomial, data=dat.nona)
# #glm_paras_logit <- glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia, family=quasibinomial (logit), data=dat.nona)
# glm_total <-glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nona)
# #glm_total_logit <-glm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial (logit), data=dat.nona)
# summary(glm_paras)
# # MODEL1: -1.97632 + 0.06533*dat.nona$Percentage.parasitemia
# #summary(glm_paras_logit)
# summary(glm_total)
# # MODEL2: -1.13941 + 0.05487*dat.nona$Percentage.parasitemia + (-0.07721)*dat.nona$Total.White.Cell.Count..x109.L.
# #summary(glm_total_logit)

# DO WE NEED LOGIT TRANSOFRMATION?
model <- glm(outcome_prop ~ dat$total_reads, family=binomial, data=dat.nona)
summary(model)

model_log <- glm(outcome_prop ~ log(dat$total_reads), family=binomial, data=dat.nona)
summary(model_log) ## ---> LOGIT TRANSFORMATION / LOGIT LINK NOT NECESSARY

# (1) GLM
glm_paras2 <- glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia, family=quasibinomial, data=dat.nona)
summary(glm_paras2)
# MODEL: -1.96396 + 0.06550*dat.nona$Percentage.parasitemia

# (2a) GLM
glm_total2 <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., family=quasibinomial, data=dat.nona)
summary(glm_total2)
# MODEL: -1.11189 + 0.05324*dat.nona$Percentage.parasitemia + (-0.07415)*dat.nona$Total.White.Cell.Count..x109.L.

# (2b) GLM -- lympho
glm_lympho2 <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.lymphocytes, family=quasibinomial, data=dat.nona)
summary(glm_lympho2)
# MODEL: -1.77229 + 0.07545*dat.nona$Percentage.parasitemia + (-0.01056)*dat.nona$Percentage.lymphocytes

# (2b) GLM -- mono
glm_mono2 <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.monocytes, family=quasibinomial, data=dat.nona)
summary(glm_mono2)
# MODEL: -2.02525 + 0.06461*dat.nona$Percentage.parasitemia + 0.01143*dat.nona$Percentage.monocytes

# (2b) GLM -- neutro
glm_neutro2 <-glm(outcome_prop.nona ~ dat.nona$Percentage.parasitemia + dat.nona$Percentage.neutrophils, family=quasibinomial, data=dat.nona)
summary(glm_neutro2)
# MODEL: -2.581276 + 0.073414 + 0.008204

# We now can compare the two models as before using ANOVA. In the case of binary data, we need to do a Chi-squared test.
anova(glm_paras, glm_total, test = "Chi")
#anova(glm_paras_logit, glm_total_logit, test = "Chi")
anova(glm_paras2, glm_total2, test = "Chi")


# # run the two models MODEL1, MODEL2
# # set up a two-panel plotting area
# par(mfrow = c(1, 2))
# # use the predict() function to create the fitted y values
# ## 1. parsitemia
# # create a sequence of x values
# xv <- seq(0, 9, 0.01)
# # y values
# yv <- predict(glm_paras, list(area = xv), type = "response")
# # plot the raw data
# plot(dat.nona$Percentage.parasitemia, dat.nona$outcome)
# # use the lines function to add the fitted lines
# lines(xv, yv)
# ## 2. parasitemia and white blood cells
# xv2 <- seq(0, 10, 0.1)
# yv2 <- predict(glm_total, list(isolation = xv2), type = "response")
# plot(dat.nona$Total.White.Cell.Count..x109.L., dat.nona$outcome)
# lines(xv2, yv2)



# https://www.theanalysisfactor.com/r-tutorial-glm1/
# http://www.simonqueenborough.info/R/stats-basic/glm.html
# http://www.simonqueenborough.info/R/statistics/glm-binomial 



# ------ BETA FUNCTIONS
# Zeileis, Kleiber, and Jackman (2018): generalized count data regression
# model shares some properties with generalized linear models (GLMs, McCullagh and Nelder 1989) like linear predictor, link function, dispersion parameter, 
# but it is not a special case of this framework. 

library(betareg)

set.seed(123)

beta <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
beta_log <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona, link = "log")
#beta_logit <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona, link = "logit")
beta_loglog <- betareg(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona, link = "loglog")
summary(beta)
#summary(beta_log)
summary(beta_logit)
summary(beta_loglog) # improves pseudo R^2 of the model
#summary(beta_loglog)$pseudo.r.squared
# CONCLUSION: THERE ARE A FEW OBSERVATIONS ABOVE THE DIAGONAL WHERE THE LOG-LOG LINK FITS BETTER THAN THE LOGIT LINK..
# WHEREAS THERE ARE FEWER SUCH OBSERVATIONS BELOW THE DIAGONAL.

# NOT USEFUL, DOES NOT REDUCE NUMBER OF ITERATIONS
#beta_loglog2 <- update(beta_loglog, link.phi = "log")
#summary(beta_loglog2)
#summary(beta_loglog)

# NOT USEFUL
#plot(beta, type="pearson")
#plot(beta, which = 5, type = "deviance", sub.caption = "")

AIC(beta, beta_logit, beta_loglog)
BIC(beta, beta_logit, beta_loglog)

# Compare means of a likelihood-ratio test to confirm results
# ie testing H_0 of equidispersion against a specific alternative of variable dispersion
lrtest(beta, beta_logit, beta_loglog)

# NOT IMPORTANT
# # DOES MODEL EXHIBIT HETEROSKEDASTICITY? use studentised Breusch and Pagan (1979) test of Koenker (1981)
# library(lmtest)
# bptest(fit.nona.paras)
# bptest(fit.nona.total)

#####-------- MULTINOMIAL MODEL
library(nnet)
#multinom

##########################################

# PLOT the best linear regression model
library(ggplot2)
ggplot(dat.nona, aes(x = dat.nona$Percentage.parasitemia, y = dat.nona$Total.White.Cell.Count..x109.L.)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# ADJUST NAMES OF AXES!!!!
# Write a function that also returns statistics
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  #ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  ggplot(fit$model, aes_string(x = "Percentage of parasitemia", y = "Percentage of reads that map to pathogen") + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R^2 = ",signif(summary(fit)$adj.r.squared, 5),
                       #"Mult R^2 = ",signif(summary(fit)$mult.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
  }

# ggplotRegression2 <- function (fit) {
#   
#   require(ggplot2)
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[3], y = names(fit$model)[1])) + 
#            geom_point() +
#            stat_smooth(method = "lm", col = "red") +
#            labs(title = paste("Adj R^2 = ",signif(summary(fit)$adj.r.squared, 5),
#                               #"Mult R^2 = ",signif(summary(fit)$mult.r.squared, 5),
#                               "Intercept =",signif(fit$coef[[1]],5 ),
#                               " Slope =",signif(fit$coef[[3]], 5),
#                               " P =",signif(summary(fit)$coef[2,4], 5)))
#   }


ggplotRegression(fit.nona.paras) # for most simple model (parasitemia percentage)
#par(new=TRUE)
#ggplotRegression(fit.nona.total) # the same plot

# Are the residuals of the linear regression model homoscedastic, ie the RVs have the same finite variance?
# SIZE AND COLOR #https://drsimonj.svbtle.com/visualising-residuals
# Same coloring as above, size corresponding as well


d <- dat.nona
d$predicted <- predict(fit.nona.total) 
d$residuals <- residuals(fit.nona.total)
library(dplyr)
d %>% select(outcome, predicted, residuals) %>% head() # quick look at the actual, predicted and residual values
#head(d)
# ADD LEGEND HERE
ggplot(d, aes(x = Percentage.parasitemia, y = outcome)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = Percentage.parasitemia, yend = predicted), alpha = .2) +
  
  # > Color AND size adjustments made here...
  geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # <
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

# MORE COMPLEX MODEL
d <- dat.nona
d$predicted <- predict(fit.nona.total) 
d$residuals <- residuals(fit.nona.total)
library(dplyr)
d %>% select(outcome, predicted, residuals) %>% head() # quick look at the actual, predicted and residual values
# Let's crank up the complexity! PLOTTING a multiple linear regression model
library(tidyr)
#d2 <- subset(d, select = c(Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L, total.number.of.cells, outcome, predicted, residuals))
# just percentage parasetemia, total number of white blood cells
d2 <- subset(d, select = c(d$Percentage.parasitemia, d$Total.White.Cell.Count..x109.L., d$outcome, d$predicted, d$residuals))
d2 %>% 
  gather(key = "iv", value = "x", -outcome, -predicted, -residuals) %>%  # Get data into shape
  ggplot(aes(x = x, y = outcome)) +  # Note use of `x` here and next line
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free_x") +  # Split panels here by `iv`
  theme_bw()


# TO MAKE PLOT INTERACTIVE. PACKAGES NOT COMPATIBLE -- NEEDS FIXING!
# require(ggiraph)
# require(ggiraphExtra)
# require(plyr)
# ggPredict(fit.nona.total,se=TRUE,interactive=TRUE)

# (2b)
fit.nona.white <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.white) # show results: R^2: 0.442
par(mfrow = c(2, 2))  
plot(fit.nona.white)  

# (3a)
fit.nona.red <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Red.blood.cell.count..x1012.L + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.red) # show results: R^2: 0.4671
par(mfrow = c(2, 2))  
plot(fit.nona.red) 

# (3b)
fit.nona.hemo <- lm(dat.nona$outcome ~ dat.nona$Percentage.parasitemia + dat.nona$Hemoglobin.concentration..g.dL. + dat.nona$Total.White.Cell.Count..x109.L., data=dat.nona)
summary(fit.nona.hemo) # show results: R^2: 0.4527
par(mfrow = c(2, 2))  
plot(fit.nona.hemo) 


# # on original data set, 46 samples
# # ---------WHAT HAPPENS TO MISSING VALUES HERE? HOW ARE THEY IMPLICITLY IMPUTED??
# fit <- lm(dat$outcome ~ dat$Percentage.parasitemia + dat$Hemoglobin.concentration..g.dL. + dat$Total.White.Cell.Count..x109.L., data=dat)
# summary(fit) # show results: R^2: 0.37

######--------------------------------NO LONGER NECESSARY--------------------------------------------------
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
#---CROSSVALIDATION---------------
#cv.lm(df = houseprices, form.lm = formula(sale.price ~ area), m=3, dots = 
        FALSE, seed=29, plotit=TRUE, printit=TRUE)
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

######--------------------------------NO LONGER NECESSARY--------------------------------------------------

######--------------------------------NO LONGER NECESSARY--------------------------------------------------
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
######--------------------------------NO LONGER NECESSARY--------------------------------------------------


######--------------------------------NO LONGER NECESSARY--------------------------------------------------
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
# qq plot for studentised residual
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
######--------------------------------NO LONGER NECESSARY--------------------------------------------------

######--------------------------------NO LONGER NECESSARY--------------------------------------------------
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
       subtitle="Percentage parasitemia vs outcome", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#--------------------------------------------------------------
#--------------------------------------------------------------
######--------------------------------NO LONGER NECESSARY--------------------------------------------------


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
png("GITHUB/shinyapp/img/missingData.png")
mice_plot <- aggr(dat, col=c('navyblue', 'yellow'), numbers=TRUE, sortVars=TRUE,
                  labels=names(dat), cex.axis=.7, gap=3, ylab=c("Missing data", "Pattern"))
dev.off()
# There are 45.7% values in the data set with no missing values 
# 17.4% missing values in parasite clone...

##--- 1. Impute missing values

#imputed_dat <- mice(dat, m=5, maxit=50, method='pmm', seed=500)
## TODO: TRY VARIOUS NUMBER OF ITERATIONS (MAXIT) AND METHODS
## Note: multiple imputation helps to reduce bias and increase efficiency

# problem here: singular matrix (some variables highly collinear)
# --> exclude some variables that are not essential to all the analyses we are going to conduct

##--- remove categorical variables
#dat.nc <- subset(dat, select = -c(Sex, Ethnicity, Sickle.cell.screen))
# SINGULAR MATRIX --> CHOOSE LESS VARIABLES
#dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, total.number.of.cells, Total.White.Cell.Count..x109.L., Monocyte.count...x109.L., Lymphocyte.count...x109.L., Red.blood.cell.count..x1012.L, Hemoglobin.concentration..g.dL., Parasite.density...µl., Parasite.clones, outcome))
dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, total.number.of.cells, Monocyte.count...x109.L., Lymphocyte.count...x109.L., outcome))

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


######--------------------------------NO LONGER NECESSARY--------------------------------------------------
#---2. PREDICTIVE / LINEAR REGRESSION MODEL
# build models on all 5 datasets
fit <- with(data = dat, exp = lm(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L. + Red.blood.cell.count..x1012.L))
summary(fit)

# combine results of all 5 models:
#combine <- pool(fit)

#---see also: https://www.r-bloggers.com/correlation-and-linear-regression/
#-- linear candidate models for the data (see Ada's slides):
fit.0 = lm(outcome ~ 1, data=dat) # just intercept
summary(fit.0)

fit.c = lm(outcome ~ Percentage.parasitemia, data=dat)
summary(fit.c) # R^2 = 0.26
fit.d = lm(outcome ~ Hemoglobin.concentration..g.dL., data=dat)
summary(fit.d) # R^2 = 0.26
fit.w = lm(outcome ~ Total.White.Cell.Count..x109.L., data=dat)
summary(fit.w) # R^2 = 0.06

fit.cd = lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL., data=dat)
summary(fit.cd) # R^2 = 0.35
fit.cw = lm(outcome ~ Percentage.parasitemia + Total.White.Cell.Count..x109.L., data=dat)
summary(fit.cw) # R^2 = 0.30
fit.dw = lm(outcome ~ Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat)
summary(fit.dw) # R^2 = 0.078

fit.cdw = lm(outcome ~ Percentage.parasitemia + Hemoglobin.concentration..g.dL. + Total.White.Cell.Count..x109.L., data=dat)
summary(fit.cdw) # R^2 = 0.37

# with parasite clones instead of hemoglobin concentration
fit.p = lm(outcome ~ Parasite.clones, data=dat)
summary(fit.p) # R^2 = 0.086
fit.pc = lm(outcome ~ Percentage.parasitemia + Parasite.clones, data=dat)
summary(fit.pc) # R^2 = 0.43
fit.pw = lm(outcome ~ Parasite.clones + Total.White.Cell.Count..x109.L., data=dat)
summary(fit.pw) # R^2 = 0.158

fit.pcw = lm(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., data=dat)
summary(fit.pcw) # R^2 = 0.48
######--------------------------------NO LONGER NECESSARY--------------------------------------------------

# check that the models assumptions are met, 
# indeed linear models make a few assumptions on the data, 
# the first one is that your data are normally distributed, 
# the second one is that the variance in y is homogeneous over 
# all x values (sometimes called homoscedasticity) and 
# independence which means that a y value at a certain x value 
# should not influence other y values.

# ---plotting three best models
# * graphs first column: look at variance homogeneity amongst others
#   (we see no pattern in dots but random clouds of points 
#   --> homogeneity assumption not violated)
# * graph top right: checks normality assumption
#   (if data normally distributed, points should fall in straight line)
# * graph bottom right: shows how each y influences the model,
#   each point is removed at a time and the new model is compared to the one with the point,
#   if the point is very influential then it will have a high leverage value.
#   POINTS WITH TO HIGH LEVERAGE VALUE SHOULD BE REMOVED FROM THE DATASET 
#   (to remove their outlying effect on the model).
# here: remove 39, maybe 3, 2.
par(mfrow=c(2,2))
plot(fit.cdw)

par(mfrow=c(2,2))
plot(fit.pc)

par(mfrow=c(2,2))
plot(fit.pcw)

######--------------------------------CONTINUE ON FROM HERE--------------------------------------------------

# 3. TRANSFORMING AND REMOVING OUTLIERS FROM THE DATA
#----Transforming the data (for non-normal or heterogeneous data)
#-----usually a trial and error process

library(gtools)
library(car)
completeData$outcome <- completeDat$outcome
completeData$outcome
#completeData$outcome<-log(completeData$outcome)
completeData$outcomeLogit <- logit(completeData$outcome, min=0, max=0)
completeData$outcomeLogit
cbind(completeData$outcome, completeData$outcomeLogit)
plot(outcome~Percentage.parasitemia,completeData)

#-- Comparing models using anova
# used to compare multiple models
# does an f-test to compare 2 or more nested models
#anova(fit.c, fit.d)
#anova(fit.c, fit.w)
#anova(fit.d, fit.w)
#anova(fit.c, fit.cd)
#anova(fit.c, fit.cw)
#anova(fit.d, fit.cd)
#anova(fit.d, fit.dw)
#anova(fit.w, fit.cw)
anova(fit.w, fit.dw) # only one that has same size of dataset

library(car)
Anova(fit.w)
Anova(fit.w, fit.dw)

drop1(fit.c, test="F")
drop1(fit.d, test="F")
drop1(fit.w, test="F")
drop1(fit.cd, test="F")
drop1(fit.cw, test="F")
drop1(fit.dw, test="F")
drop1(fit.cdw, test="F")

## logistic regression 
## GENERALISED LINEAR MODEL, ie robust regression method on the raw, untransformed values
library(ISLR)
glm.fit <- glm(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., completeDat, family=binomial)
summary(glm.fit)
glm.fit2 <- glm(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., completeDat, family=quasibinomial())
summary(glm.fit2)
# null deviance (the deviance just for the mean) and 
# the residual deviance (the deviance for the model with all the predictors)

# make predictions on the training data that we use to fit the model
# returns vector of fitted probabilities
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:5] # look at the first 5 probabilities

# now define what coverage is enough, eg elem [0.2, 0.8], st we have enough host and pathogen maps
#glm.pred <- ifelse((glm.probs > 0.2 && glm.probs < 0.8) , "Enough", "Not enough")

#attach(completeDat)
#table(glm.pred, outcome)

#mean(glm.pred == outcome)

# GENERALISED LINEAR MODEL (extension of the general linear model) ON BEST MODEL FROM ABOVE
# unlike general linear model does not require that response variable is normally distributed
# --> try different link functions (~transformation of response variable) for different 
#     relationships between linear model and response variable (eg inverse, logit, log etc)

library(nnet)
#completeData$Percentage.parasitemia <- relevel(completeData$Percentage.parasitemia, ref = "parasetemia")
test <- multinom(outcome ~ Percentage.parasitemia + Parasite.clones + Total.White.Cell.Count..x109.L., data=completeDat)
summary(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## extract the coefficients from the model and exponentiate
exp(coef(test))

head(pp <- fitted(test))

#dses <- data.frame(ses = c("low", "middle", "high"), write = mean(completeDat$outcome))
#predict(test, newdata = dses, "probs")

