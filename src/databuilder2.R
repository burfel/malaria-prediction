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

dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L))

# 0. Impute missing values
library(Amelia)
library(mlbench)
missmap(dat, col=c("blue", "red"), legend=FALSE)
missmap(dat.nc, col=c("blue", "red"), legend=FALSE)

imputed_dat <- mice(dat.nc, m=5, maxit=50, method='pmm', seed=500)
# should we use parasite.clone at all?

imputed_dat$imp


#########
# 0. Kernel Density Plot to view the distribution of variables
para <- density(completeDat$Percentage.parasitemia) # returns the density data
plot(para) # plots the results 
wcc <- density(completeDat$Total.White.Cell.Count..x109.L.) 
plot(wcc) 
rcc <- density(completeDat$Red.blood.cell.count..x1012.L) 
plot(rcc) 

par(mfrow=c(2,5))
for(i in 2:5) {
  boxplot(completeDat[,i], main=names(completeDat)[i])
}
par(mfrow=c(2,5))
for(i in 2:5) {
  hist(completeDat[,i], main=names(completeDat)[i])
}

##########
# 1. CORRELATION -- nice first step to data exploration
#                before going into more serious analysis and to select variable that might be of interest

completeDat2 <- completeDat
completeDat2$Total.Cell.Count <- completeDat$Total.White.Cell.Count..x109.L. + completeDat$Red.blood.cell.count..x1012.L
cor(completeDat[,-1],method="pearson")
cor(completeDat[,-1],method="spearman")

# correlation of whole matrix
la <- dat.nona[,-1,-2]
la <- la[,-1]
la <- la[,-14]
cor(la, method="pearson")
cor(la, method="spearman")
# parasite density highest correlation (0.65) to outcome
# percentage parasitemia second highest correlation (0.55) to outcome
# neutrophil count approx same correlation (0.47) as total white cell count to outcome
# hemoglobin very low correlation to outcome (0.03), also red blood cell count (0.066)
# mean cell volume (0.07)
# parasite clones (0.32)
# REST BELOW 5%

library(corrplot)
## corrplot 0.84 loaded
M <- cor(la, method="pearson")
#corrplot(M, method = "square") ### PLOT FOR WEBSITE
#corrplot(M, method = "circle")
#corrplot(M, method = "number")
#corrplot(M, method = "shade")
corrplot(M, method = "color")

Mp <- cor(completeDat2[,-1], method="pearson")
Ms <- cor(completeDat2[,-1], method="spearman")


corrplot(M, order = "AOE") ### PLOT FOR WEBSITE
corrplot(M, order = "hclust")
corrplot(M, order = "FPC")
corrplot(M, order = "alphabet")
corrplot(M, order = "hclust", addrect = 2)

corrplot(Mp, order = "AOE") ### PLOT FOR WEBSITE
corrplot(Mp, order = "hclust")
corrplot(Mp, order = "FPC")
corrplot(Mp, order = "alphabet")
corrplot(Mp, order = "hclust", addrect = 2)

corrplot(Ms, order = "AOE") ### PLOT FOR WEBSITE
corrplot(Ms, order = "hclust")
corrplot(Ms, order = "FPC")
corrplot(Ms, order = "alphabet")
corrplot(Ms, order = "hclust", addrect = 2)

# matrix of the p-value of the correlation
p.mat <- cor.mtest(la)$p
head(p.mat[, 1:5])
# Specialised the insignificant value according to the significant level
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.01)

# --> recommendation for predictors:
# 1. percentage parasitemia || parasite density
# 2. total white cell count || neutrophil count
# 3. parasite clones
# [4. red blood cell count || hemoglobin concentration]

# PLOT THE DATA
pairs(completeDat, col=completeDat$outcome)

#library(caret)
#x <- completeDat[,2:5]
#y <- completeDat[,6]
#scales <- list(x=list(relation="free"), y=list(relation="free"))
#featurePlot(x=x, y=y, plot="density", scales=scales)

# the next step is to model the variable relationship and 
# the most basic models are bivariate linear regression that 
# put the relation between the response variable and the predictor variable 
# into equation and testing this using the summary and anova() function. 

# Since linear regression make several assumptions on the data before interpreting 
# the results of the model plot and look if the data are normally distributed, that 
# the variance is homogeneous (no pattern in the residuals~fitted values plot) and 
# when necessary remove outliers.

# next step: multiple predictors, generalised linear models

###############
library(AppliedPredictiveModeling)
data(twoClassData)
