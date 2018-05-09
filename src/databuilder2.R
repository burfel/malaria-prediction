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

imputed_dat <- mice(dat.nc, m=5, maxit=50, method='pmm', seed=500)

imputed_dat$imp


#########
# Kernel Density Plot to view the distribution of variables
para <- density(completeDat$Percentage.parasitemia) # returns the density data
plot(para) # plots the results 
wcc <- density(completeDat$Total.White.Cell.Count..x109.L.) 
plot(wcc) 
rcc <- density(completeDat$Red.blood.cell.count..x1012.L) 
plot(rcc) 
##########
# CORRELATION

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

corrplot(M, order = "AOE") ### PLOT FOR WEBSITE
corrplot(M, order = "hclust")
corrplot(M, order = "FPC")
corrplot(M, order = "alphabet")
corrplot(M, order = "hclust", addrect = 2)

# matrix of the p-value of the correlation
p.mat <- cor.mtest(la)$p
head(p.mat[, 1:5])
# Specialized the insignificant value according to the significant level
corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat, sig.level = 0.01)

# --> recommendation for predictors:
# 1. percentage parasitemia || parasite density
# 2. total white cell count || neutrophil count
# 3. parasite clones
# [4. red blood cell count || hemoglobin concentration]

library(AppliedPredictiveModeling)
data(twoClassData)
