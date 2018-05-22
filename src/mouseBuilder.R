library(caret)
library(doParallel)
registerDoParallel(detectCores() - 1)

setwd("~/Documents/IMPERIAL/PROJECTS/project2")

# read in files
mouseDat <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/rnaseq_stats.csv")

# add column "outcome" which gives the proportion of pathogen reads
mouseDat$outcome <- mouseDat$PF.mapped.reads / (mouseDat$PF.mapped.reads + mouseDat$MM.mapped.reads)

rel <- mouseDat[, -2, -3] 
rel <- rel[, -4, -5]
rel <- rel[, -2, -3]
rel <- rel[,-1,-2]
rel <- rel[,-1,-2]
rel
cor(rel, method="pearson") # parasetemia and outcome: 0.29
MM <- cor(rel, method="spearman") # parasetemia and outcome: 0.41

library(corrplot)
corrplot(MM, order = "AOE") ### PLOT FOR WEBSITE

#png("GITHUB/shinyapp/img/test.png",width=3.25,height=3.25,units="in",res=1200)
png("GITHUB/shinyapp/img/mouseData_cov.png")
corrplot(MM, order = "hclust")
dev.off()
corrplot(MM, order = "FPC")
corrplot(MM, order = "alphabet")
corrplot(MM, order = "hclust", addrect = 2)

fit.mouse <- lm(mouseDat$outcome ~ mouseDat$Parasitemia.., data=mouseDat)
summary(fit.mouse)
par(mfrow = c(2, 2))  
plot(fit.mouse)

# MOUSE DATA -- SOME NON-LINEAR CORRELATIONS
