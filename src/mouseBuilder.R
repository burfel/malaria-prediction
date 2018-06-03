library(caret)
library(doParallel)
registerDoParallel(detectCores() - 1)

setwd("~/Documents/IMPERIAL/PROJECTS/project2")

# read in files
mouseDat <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/rnaseq_stats.csv")

# add column "outcome" which gives the proportion of pathogen reads
mouseDat$outcome <- mouseDat$PF.mapped.reads / (mouseDat$PF.mapped.reads + mouseDat$MM.mapped.reads)
outcome_prop.mouse <- cbind(mouseDat$PF.mapped.reads, mouseDat$MM.mapped.reads) # 21x2 matrix

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
#png("GITHUB/shinyapp/img/mouseData_cov.png")
corrplot(MM, order = "hclust")
#dev.off()
corrplot(MM, order = "FPC")
corrplot(MM, order = "alphabet")
corrplot(MM, order = "hclust", addrect = 2)

fit.mouse <- lm(mouseDat$outcome ~ mouseDat$Parasitemia.., data=mouseDat)
summary(fit.mouse) # --- RESIDUALS NOT NORMALLY DISTRIBUTED --> USE LOGIT TRANSFORMATION
par(mfrow = c(2, 2))  
plot(fit.mouse)

glm.mouse <- glm(outcome_prop.mouse ~ mouseDat$Parasitemia.., family=quasibinomial, data=mouseDat)
summary(glm.mouse)
# MODEL: 0.68967 + 0.02428*mouseDat$Parasitemia.. -- only significant with alpha=0.05
glm.mouse.logit <- glm(outcome_prop.mouse ~ mouseDat$Parasitemia.., family=binomial(link = 'logit'), data=mouseDat)
summary(glm.mouse.logit)
# MODEL.log: -1.964e+00 + 6.550e-02*dat.nona$Percentage.parasitemia -- very significant when using logit transformation 
# <2e-16 ***

# MOUSE DATA -- SOME NON-LINEAR CORRELATIONS
