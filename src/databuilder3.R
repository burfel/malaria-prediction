library(caret)
library(doParallel)
registerDoParallel(detectCores() - 1)

#setwd("~/Documents/IMPERIAL/PROJECTS/project2")

#--- run databuilder.R ---------------------------------------------------------------------------------

# read in files
devStages <- read.csv("~/Documents/IMPERIAL/PROJECTS/project2/data/pf_SPVs.csv")

# merge data sets (by samples/ subjectID)
dat_devStages <- merge(dat, devStages, by.y = "samples", by.x = "Subject.ID")

# for (row in c(1:43)){
#   for (year in dat_devStages[1,28:30]){
#     plot(year, dat_devStages[row, year])
#   }
# }

# most important non-categorical variables 
#dat.nc <- subset(dat, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Red.blood.cell.count..x1012.L, outcome))
dat_devStages.nc <- subset(dat_devStages, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Percentage.lymphocytes, Percentage.monocytes, Percentage.neutrophils, outcome))
dat_devStages.nc.logit <- subset(dat_devStages, select = c(Subject.ID, Percentage.parasitemia, Total.White.Cell.Count..x109.L., Percentage.lymphocytes, Percentage.monocytes, Percentage.neutrophils, outcome.logit))

# drop the samples that have blanks
dat_devStages.nona <- na.omit(dat_devStages) # ONLY 21 OBSERVATIONS ANYMORE
dat_devStages.nc.nona <- na.omit(dat_devStages.nc)
dat.nc.nona.logit <- na.omit(dat_devStages.nc.logit)
#outcome_prop.nona <- cbind(dat.nona$pf_count, dat.nona$hg_count) # 21x2 matrix

# CORRPLOT
my_matrix2 <- dat_devStages.nona[,-c(1,2,16,23,24)]
M2 <- cor(my_matrix2, method="pearson")
corrplot(M2, method = "square") ### PLOT FOR WEBSITE
corrplot(M2, order = "AOE") 
corrplot(M2, order = "hclust") # FOR WEBSITE
corrplot(M2, order = "FPC")
corrplot(M2, order = "alphabet")
dev.new(width=5, height=4)
corrplot(M2, order = "hclust", addrect = 2) # !!!!!!!!!


M2sub <- subset(M2, select = c(X0hour, X24hour, X48hour, Gam5))
#dev.new(width=10, height=10)
heatmap(M2sub, legend=c("col")) # !!!!!!!!!
# heatmap(M2sub, Rowv=TRUE, Colv=TRUE,
#         sideColors=NULL, col=heat.colors(12),
#         base=0.05, margins=c(5, 5), main=NULL, dendScale=1,
#         barScale=1, legend=c("none", "col"))

#print(levelplot(M2sub, col.regions=heat.colors))
# heatmap.2(as.matrix(M2sub), col=redgreen(75), 
#           density.info="none", trace="none", dendrogram=c("row"), 
#           symm=F,symkey=T,symbreaks=T, scale="none") 

# #acf(dat_devStages.nona[2,28:30])
# for (i in c(1:20)){
# plot(c('0h', '24h', '48h'), dat_devStages.nona[i,28:30])
# }
# 
# plot(c('0h', '24h', '48h'), dat_devStages.nona[1,28:30])


# PLOT TIME POINTS AGAINST DIFFERENT COVARIATES
#M2subsub <- subset(M2sub, select = c(M2sub$))

plot.ts(t(M2sub)[,3:7])
plot.ts(t(M2sub)[,8:13])
plot.ts(t(M2sub)[,c(14,15,16,17,18,19,21,22)])
plot.ts(t(M2sub)[,23:26])
# title(main="main title", sub="sub-title", 
#       ylab="x-axis label", xlab=c("0", "24", "48"))


# INTERESTING PLOTS 
plot.ts(t(M2sub)[,c(3,5,6,8,10,12)])
plot.ts(t(M2sub)[,c(13,14,15,16,17,18)])
plot.ts(t(M2sub)[,c(19,21,22)])
plot.ts(t(M2sub)[,23:26])
                 