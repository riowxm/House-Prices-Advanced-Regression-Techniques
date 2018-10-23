mydata <- read.csv("/Users/xiaomengwei/Desktop/Machine learning/project_house price/BAIT509_Project_XIAOMENG_JINYE_EGOR/original dataset/train.csv")

suppressPackageStartupMessages(library(randomForest))
set.seed(40)
library(magrittr)
library(dplyr)
library(ggplot2)

na.cols <- which(colSums(is.na(mydata)) > 0)
sort(colSums(sapply(mydata[na.cols], is.na)), decreasing = TRUE)

allSet <- mydata

levels(allSet$Fence) <- c(levels(allSet$Fence),"None")

allSet$Fence[is.na(allSet$Fence)] <- "None"

summary(allSet)
levels(allSet$Alley) <- c(levels(allSet$Alley),"None")
allSet$Alley[is.na(allSet$Alley)] <- "None"

levels(allSet$MiscFeature) <- c(levels(allSet$MiscFeature),"None")
allSet$MiscFeature[is.na(allSet$MiscFeature)] <- "None"


# check number of houses with missing (NA) Fireplace quality and zero fireplaces.
fpNoQul <- allSet[is.na(allSet$FireplaceQu) & allSet$Fireplaces > 0 ,c("Fireplaces","FireplaceQu")]

# such houses should have "None" values in the FireplaceQu feature.
levels(allSet$FireplaceQu) <- c(levels(allSet$FireplaceQu),"None")
allSet$FireplaceQu[is.na(allSet$FireplaceQu) & allSet$Fireplaces == 0] <- "None"


# check that houses with no fireplaces all have "None" value for FireplaceQu
fp <- allSet[allSet$Fireplaces == 0 ,c("Fireplaces","FireplaceQu")]
table(fp)

allSet$MasVnrType[is.na(allSet$MasVnrType)] <- "None"
allSet$MasVnrArea[is.na(allSet$MasVnrArea)] <- 0


allSet$Electrical[is.na(allSet$Electrical)] <- "SBrkr"

table(allSet$Electrical)

levels(allSet$PoolQC) <- c(levels(allSet$PoolQC),"None")
allSet$PoolQC[allSet$PoolArea == 0] <- "None"
(rIndx <- which(is.na(allSet$PoolQC) & allSet$PoolArea >0))


garageCols <- names(allSet)[grepl("Garage.*", names(allSet))]
noGarage <- which((is.na(allSet$GarageArea) | allSet$GarageArea == 0)
                  & (is.na(allSet$GarageCars) | allSet$GarageCars == 0)
                  & is.na(allSet$GarageCond)
                  & is.na(allSet$GarageFinish)
                  & is.na(allSet$GarageQual)
                  & is.na(allSet$GarageType)
                  & (is.na(allSet$GarageYrBlt) | allSet$GarageYrBlt == 0))


levels(allSet$GarageType) <- c(levels(allSet$GarageType),"None")
levels(allSet$GarageFinish) <- c(levels(allSet$GarageFinish),"None")
levels(allSet$GarageCond) <- c(levels(allSet$GarageCond),"None")
levels(allSet$GarageQual) <- c(levels(allSet$GarageQual),"None")


allSet[noGarage,c("GarageType","GarageFinish","GarageQual","GarageCond")] <- "None"
allSet[noGarage, c("GarageYrBlt","GarageCars","GarageArea")] <- 0

bsmtCols <- names(allSet)[grepl("Bsmt.*", names(allSet))]
bsmtCols

allSet[is.na(allSet$BsmtExposure) & !is.na(allSet$BsmtFinType2) & !is.na(allSet$BsmtFinType1) & !is.na(allSet$BsmtQual) & !is.na(allSet$BsmtCond),bsmtCols]
allSet[!is.na(allSet$BsmtExposure) & is.na(allSet$BsmtFinType2) & !is.na(allSet$BsmtFinType1) & !is.na(allSet$BsmtQual) & !is.na(allSet$BsmtCond),bsmtCols]

allSet <- allSet[-c(333, 949), ]

levels(allSet$BsmtCond) <- c(levels(allSet$BsmtCond),"None")
levels(allSet$BsmtQual) <- c(levels(allSet$BsmtQual),"None")
levels(allSet$BsmtExposure) <- c(levels(allSet$BsmtExposure),"None")
levels(allSet$BsmtFinType1) <- c(levels(allSet$BsmtFinType1),"None")
levels(allSet$BsmtFinType2) <- c(levels(allSet$BsmtFinType2),"None")
allSet$BsmtQual[is.na(allSet$BsmtQual)] <- "None"
allSet$BsmtCond[is.na(allSet$BsmtCond)] <- "None"
allSet$BsmtExposure[is.na(allSet$BsmtExposure)] <- "None"
allSet$BsmtFinType1[is.na(allSet$BsmtFinType1)] <- "None"
allSet$BsmtFinType2[is.na(allSet$BsmtFinType2)] <- "None"


lot.by.nbrh <- allSet[,c('Neighborhood','LotFrontage')] %>% group_by(Neighborhood) %>% summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh

idx = which(is.na(allSet$LotFrontage))

for (i in idx){
  lot.median <- lot.by.nbrh[lot.by.nbrh$Neighborhood == allSet$Neighborhood[i],'median']
  allSet[i,'LotFrontage'] <- lot.median[[1]]
}


(na.cols <- which(colSums(is.na(allSet)) > 0))

write.csv(allSet,"train_cleaned.csv")

