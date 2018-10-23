
allSet <- read.csv("/Users/xiaomengwei/Desktop/Machine learning/project_house price/BAIT509_Project_XIAOMENG_JINYE_EGOR/code/train_cleaned.csv")

set.seed(10)
train <- allSet[sample(nrow(allSet),1020), ]
test <- allSet[ !(allSet$Id %in% train$Id), ]

rf1=randomForest(SalePrice ~ . , data = train ,mtry=4,ntree=200) 
importance(rf1)

sele.train<- train[,c("OverallQual", "GrLivArea", "GarageCars",  "Neighborhood", "ExterQual", "X1stFlrSF", "BsmtFinSF1", "LotArea", "X2ndFlrSF","KitchenQual","YearBuilt", "FullBath","SalePrice")]
sele.test<- test[,c("OverallQual", "GrLivArea", "GarageCars",  "Neighborhood", "ExterQual", "X1stFlrSF", "BsmtFinSF1", "LotArea", "X2ndFlrSF","KitchenQual","YearBuilt", "FullBath","SalePrice")]

write.csv(sele.train,"selectedtrain.csv")
write.csv(sele.test,"selectedtest.csv")

