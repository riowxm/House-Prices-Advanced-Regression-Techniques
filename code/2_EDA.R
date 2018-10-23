### Set the working directory
setwd("/Users/xiaomengwei/Desktop/Machine learning/project_house price/code")
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(tidyverse)
library(tidyr)
library(rpart)
library(tree)
library(gbm)

mydt <- read.csv("train_cleaned.csv")
dim(mydt)
summary(mydt)
str(mydt[,c(1:30, 81)])

### view histogram of the sales price 
ggplot(data=mydt, aes(SalePrice)) + geom_histogram(fill = "deepskyblue4", col= "white", binwidth = 20000) +
  scale_x_continuous(breaks = seq(0, 700000, by=100000), labels = comma) + labs(title = "Histogram of the response variable: Sale Price")

summary(mydt$SalePrice)

### Build corrleogram between sales price and predictor variables

numericvars <- which(sapply(mydt, is.numeric)) #index vector numeric variables
numericvars
numericvarnames <- names(numericvars) #saving names vector for use later on
numericvarnames
length(numericvars) #there are 39 numberic variables

numvar_all <- mydt[,numericvarnames]
cor_numVar <- cor(numvar_all, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

#Plot the matrix
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

### EXPLORE RELATIONSHIPS BETWEEN RESPONSE AND PREDICTORS

ggplot(data=mydt, aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot() + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  labs(title = "Scatterplot of Sales price vs Ovarall Quality")

ggplot(data=mydt, aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='deepskyblue4') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  scale_x_continuous(breaks = seq(0, 5000, 500), labels = comma) + labs(title = "Scatterplot of Sales price vs Living Area")

ggplot(data=mydt, aes(x=YearBuilt, y=SalePrice))+
  geom_point(col='deepskyblue4') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  labs(title = "Scatterplot of Sales price vs Year of construction")

ggplot(data=mydt, aes(x=X1stFlrSF, y=SalePrice))+
  geom_point(col='deepskyblue4') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  labs(title = "Scatterplot of Sales price vs First floor area in feet")

ggplot(data=mydt, aes(x=factor(GarageCars), y=SalePrice))+
  geom_boxplot() + labs(x='Number of Cars in Garage') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  labs(title = "Scatterplot of Sales price vs Capacity of the Garage")

ggplot(data=mydt, aes(x=factor(FullBath), y=SalePrice))+
  geom_boxplot() + labs(x='Number of Bathrooms in the House') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  labs(title = "Scatterplot of Sales price vs Number of Bathrooms")

#estimate importance scores from the RF model
test <- read.csv("test.csv")
set.seed(10)

model.tree = tree(SalePrice ~ ., data = mydt)
plot(model.tree)
text(model.tree, pretty = 0)
cv.new = cv.tree(model.treee, FUN=prune.misclass)

model.rf=randomForest(SalePrice ~ . , data = mydt, mtry=40, ntree=300, importance = TRUE) 
importance(model.rf)
varImpPlot(model.rf)

# Classification Tree with rpart
library(rpart)
cor_numVar[,c(1:11)]

quantVar <- t(cor_numVar[c(2:11), 0])
quantVar

# grow tree 
model.regress <- rpart(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF + X1stFlrSF + FullBath + TotRmsAbvGrd + YearBuilt + YearRemodAdd, 
              method="anova", data=mydt)

printcp(model.regress) # display the results 
plotcp(model.regress) # visualize cross-validation results 
summary(model.regress) # detailed summary of splits

plot(model.regress, uniform=TRUE, 
     main="Regression Tree")
text(model.regress, use.n=TRUE, all=TRUE, cex=.8)

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(model.regress, cp = 0.002)
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

### Discuss relationships between predictors and response variable

Kitchen <- mapvalues(mydt$KitchenQual, from = c("Ex", "Gd", "Fa", "TA"), to = c(5, 4, 3, 2))
Exter <- mapvalues(mydt$ExterQual, from = c("Ex", "Gd", "Fa", "TA"), to = c(5, 4, 3, 2))
Kitchen <- as.numeric(Kitchen)
Exter <- as.numeric(Exter)
mydt <-cbind(mydt, Kitchen, Exter)

mydt[,c("SalePrice", "YearBuilt", "LotArea", "GrLivArea", "X1stFlrSF", "X2ndFlrSF","BsmtFinSF1")] %>% as.data.frame() %>% GGally::ggpairs()
mydt[,c("SalePrice", "GarageCars", "FullBath", "OverallQual", "Exter",  "Kitchen")] %>% as.data.frame() %>% GGally::ggpairs()

#selectedvar <- c("SalePrice", "OverallQual", "GrLivArea", "Neighborhood", "GarageCars", "X1stFlrSF", "X2ndFlrSF", "ExterQual", "FullBath", "KitchenQual", "YearBuilt", "BsmtFinSF1", "LotArea")














