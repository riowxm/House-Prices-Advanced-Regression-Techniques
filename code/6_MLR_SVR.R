### Set the working directory
setwd("/Users/eseli/Documents/01 Work/04 MBAN Term 4/3. BAIT_509_Machine learning/Final project")
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(rpart)
library(Metrics)
library(MASS)
library(e1071)

rm(list = ls())

mydata <- read.csv("train.csv")
Kitchen <- mapvalues(mydata$KitchenQual, from = c("Ex", "Gd", "Fa", "TA"), to = c(5, 4, 3, 2))
Exter <- mapvalues(mydata$ExterQual, from = c("Ex", "Gd", "Fa", "TA"), to = c(5, 4, 3, 2))
Kitchen <- as.numeric(Kitchen)
Exter <- as.numeric(Exter)
mydata <-cbind(mydata, Kitchen, Exter)

set.seed(77)
train<-sample_frac(mydata, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-mydata[-sid,]

dim(test)
dim(train)

z.1 <- lm(formula = SalePrice ~ OverallQual + Neighborhood + GarageCars + FullBath + GrLivArea +
                        X1stFlrSF + X2ndFlrSF + Exter + Kitchen + YearBuilt + BsmtFinSF1 + LotArea, data = train)

summary(z.1)
yhat.1 <- predict(z.1, newdata = test)
mae(test$SalePrice, yhat.1)
mse(test$SalePrice, yhat.1)


# We remove GrLivArea and FullBath, because they failed t-test
z.2 <- lm(formula = SalePrice ~ OverallQual + Neighborhood + GarageCars +
            X1stFlrSF + X2ndFlrSF + Exter + Kitchen + YearBuilt + BsmtFinSF1 + LotArea, data = train)

summary(z.2)
yhat.2 <- predict(z.2, newdata = test)
mae(test$SalePrice, yhat.2)
mse(test$SalePrice, yhat.2)

test1 <-cbind(test, yhat.2)
ggplot(data=test1, aes(x=X1stFlrSF, y=SalePrice)) + geom_line(aes(x = X1stFlrSF, y=yhat.2, colour="darkred"), test1) + geom_line(col='deepskyblue4')

          


selectedvar <- c("SalePrice", "OverallQual", "GrLivArea", "Neighborhood", "GarageCars", "X1stFlrSF", "X2ndFlrSF", "Exter", "FullBath", "Kitchen", "YearBuilt", "BsmtFinSF1", "LotArea")
head(train[ ,selectedvar])

z.3 <- svm(SalePrice ~ ., data = train[ ,selectedvar])
yhat.3 <- predict(z.3, newdata = test[ ,selectedvar])
mae(test$SalePrice, yhat.3)
mse(test$SalePrice, yhat.3)
mean(abs(test$SalePrice - yhat.3))

svm_tune <- tune(svm, SalePrice ~ ., data = train[ , selectedvar], ranges = list(epsilon = c(0.07, 0.08, 0.09, 0.1), cost = c(4, 5, 6, 7, 8, 9, 10)))
plot(svm_tune)
print(svm_tune)

z.4 <- svm_tune$best.model
yhat.4 <- predict(z.4, newdata = test[ , selectedvar])
mae(test$SalePrice, yhat.4)