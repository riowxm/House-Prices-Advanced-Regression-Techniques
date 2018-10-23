
library(randomForest)
library(gbm)

sele.train <- read.csv("/Users/xiaomengwei/Desktop/Machine learning/project_house price/BAIT509_Project_XIAOMENG_JINYE_EGOR/code/selectedtrain.csv")
sele.test <- read.csv("/Users/xiaomengwei/Desktop/Machine learning/project_house price/BAIT509_Project_XIAOMENG_JINYE_EGOR/code/selectedtest.csv")

set.seed(10)
rf1=randomForest(SalePrice ~ . , data = sele.train ,mtry=4,ntree=400) 

yhat = predict(rf1,newdata = sele.test)

mean((yhat-test$SalePrice)^2)
mean(abs(yhat-test$SalePrice))


####try different mtry ntree
err1=integer(350)
err2=integer(350)
err3=integer(350)
err4=integer(350)

for(i in 1:350) 
{
  rf1=randomForest(SalePrice ~ . , data = sele.train ,mtry=12,ntree=i) 
  yhat1 <-predict(rf1,newdata = sele.test)
  err1[i] = mean(abs(yhat1 - sele.test$SalePrice),na.rm=TRUE) #Error of all Trees fitted
  rf2=randomForest(SalePrice ~ . , data = sele.train ,mtry=6,ntree=i) 
  yhat2 <-predict(rf2,newdata = sele.test)
  err2[i] = mean(abs(yhat2 - sele.test$SalePrice),na.rm=TRUE) #Error of all Trees fitted
  rf3=randomForest(SalePrice ~ . , data = sele.train ,mtry=4,ntree=i) 
  yhat3 <-predict(rf3,newdata = sele.test)
  err3[i] = mean(abs(yhat3 - sele.test$SalePrice),na.rm=TRUE) #Error of all Trees fitted
  rf4=randomForest(SalePrice ~ . , data = sele.train ,mtry=3,ntree=i) 
  yhat4 <-predict(rf4,newdata = sele.test)
  err4[i] = mean(abs(yhat4 - sele.test$SalePrice),na.rm=TRUE) #Error of all Trees fitted
  
}

x=seq(1,350)
plot(NULL,xlim = c(1,350),ylim=c(15000,30000), ylab="Mean Absolute Error",xlab="tree")
lines(x,err1[1:350],col="blue")
lines(x,err2[1:350],col="red")
lines(x,err3[1:350],col="forestgreen")
legend("topright",legend=c("12 predictors at each split","6 predictors at each split","4 predictors at each split"),lty = 1, col=c("red","blue","forestgreen"))


set.seed(10)
Boston.boost=gbm(SalePrice ~ . ,data = sele.train,n.trees = 4000,
                 shrinkage = 0.01, interaction.depth = 4)

yhat2 = predict(Boston.boost,newdata = sele.test,n.trees = 4000)


mean((yhat2-sele.test$SalePrice)^2)
mean(abs(yhat2-sele.test$SalePrice))

