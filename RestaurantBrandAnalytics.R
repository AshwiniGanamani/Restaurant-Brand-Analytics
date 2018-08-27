###Restaurant Brand Analytics

#Read the data set for Restaurants
setwd("/Users/anushiarora/Desktop/project BA")
data<- read.csv("Complete.csv", header = TRUE, stringsAsFactors = FALSE)

#About the data set
dim(data)
str(data)
names(data)
head(data)

#Generating Training and Test Data Set for Machine Learning
ind <- sample(2,nrow(data1),replace=TRUE,prob=c(0.7,0.3))
trainData <- data1[ind==1,]
testData <- data1[ind==2,]

#Installing Random Forest
install.packages("randomForest")
library(randomForest)

#Generating Random Forest Model
iris_rf <- randomForest(trainData$DriverConvenientPR~.,data=trainData,ntree=100,proximity=TRUE)
table(predict(iris_rf),trainData$Visits)
print(iris_rf)
plot(iris_rf)
importance(iris_rf)
varImpPlot(iris_rf, main = "Due to Driver Convenience")
irisPred<-predict(iris_rf,newdata=testData)
table(irisPred, testData$DriverHealthyPR)
plot(margin(iris_rf,irisPred))
tune.rf <- tuneRF(data[,-5],data[,5], stepFactor=0.5)
print(tune.rf)
plot(tune.rf)
