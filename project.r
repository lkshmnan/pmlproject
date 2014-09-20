library(ggplot2); library(caret); library(kernlab)
library(rattle)
set.seed(23312)
MyData <- read.csv(file="pml-training.csv", header=TRUE, sep=",")
inTrain <- createDataPartition(y=MyData$classe,p=0.7,list=FALSE)

training <- MyData[inTrain,]
testing <- MyData[-inTrain,]
print(dim(MyData))

print(featurePlot(x=training[,c("roll_belt","pitch_belt","yaw_belt")],y=training$classe,plot="pairs"))

adata <- aggregate(training[c("roll_belt","pitch_belt","yaw_belt","roll_arm","pitch_arm","yaw_arm","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","roll_forearm","pitch_forearm","yaw_forearm")], by=training["X"], FUN=mean)
adatad <- adata[,2:13]
#print(adatad)

trainClass <- training[,160]
modelFit <- train(trainClass ~ ., method="rf",data=adatad,preProcess=c("center","scale"))#,tuneLength=3)#,trControl = fitControl,prox=TRUE)

predictions <- predict(modelFit,newdata=testing)#,type="classe")

# Accuracy rate
#print(sum(testing$classe==predictions)/length(predictions))

print(confusionMatrix(testing$classe,predictions))
