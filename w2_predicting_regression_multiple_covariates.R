library(ISLR)
data(Wage)
Wage <- subset(Wage, select = -c(logwage))
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)
featurePlot(x=training[,c("age","education","jobclass")], y=training$wage, plot="pairs")
qplot(age, wage, data=training)
qplot(age, wage, colour=jobclass, data=training)
qplot(age, wage, colour=education, data=training)
modFit1 <- train(wage~age + jobclass + education, method="lm", data=training)
finMod<-modFit1$finalModel
print(modFit1)
plot(finMod,1,pch=19,cex=0.5,col="#00000010")
qplot(age, wage, colour=race, data=training)
plot(finMod$residuals,pch=19)
# Predicted versus truth in test set
pred <- predict(modFit1, testing)
qplot(wage, pred, colour=year, data=testing)
