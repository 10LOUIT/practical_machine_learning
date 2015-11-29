# Question 1
# Which of the following commands will create training and test sets with about 50% of the observations assigned to each?

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2
# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# plot histogram
hist(mixtures$CompressiveStrength)
hist(log(mixtures$Superplasticizer))
# training data set
hist(training$Superplasticizer)
hist(log(training$Superplasticizer))
# test data set
hist(testing$Superplasticizer)
hist(log(testing$Superplasticizer))

# Question 3
# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function from the caret package. Calculate the number of principal components needed to capture 90% of the variance. 
# How many are there?

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
IL <- training[,grep('^IL', x = names(training) )]
preProc <- preProcess(IL, method='pca', thresh=0.9, outcome=training$diagnosis)
preProc$rotation
# Question 4
# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function. 
# What is the accuracy of each method in the test set? Which is more accurate?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
IL <- grep("^IL", colnames(training), value=TRUE)
ILpred <- predictors[, IL]
dfIL <- data.frame(diagnosis, ILpred)
inTrain <- createDataPartition(dfIL$diagnosis, p=3/4)[[1]]
training <- dfIL[inTrain, ]
testing <- dfIL[-inTrain, ]
# build a predictive model with glm method
modelFit <- train(diagnosis~.,method="glm",data=training)
predictions <- predict(modelFit, newdata=testing)
CM1 <- confusionMatrix(predictions, testing$diagnosis)
print(CM1)
NONPCA <- CM1$overall[1]
NONPCA
### 
modelFit <- train(training$diagnosis~.,method="glm", preProcess="pca", data=training, trControl=trainControl(preProcOptions = list(tresh=0.8)))
CM2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(CM2)
PCA <- CM2$overall[1]
PCA
