library(kernlab)
data(spam)

#Sampling data start
------     ------

set.seed(3435)
trainIndicator = rbinom(4601, prob = 0.5, size =1)
table(trainIndicator)

trainSPAM <- spam[trainIndicator == 1,]
testSPAM <- spam[trainIndicator==0, ]

dim(trainSPAM)
dim(testSPAM)

table(trainSPAM$type)
table(testSPAM$type)

-------   -------
# Sampling End

# Exploratory data analysis START
-------   -------

boxplot(trainSPAM$capitalAve ~ trainSPAM$type)
boxplot(log10(trainSPAM$capitalAve ) ~ trainSPAM$type)

# ** spam emails have more capital letters than a non spam


plot(log10(trainSPAM[,1:4] +1)) 

hCluster <- hclust(dist(t(trainSPAM[, 1:57])))
plot(hCluster)

hClusterUpdated <- hclust(dist(t(log10(trainSPAM[, 1:55]+1))))
plot(hClusterUpdated)

-------   -------
# ** clustering end

# Statistical prediction START
-------   -------

trainSPAM$numType <- as.numeric(trainSPAM$type) - 1
costFunction <- function(x,y) sum(x!= (y>0.5))
cvError <- rep (NA, 55)
library(boot)

for(i in 1:55){

lmFormula = reformulate(names(trainSPAM)[i], response = "numType")
glmfit = glm(lmFormula, family = "binomial", data = trainSPAM)
cvError[i] = cv.glm(trainSPAM, glmfit, costFunction, 2)$delta[2]

}

# Which predictor has minimum cross validated error 
print(names(trainSPAM[which.min(cvError)]))

-------   -------

# Statistical prediction STOP

# Lets look for the best model - i.e with the predictor w/ lowest cost of error function
-------   -------

predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSPAM)

#get prediction on the test set
predictionTest = predict(predictionModel, testSPAM)
predictedSPAM = rep("nonspam", dim(testSPAM)[1])

predictedSPAM[predictionModel$fitted > 0.5] = "spam"
predictedSPAM <- predictedSPAM[1: dim(testSPAM)[1]]

table(predictedSPAM, testSPAM$type)

-------   -------
# Calculate prediction error