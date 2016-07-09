
---
title: "Practical Machine Learning Course Project"
author: "Jesus Ariel Pena"
date: "July 8, 2016"
output: html_document
---

###**Background**

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

###**Data**

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

###**Project Objectives**
The goal of your project is to predict the manner in which they did the exercise. This is the “classe” variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

###**Reproduceablity**

The following section includes the different libraries that need to be loaded and the seed set in order to the project to be reproduceable.


```{r}
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
## Rattle: A free graphical interface for data mining with R.
## Version 4.1.0 Copyright (c) 2006-2015 Togaware Pty Ltd.
## Type 'rattle()' to shake, rattle, and roll your data.
library(randomForest)
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## The following object is masked from 'package:ggplot2':
## 
##     margin
library(e1071)
set.seed(12345)
```

###**Getting the data**
First storage the two dataset in different variables, Train and Test and clean the data by omitting NA values. 

```{r}
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

dim(training)
dim(testing)

##The training dataset contains 19622 observations and 160 variables, the testing dataset contains 20 observations and 160 variables
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]

## the first columns are not usuable for this project so will be removed 

training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]


```

###**Cross Validation**
Now the cross validation will be perfomed splitting the data into 70% for the training and 20% for the testing.

The method to use is =Random Forest due to its higly accuracy rate. 
Additionally the cross validation is used as train control with 5-folds as the parameter number. 

```{r}
subSamples <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
subTraining <- training[subSamples, ] 
subTesting <- training[-subSamples, ]

model1 <- trainControl(method = "cv", number = 5, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)


##Start with a Random Forest 

rf <- train(classe ~ ., data = subTraining, method = "rf", trControl= model1, ntree=200)

rf

# Evaluate the performance of the model with a confusion matrix function and with the estimated accuracy.
predictions <- predict(rf, newdata=subTesting)
confusionMatrix(subTesting$classe, predictions)

accuracy <- postResample(predictions, subTesting$classe)
accuracy

```
Next the decision tree  is applied and a graph is provided and the corresponding confusionMatrix

```{r}



# First we need to fit the model , the subTraining dataset is used

modFit <- rpart(classe ~ ., data=subTraining, method="class")

# Next we need to perform the prediction
decisiontree <- predict(modFit, subTesting, type = "class")

# Now a plot showing the classifcation tree is provided.

rpart.plot(modFit, main="Classification Tree", extra=102, under=TRUE, faclen=0)


#Finally evaluate the performance of the model by creating a confusion matrix function.
confusionMatrix(decisiontree, subTesting$classe)




#Finally predict the model using the testing dataset 
last_result <- predict(rf, testing)
last_result


```

###**Conclusion**

As previously shown in the analysis the Random Forest  method provides the better accuracy 
