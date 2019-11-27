#author: vincent acuesta
#methods: random forest decision trees and knn algorithm
#class: quantitative methods in biostats

#random forest implementation
library(caret)
library(ggplot2)
library(cowplot)
library(randomForest)
#be connected to the internet and you can read the url of the data you need 
url<- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'

#now read that data into a csv file
data <- read.csv(url, header=FALSE)

#considering they are not labeled it makes it easer to label the columns
colnames(data)<-c(
  'id',
  'clump_thickness',
  'uniform_cell_size',
  'uniform_cell_shape',
  'marginal_adhesion',
  'single_epithelial_cell_size',
  'bare_nuclei',
  'bland_chromatin',
  'normal_nucleoli',
  'mitoses',
  'class'
)

#we need to get rid of the first column because it does not influence the classification of the test sample, it is just the identification of each instance 
data<-data[,-1]

#check the data to be sure that everything is okay
head(data)

#this will take into account the columns that had missing attributes to begin with 
data[data=='?']<- NA
data$bare_nuclei <- as.integer(data$bare_nuclei)
data$bare_nuclei <- as.factor(data$bare_nuclei)
#in order to do classification of the random forest method you need to make a factor out of the classification because you need to denote that it is either malignant or benign
data$class <- ifelse(test=data$class == 2, yes="benign", no="malignant")
#make into a factor in order to use classification
data$class <- as.factor(data$class) 

#we have to take into account missing values in the attributes after looking at the missing attributes in the data file, it says that there are 8 missing values that are denoted by a '?' to take into account
str(data)

set.seed(123)
#impute any missing values in the training set using proximities
#this is needed to account for the missing values in the file
data.imputed <- rfImpute(class ~ ., data = data, iter=6)

#create the model of the random forest and to cluster the samples 
forest_model <- randomForest(class ~ ., data=data.imputed, proximity=TRUE)

#go see the summary of the model
forest_model

#create data frame to see the oob error and how it decreases based on increasing the amount of decision trees involved
error_rate<- data.frame(
  Trees= rep(1:nrow(forest_model$err.rate), times=3),
  Type= rep(c('OOB', 'benign', 'malignant'), each=nrow(forest_model$err.rate)),
  Error=c(forest_model$err.rate[,'OOB'],
          forest_model$err.rate[,'benign'],
          forest_model$err.rate[,'malignant']
          ))
#use ggplot() in order to find how it decreases with the high number of decision trees involved in the forest
ggplot(data=error_rate, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#this is just to show a sample tree representation
reprtree:::plot.getTree(forest_model)


#random forest implementation

library(caret)
library(ggplot2)
library(cowplot)
library(randomForest)
#be connected to the internet and you can read the url of the data you need 
url<- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'

#now read that data into a csv file
data <- read.csv(url, header=FALSE)

#considering they are not labeled it makes it easer to label the columns
colnames(data)<-c(
  'id',
  'clump_thickness',
  'uniform_cell_size',
  'uniform_cell_shape',
  'marginal_adhesion',
  'single_epithelial_cell_size',
  'bare_nuclei',
  'bland_chromatin',
  'normal_nucleoli',
  'mitoses',
  'class'
)

#we need to get rid of the first column because it does not influence the classification of the test sample, it is just the identification of each instance 
data<-data[,-1]

#this will take into account the columns that had missing attributes to begin with 
data[data=='?']<- NA
data$bare_nuclei <- as.integer(data$bare_nuclei)
data$bare_nuclei <- as.factor(data$bare_nuclei)
#in order to do classification of the random forest method you need to make a factor out of the classification because you need to denote that it is either malignant or benign
data$class <- ifelse(test=data$class == 2, yes="benign", no="malignant")
#make into a factor in order to use classification
data$class <- as.factor(data$class) 

set.seed(123)
#impute any missing values in the training set using proximities
#this is needed to account for the missing values in the file
data.imputed <- rfImpute(class ~ ., data = data, iter=6)

#partition the data sample
set.seed(123)
pred_var<-sample(2, nrow(data.imputed), replace = TRUE, prob = c(0.7,0.3))
train<- data.imputed[pred_var==1,]
test<-data.imputed[pred_var==2,]

set.seed(1234)
forest_model1 <- randomForest(class~., data=train)
forest_model1

#do a prediction model on the training set and then peak the head to see the classification of those
library(caret)
pred_1<- predict(forest_model1, train)
head(pred_1)
#analyze the error based on the trained model
confusionMatrix(pred_1, train$class)

#do a second prediction on the test values that are not trained and seen by the computer
pred_2<- predict(forest_model1, test)
confusionMatrix(pred_2, test$class)

#knn algorithm classification comparison
library(VIM)
library(mice)
library(caret)
library(e1071)

#be connected to the internet and you can read the url of the data you need 
url<- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'

#now read that data into a csv file
data <- read.csv(url, header=FALSE)

#considering they are not labeled it makes it easer to label the columns
colnames(data)<-c(
  'id',
  'clump_thickness',
  'uniform_cell_size',
  'uniform_cell_shape',
  'marginal_adhesion',
  'single_epithelial_cell_size',
  'bare_nuclei',
  'bland_chromatin',
  'normal_nucleoli',
  'mitoses',
  'class'
)

#The first column can be removed because it does not effect the classification portion of the data
data<-data[,-1]
data[data=='?']<- NA
data$bare_nuclei <- as.integer(data$bare_nuclei)
data$bare_nuclei <- as.factor(data$bare_nuclei)
data<-kNN(data, variable = c('bare_nuclei'), k=6)
str(data)

#set the seed so that it is reproducible then make the training set to train then the testing set in order to partition what needs to be trained. 
set.seed(1234)
train_this <- createDataPartition(y = data$class, p= 0.7, list = FALSE)
training <- data[train_this,]
testing <- data[-train_this,]

dim(training)
dim(testing)

anyNA(data)

#This will train for the classifier for either 2 or 4, which is benigh or malignant respectively
training[["class"]] = factor(training[["class"]])

#This will test the knn model
trn_control <- trainControl(method = "repeatedcv", number = 10, repeats = 4)
set.seed(1234)
knn_fit <- train(class ~., data = training, method = "knn",
                 trControl=trn_control,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

#do a prediction on test sit that is not involved with the training dataset
test_pred <- predict(knn_fit, newdata = testing)
test_pred



