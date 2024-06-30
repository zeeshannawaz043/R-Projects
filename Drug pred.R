
# Includes these libraries

library(ggplot2)
library(dplyr)


# import data
data <- read.csv("drug200.csv")


# data understadning and prepration

# structure of the dataset
str(data)


# misisng vlaues from data

#Find the missing values from the dataset with rescpt to each column
print(colSums(is.na(data)))


# factor all the varibales

data$Sex<-as.factor(data$Sex)
data$BP<-as.factor(data$BP)
data$Cholesterol<- as.factor(data$Cholesterol)
data$Drug<-as.factor(data$Drug)

# data understaidnngs
summary(data$Age)

hist(data$Age) # hist of age

summary(data$Na_to_K)

hist(data$Na_to_K) # hist of the nA_to_K

# visualization of target class
plot(data$Drug, main = 'Target variable Distribution')

str(data)


#Exploratroy data analysis


# plot for the Drug and age
ggplot(data, aes(x =  Drug, y = Age)) +
  ylab("Age Count")+
  xlab("Drug")+
  geom_col(position = "dodge", fill = "blue")


# plot for the BP, Na_to_k with Sex
ggplot(data, aes(x = BP, y = Na_to_K , col = Sex)) +   # Change color of borders
  geom_boxplot()


# plot for the age, Na and chorestrol
qplot(x=Age,
      y=Na_to_K,
      data = data,
      color=Cholesterol,
      main="Three-way Relationship")


#Implementation of Algorithms
library(caret)

# divide test and train data

# Set seeds
set.seed(300)
ind <- createDataPartition(data$Drug, p = 0.60, list = FALSE)
traind <- data[ind, ]
testd  <- data[-ind, ]


dim(traind)
dim(testd)


# Apply the decsion tree

set.seed(500)


library(rpart)
library(rpart.plot)

set.seed(500)
tree_m<-rpart(Drug~., data = traind)

rpart.plot(tree_m)


# test data predicitons
trr_pred <- predict(object = tree_m, # train object
                   newdata = testd, type="class") # input variables in test set

tr_per <- confusionMatrix(data = trr_pred, 
                          reference = as.factor(testd$Drug), 
                          positive = "0",
                          mode = "everything")
tr_per


# apply the random forest

library(randomForest)

set.seed(500)
tree_m1<-randomForest(Drug~., data = traind)

plot(tree_m1)


# test data predicitons
trr_pred <- predict(object = tree_m1, # train object
                    newdata = testd, type="class") # input variables in test set

tr_per1 <- confusionMatrix(data = trr_pred, 
                          reference = as.factor(testd$Drug), 
                          positive = "1",
                          mode = "everything")
tr_per1


