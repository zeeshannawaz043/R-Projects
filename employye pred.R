
library(MASS) 
library(ISLR)
library(dplyr)
library(psych)
library(ggplot2)
library(tidyverse)
library(gmodels) 
library(e1071) 
library(caret) 
library(tree) 
library(party)



# IMport data sets
data <- read.csv("employee_dataset.csv", sep = ';')

# shape of data
str(data)

#Exploratory Data analysis

library(stats)

# sats of the dataset

describe(data)





#plot for the  Overall_SatisfactionScore
ggplot(data, aes(x =  Overall_SatisfactionScore)) +
  geom_bar(fill = "blue")+
  theme_gray()

#plot for the  EducationType, MonthlyIncome, PerformanceReview

ggplot(data, aes(x = EducationType, y = MonthlyIncome, fill =  PerformanceReview )) +
  ylab("Monthly Income")+
  xlab("Education Type")+
  ggtitle("")+
  geom_col(position = "dodge")

#plot for the  Age, TotalExperience, JobRole_SatisfactionScore
qplot(x=Age,
      y=TotalExperience  ,
      data = data,
      color=JobRole_SatisfactionScore)


# plot for 
ggplot(data, aes(x = SatisfactionScore, y = Years_at_Company, fill = PotentialReview)) +
  geom_col(position = "dodge")


# Data preprocessing

#Find the missing values from the dataset with rescpt to each column
print(colSums(is.na(data)))

#Variable conversion form char to numeric

df<-data

# Gender variable conversion
df$Gender[df$Gender=="Male"] <- 2
df$Gender[df$Gender=="Female"] <- 1
df$Gender <- as.numeric(df$Gender)


# Education variable conversion

df$Education[df$Education=="Under Graduation"] <- 1
df$Education[df$Education=="Graduation"] <- 2
df$Education[df$Education=="Masters / PHD"] <- 3
df$Education <- as.numeric(df$Education)


# EducationType variable conversion

df$EducationType[df$EducationType=="Bio-technology"] <- 1
df$EducationType[df$EducationType=="Economics"] <- 2
df$EducationType[df$EducationType=="Marketing / Finance"] <- 3
df$EducationType[df$EducationType=="Phycology / Behavior Sciences"] <- 4
df$EducationType <- as.numeric(df$EducationType)


# MaritalStatus variable conversion

df$MaritalStatus[df$MaritalStatus=="Single"] <- 1
df$MaritalStatus[df$MaritalStatus=="Married"] <- 2
df$MaritalStatus[df$MaritalStatus=="Divorced"] <- 3
df$MaritalStatus <- as.numeric(df$MaritalStatus)



# Department variable conversion

df$Department[df$Department=="BusinessDevelopment"] <- 1
df$Department[df$Department=="ClientSolutions"] <- 2
df$Department[df$Department=="Support"] <- 3
df$Department <- as.numeric(df$Department)



# Traveltype_last_year variable conversion

df$Traveltype_last_year[df$Traveltype_last_year=="No"] <- 1
df$Traveltype_last_year[df$Traveltype_last_year=="MonthlyTravel"] <- 2
df$Traveltype_last_year[df$Traveltype_last_year=="LongTermProject"] <- 3
df$Traveltype_last_year[df$Traveltype_last_year=="Conference"] <- 4
df$Traveltype_last_year[df$Traveltype_last_year=="ClientEngagement"] <- 5
df$Traveltype_last_year <- as.numeric(df$Traveltype_last_year)


# PotentialReview  variable conversion

df$PotentialReview[df$PotentialReview=="Low"] <- 1
df$PotentialReview[df$PotentialReview=="Medium"] <- 2
df$PotentialReview[df$PotentialReview=="High"] <- 3
df$PotentialReview[df$PotentialReview=="Very High"] <- 4
df$PotentialReview <- as.numeric(df$PotentialReview)


# PerformanceReview  variable conversion

df$PerformanceReview[df$PerformanceReview=="Inconsistent"] <- 1
df$PerformanceReview[df$PerformanceReview=="Met Expectations"] <- 2
df$PerformanceReview[df$PerformanceReview=="Exceed Expectations"] <- 3
df$PerformanceReview <- as.numeric(df$PerformanceReview)



# SatisfactionScore   variable conversion

df$SatisfactionScore[df$SatisfactionScore=="Detractor"] <- 1
df$SatisfactionScore[df$SatisfactionScore=="Passive"] <- 2
df$SatisfactionScore[df$SatisfactionScore=="Promoter"] <- 3
df$SatisfactionScore <- as.numeric(df$SatisfactionScore)



# JobRole_SatisfactionScore variable conversion

df$JobRole_SatisfactionScore[df$JobRole_SatisfactionScore=="Detractor"] <- 1
df$JobRole_SatisfactionScore[df$JobRole_SatisfactionScore=="Passive"] <- 2
df$JobRole_SatisfactionScore[df$JobRole_SatisfactionScore=="Promoter"] <- 3
df$JobRole_SatisfactionScore <- as.numeric(df$JobRole_SatisfactionScore)


# Overall_SatisfactionScore variable conversion

df$Overall_SatisfactionScore[df$Overall_SatisfactionScore=="Detractor"] <- 1
df$Overall_SatisfactionScore[df$Overall_SatisfactionScore=="Passive"] <- 2
df$Overall_SatisfactionScore[df$Overall_SatisfactionScore=="Promoter"] <- 3
df$Overall_SatisfactionScore <- as.numeric(df$Overall_SatisfactionScore)


str(df)

#Ubsupervised learning

#Apply the PCA on the Numeric data

# Creat a new data
unsup_data<-df

# Remove the ID and target variable from data

unsup_data$EmployeeID<-NULL
unsup_data$Overall_SatisfactionScore<-NULL


# Nomralized the data

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
scaled_features <- as.data.frame(lapply(unsup_data, normalize))
head(scaled_features)

# Apply the PCA on the dataset

pca_df = prcomp(scaled_features, center = TRUE, scale = TRUE)
summary(pca_df)

plot(pca_df)


# extract the 2 compnents which are good for clustering
data_trans= as.data.frame(-pca_df$x[,1:2])
head(data_trans, 3)

# Apply the K-mean clustering

library(cluster)
library(factoextra)


## silhouette method to find the best cluster 

fviz_nbclust(data_trans,kmeans, method = 'silhouette')


#kmeans with K=3

# implementation of the kmean
k = 3
kmeans_22 = kmeans(data_trans, centers = k, nstart = 50)
print(kmeans_2)


#plot the clusters
library(cluster)
# clustering visualization
fviz_cluster(object = kmeans_22, 
             data = data_trans)


# Apply the Supervised Learning

str(data)

full_data<-cbind(scaled_features, data[c('Overall_SatisfactionScore')])


full_data$Overall_SatisfactionScore<-as.factor(full_data$Overall_SatisfactionScore)

str(full_data)


# Data partion for training and testing

library(caret)

set.seed(42)
index <- createDataPartition(full_data$Overall_SatisfactionScore, p = 0.7, list = FALSE)
train <- full_data[index, ]
test  <- full_data[-index, ]

dim(train)
dim(test)

# Apply the LDA
set.seed(42)
LDA = lda(Overall_SatisfactionScore~., data=train)
summary(LDA)

# Make prediction on the test data
LDApredict = predict(LDA, test)$class
#Print the classification results
confusionMatrix(LDApredict,test$Overall_SatisfactionScore)



# Apply the QDA

QDA = qda(Overall_SatisfactionScore~., data=train)
summary(QDA)

# Make prediction on the test data
QDApredict = predict(QDA, test)$class
#Print the classification results
confusionMatrix(QDApredict,test$Overall_SatisfactionScore)

#Apply Multimonial regression

library(nnet)
mod <- multinom(Overall_SatisfactionScore ~ ., data=train)
summary(mod)

#Predict Table
pred <- predict(mod, test)

#Create a table for cunfusion metrics

table12 <- confusionMatrix(pred, test$Overall_SatisfactionScore)
table12


# Apply random forest

library(randomForest)
Forest <- randomForest(Overall_SatisfactionScore ~ ., data=train)
print(Forest)

#Predict Table
pred11 <- predict(Forest, test)

#Create a table for confusion metrics

table121 <- confusionMatrix(pred11, test$Overall_SatisfactionScore)
table121

# LAsso and Ridge regression


# Data partion for training and testing




library(caret)
set.seed(78204) # for reproducible results
sample.size <- floor(0.7 * nrow(df)) # %80 for training, %20 for testing
train.index <- sample(seq_len(nrow(df)), size = sample.size)
train <- df[train.index, ]
test <- df[-train.index, ]
dim(train)
dim(test)



# Ridge regression

str(train)
library(glmnet)

x <- model.matrix(Overall_SatisfactionScore ~ ., df)[, -1]
y <- df$Overall_SatisfactionScore
# cross-validation on Ridge
# CV on Ridge
Ridge <- cv.glmnet(x[train.index, ], y[train.index], alpha = 0)
print(Ridge)
plot(Ridge)
bestlambada <- Ridge$lambda.min
bestlambada

# Rebuild the model over the best lambda 
ridge_model <- glmnet(x[index, ], y[index], alpha = 0, lambda = bestlambada) 

# Apply again with the best lambda value to get predictions 
ridge_pred <- predict(ridge_model, s = bestlambada, newx = x[-train.index, ])
## Compute test error
test_error = mean((test$Overall_SatisfactionScore - ridge_pred)^2)
test_error


# Lasso Regression


lasso <- cv.glmnet(x[train.index, ], y[train.index], alpha = 1)
plot(lasso)
bestlambada <- lasso$lambda.min
bestlambada

# Rebuild the model over the best lambda 
lasso_model <- glmnet(x[train.index, ], y[train.index], alpha = 1, lambda = bestlambada) 

# Apply again with the best lambda value to get predictions 
lasso_pred <- predict(lasso_model, s = bestlambada, newx = x[-train.index, ])

## Compute test error lasso_model
test_error = mean((test$Overall_SatisfactionScore - lasso_pred)^2)
test_error



