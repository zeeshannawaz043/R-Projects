
# Import these libraries

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("rpart")
#install.packages("randomForest")
#install.packages("e1071")


library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(randomForest)
library(e1071)

#import the data sets
df <- read.csv("breast_cancer_bd.csv")

# head of the dataset
str(df)

#summary of the dataset
summary(df)


# Data preprocesisng


# Print the data type in this varibale
table(df$Bare.Nuclei)


# Replace '?' with NA
df[df == '?'] <- NA
table(df$Bare.Nuclei)
# Convert the 'Bare Nuclei' column to numeric
df$Bare.Nuclei<-as.numeric(df$Bare.Nuclei)

table(df$Bare.Nuclei)


#Find the missing values 
print(colSums(is.na(df)))



# Custom function to calculate mode
mode_function <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Calculate the mode (excluding NA values)
mode_value <- mode_function(df$Bare.Nuclei[!is.na(df$Bare.Nuclei)])

# Replace NA values with the mode
df$Bare.Nuclei <- ifelse(is.na(df$Bare.Nuclei), mode_value, df$Bare.Nuclei)






#Agian Check the missing values 
print(colSums(is.na(df)))

#### Finds the Influential varibales

library(corrplot)
corrplot(cor(df),
         method = "color", 
         addCoef.col="grey", 
         order = "AOE", 
         number.cex=0.60)



# COnvert the target class into the categorical

df$Class <- ifelse(df$Class == 2, 'benign', 'malignant')

table(df$Class)

# make the target class factorized

df$Class<-as.factor(df$Class)

df$Sample.code.number<-NULL # remove this variable

# Show the data structure

str(df)



################ Model Building ##################


# Normalize the datasets before classification


# Function to normalize a vector
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Exclude the last columns which is target class while normlaization
features <- names(df)[1:(ncol(df) - 1)]

# Normalize the features
df[, features] <- lapply(df[, features], normalize)

# Show the normalized dataset
head(df)


# Split the data for trianing and testing


set.seed(150)
ind <- createDataPartition(df$Class, p = 0.75, list = FALSE)
train_df <- df[ind, ]
test_df  <- df[-ind, ]


# Show the dimensions of the dataset
dim(train_df)
dim(test_df)


# Apply the Decison Tree
library(rpart.plot)

classification_tree_model <- rpart(Class ~ ., data=train_df, method="class")

rpart.plot(classification_tree_model)

# Predict the values using the test dataset
classification_tree_predictions <- predict(classification_tree_model, test_df, type="class")



# Performance of the model
tree_results <- confusionMatrix(data = classification_tree_predictions, 
                          reference = test_df$Class, 
                          positive = "malignant",
                          mode = "everything")
tree_results


# Apply the Random FOrest


RF_model <- randomForest(Class ~ ., data=train_df)

# Predict the values using the test dataset
RF_model_predictions <- predict(RF_model, test_df, type="class")

# Performance of the model
RF_results <- confusionMatrix(data = RF_model_predictions, 
                                reference = test_df$Class, 
                                positive = "malignant",
                                mode = "everything")
RF_results


# Apply the SVM


SVM_model <- svm(Class ~ ., data=train_df,kernel = "linear")

# Predict the values using the test dataset
SVM_model_predictions <- predict(SVM_model, test_df, type="class")

# Performance of the model
SVM_results <- confusionMatrix(data = SVM_model_predictions, 
                              reference = test_df$Class, 
                              positive = "malignant",
                              mode = "everything")
SVM_results

