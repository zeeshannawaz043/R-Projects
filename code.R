
# Import libraries
library(ggplot2)
library(dplyr)
library(psych)
library(corrplot)
library(factoextra)
library(cluster)

#Read dataset

wine_20066799 <- read.csv("wine.20066799.csv")
View(wine_20066799)

# Structure of the dataset

str(wine_20066799)


# packages for k-medoids


# data mining
attributes(wine_20066799)
summary(wine_20066799)

# data visualization
plot(wine_20066799)

# Remove the ID and and density from the data set
# create the new dataframe 

df<- wine_20066799

df$ID <-NULL
df$density <- NULL

str(df)

# Scale the dataset before clustering

scale_df <- scale(df)
head(scale_df)

# Now combine the Density with the scaled dataset

complete_df<- cbind(scale_df, wine_20066799[c('density')])
str(complete_df)

# Now the dataset is ready
# k_medoid by using the K=9
# pam() function is used to calculate the k_medoids of dataset.  K is the number of clusters chooses for this computation. 
#the metric = 'euclidean'

pam(complete_df, k=9, metric = 'euclidean')


# to determine number of clusters use the function fviz_gap_stat()
Library(cluster)

gap_stat = clusGap(scale_df,FUN = pam, K.max = 10,B = 10)
fviz_gap_stat(gap_stat )

# Lets run the various clusters to make the comparision
model<-pam(complete_df, k=2 )
model$clusinfo


model<-pam(complete_df, k=3 )
model$clusinfo

model<-pam(complete_df, k=5 )
model$clusinfo

model<-pam(complete_df, k=6 )
model$clusinfo

model<-pam(complete_df, k=8 )
model$clusinfo

model<-pam(complete_df, k=9 )
model$clusinfo

model<-pam(complete_df, k=10 )
model$clusinfo

# Show the final model
pam(complete_df, k=9, metric = 'euclidean')
