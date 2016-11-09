# ClintonObama.r
# Replicates analysis for Clinton-Obama voters case
setwd("~/Dropbox/In process/MyRWork")

# Read this data into R

election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to find out the number of missing data points in the demographic/county columns. The missing data points are listed as number of NA's under the respective columns.
summary(election_data)

# Write a function that replaces NAs with the mean of the non-missing data in the column. This function can be called for different data sets to impute the data.
impute_data <- function(vec, mn) {
      ifelse(is.na(vec), mn, vec)
}

# Find the means for all the numeric columns. The function sapply automatically runs the mean function (specified as second argument) on the columns 10 through 41. The means are then saved in the vector named train_data_mean. We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)
# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and for each column. C call the function impute_train_data.
for(i in 10:41) {
      election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, use the following "write" commands in R.
write.csv(election_data_train, "electionDataTrain.csv")
write.csv(election_data_test, "electionDataTest.csv")


# Create some possible independent variables (things we might like to predict in a regression using the demographic information). These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)


# A best practice in supervised learning is to further split up
#the training set into a smaller training set and a validation
#set. You can compare the performance of candidate models 
#(each trained on the smaller training set) on the validation set. The following code randomly splits your training set into a smaller training set (75% of the training data) and a validation set (25% of the training data).

# Find the number of rows in the training set.
nrow_train <- nrow(election_data_train)

# Compute the number of rows in the smaller training set.
nrow_small_train <- round(nrow(election_data_train)*0.75)
nrow_validation <- nrow_train - nrow_small_train

# Set the seed for a random sample of the row indices in the smaller training set.
set.seed(9211)
# Sample the row indices in the smaller training set
row_indices_smaller_train <- sample(1:nrow_train, size=nrow_small_train, replace=FALSE)
# Split the training set into the smaller training set and the validation set using these indices. 
election_data_smaller_train <- election_data_train[row_indices_smaller_train,]
election_data_validation <- election_data_train[-row_indices_smaller_train,]

# We put the validation set aside for a moment. The following is an example of exploratory data analysis using the smaller training set. We create a pairwise plot between Obama_margin_percent. The function pairs creates a matrix of pairwise plots between the parameters passed to it. From this you can visualize how X's correlate with the Y.

pairs(~Obama_margin_percent + Region + Black + HighSchool + Poverty + PopDensity + SpeakingNonEnglish + LandArea, data=election_data_smaller_train, pch=".")

# Run a linear regression model of Y variable Obama_margin_percent on X variables Region, Black, HighSchool, Bachelors, Poverty, and PopDensity. The variables are specified in the following way - lm(Y ~ x1 + x2 + .... xn)
election_linear_model <- lm(Obama_margin_percent ~ Region + Black + HighSchool + Poverty + PopDensity + SpeakingNonEnglish + LandArea, data=election_data_smaller_train)
# Use summary function to see the model summary. The summary gives the coefficients, their p-values, and other regression statistics.
summary(election_linear_model)

# The step function in R steps through the regression. The argument backwards tells the function to remove one variable at a time to find best fitting model based on the AIC score. It tries to minimize the AIC score.
election_linear_model_stepped <- step(election_linear_model, direction="backward")
summary(election_linear_model_stepped)

# We construct a regression tree now. To start with, we first install the rpart package required to construct the tree. Once installed, we load it in our current session using library function. 
# NOTE - You only need to install a package once in on your system, but you mustrequire to load it everytime you use the Rpart package. While installing the package, make sure that you are connected to the internet. In our tree, we use the variables that we get in our optimized linear model.

# install.packages("rpart")
library(rpart)
# election_reg_tree <- rpart(Obama_margin_percent ~ Region + Black + HighSchool + Poverty + PopDensity, data=election_data_smaller_train)
election_reg_tree <- rpart(Obama_margin_percent ~ Black + HighSchool + Region + IncomeAbove75K , data=election_data_smaller_train)

# summary gives a summary of the tree.
summary(election_reg_tree)
# Print prints out the text version of the tree nodes. To see the graphics, we use the plot function as described below.
print(election_reg_tree)
# Plot the tree.
plot(election_reg_tree)
# The plotted tree doesn't have any labels. To get the labels, we use the text command. use.n labels the terminal nodes with number of entries in that node and cex specifies text size relative to 1.
text(election_reg_tree, use.n=TRUE, cex=0.5, offset=.5)

# Ok, this tree looks good but we have a way to make it look more beautiful. We use packages names named partykit and party to do that. Let's try it.
#  install.packages("partykit")  # we have this package already.
library(partykit)
library(party)

plot(as.party(election_reg_tree), type="extended")
# Simpler tree to demonstrate concept

tree2 <- ctree(Obama_margin_percent~ Black + HighSchool, 
               data=election_data_smaller_train)
plot(tree2,main="Regression Tree for Obama Margin %", 
     gp=gpar(fontsize=4),
     inner_panel=node_inner,
     ip_args=list(
           abbreviate = TRUE, 
           id = FALSE))

# Now that we have two models (a linear regression and a regression tree), we can compare their prediction accuracy on the validation set. To do this, we first get out prediciton from each of the models. The predict function takes the model as its first argument and the data set with the new Xs as its second argument. Its output is a set of new Y predictions.

reg_predictions <- predict(election_linear_model_stepped,election_data_validation)
tree_predictions <- predict(election_reg_tree,election_data_validation)


# From the forecast package, we use the accuracy function to compute performance measure for our two models' predictions. The accuracy function takes two arguments. The first is the set of predictions, and the second is the set of realizations. Its output is a set of performance measures (Mean Error, Root Mean Squared Error, Mean Absolute Error, Mean Percentage Error, and Mean Absolute Percentage Error). For more information on the measures, see Section 2/5 in FPP by Hyndman and Athanasopoulos (2013): http://otexts.com/fpp/2/5/. 
install.packages("forecast")
library(forecast)
accuracy(reg_predictions,election_data_validation$Obama_margin_percent)
accuracy(tree_predictions,election_data_validation$Obama_margin_percent)

