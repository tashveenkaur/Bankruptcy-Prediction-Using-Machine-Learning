
#Group 1


# clearing object environment
rm(list = ls())

options(repos = "http://cran.rstudio.com")

#install.packages("ggplot2")
install.packages("skimr")
install.packages("GGally")
install.packages("car")
install.packages("nortest")
install.packages("moments")
install.packages("skimr")
install.packages("randomForest")
install.packages("caret")

# Load the required libraries
library(tidyverse) # for data manipulation and visualization
library(skimr) # for summary statistics
library(GGally) # for visualizing pairwise relationships
library(car) # for testing for normality
library(nortest) # for testing for normality
library(moments) # for calculating skewness and kurtosis

# set working directory
setwd("C:/Users/tashv/Desktop/Datascience")

# Loading data file 
df <- read.csv("project_data.csv", header=TRUE)

#Viewing the data 
View(df)
summary(df)
head(df)
tail(df)
names(df)

#Oversampling the bankrupt (response)variable
install.packages("ROSE")
library(ROSE)

# Split the data into predictors and target variable
X <- df[, !(names(df) %in% c("Bankrupt."))]
y <- df$Bankrupt.

# Apply SMOTE using ROSE
oversampled_data <- ovun.sample(Bankrupt. ~ ., data = df, method = "both", p = 0.5, seed = 123)

# Access the oversampled data
new_df <- oversampled_data$data

#Viewing thw original and oversampled bankrupt variable
table(df$Bankrupt.)
table(new_df$Bankrupt.)

#Cleaning the data 

# Total number of missing values in data set
sum(is.na(new_df))

#Structure of the data
str(new_df)


#Removing Net.Income.Flag variable (same values in each row)
new_df <- new_df[, -which(names(new_df) == "Net.Income.Flag")]


#Skim the data
skim(new_df)

# create a list of all column names
all_cols <- colnames(new_df)

# Set the plot margins
par(mar=c(3,3,1,1))

# loop over all columns and create a histogram for equity to liability ratio
for (col in all_cols) {
  hist(new_df[[col]], main=col, xlab="Value")
 }

# Set smaller margins
par(mfrow=c(9,11))
par(mar = c(0.5, 0.5, 0.5, 0.5))

# Set smaller plotting region
par(plt = c(0.05, 0.95, 0.05, 0.95))

# Creating histograms for all features and each histogram represents the distribution of values for a single feature.
for(i in 1:ncol(new_df)) {
  hist(new_df[,i], main = colnames(new_df)[i], xlab = "", ylab = "Frequency", col = "steelblue")
}

# Creating scatterplot fpr accounts recievable turnover ratio vs quick ratio
ggplot(data = new_df, aes(x = Accounts.Receivable.Turnover, y = Quick.Ratio)) + 
  geom_point() + 
  xlab("Accounts.Receivable.Turnover") + 
  ylab("Quick.Ratio") + 
  ggtitle("Scatter plot of Accounts Receivable Turnover vs. Quick Ratio")


# Choosing some continuous numerical variables
numerical_vars <- c("ROA.C..before.interest.and.depreciation.before.interest", 
                    "ROA.A..before.interest.and...after.tax",
                    "Operating.Gross.Margin",
                    "Realized.Sales.Gross.Margin")

# Create histograms for the above selected variables
par(mfrow = c(2, 2))
for (var in numerical_vars) {
  hist(new_df[, var], main = var, xlab = var)
}


# Melt the data
df_melt <- reshape2::melt(new_df, id.vars="Bankrupt.")

# Create a boxplot
ggplot(data = df_melt, aes(x = variable, y = value, fill = as.factor(Bankrupt.))) +
  geom_boxplot() +
  scale_fill_discrete(name = "Bankrupt.") +
  labs(x = "Variable", y = "Value") +
  theme_bw()


#boxplot check outliers
par(mfrow = c(2,4))
for (i in seq(2:9))
  boxplot(new_df[,i],xlab=colnames(new_df)[i],col=i)


# Normality tests for all variables

for (i in 1:ncol(new_df)) {
  n <- sum(!is.na(new_df[,i]))
  if (n >= 3 & n <= 5000) {
    pval_shapiro <- shapiro.test(new_df[,i])$p.value
    pval_ad <- ad.test(new_df[,i])$p.value
    cat(paste0("Variable: ", names(new_df)[i], "\n",
               "Shapiro-Wilk test p-value: ", pval_shapiro, "\n",
               "Anderson-Darling test p-value: ", pval_ad, "\n\n"))
  } else {
    cat(paste0("Variable: ", names(new_df)[i], " excluded due to sample size: ", n, "\n\n"))
  }
}

#Skewness and kurtosis for all variables
for (i in 1:ncol(new_df)) {
  cat(paste0("Variable: ", names(new_df)[i], "\n",
             "Skewness: ", skewness(new_df[,i]), "\n",
             "Kurtosis: ", kurtosis(new_df[,i]), "\n\n"))
}


# Calculate skewness for all variables
skewness_vals <- sapply(new_df, skewness)

# Create a data frame with variable names and skewness values
skewness_df <- data.frame(variable = names(new_df), skewness = skewness_vals)

# Create a bar plot of skewness of the variables
ggplot(skewness_df, aes(x = variable, y = skewness)) +
  geom_bar(stat = "identity") +
  xlab("Variable") +
  ylab("Skewness") +
  ggtitle("Skewness of variables")


# Calculate kurtosis values for each variable
kurt_df <- data.frame(variable = names(new_df),
                      kurtosis = sapply(new_df, kurtosis))

# Create the plot of Kurtosis for each variable
ggplot(kurt_df, aes(x = variable, y = kurtosis)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Variable") +
  ylab("Kurtosis") +
  ggtitle("Kurtosis for each variable") +
  theme_bw()


#setting bankrupsy as target variable 
target_labels = new_df$Bankrupt 


#Standardisation of the features 
norm.data <- as.data.frame(scale(new_df))


#Dividing dataset in training and testing data 
library(caret)
set.seed(123)
train_indices <- sample(nrow(new_df), 0.8 * nrow(new_df))
train_set <- new_df[train_indices, ]
test_set <- new_df[-train_indices, ]


# Change Bankrupt. to a factor variable with levels 0 and 1
train_set$Bankrupt. <- factor(train_set$Bankrupt., levels = c(0, 1), labels("NO","YES"))


#Building Random Forest Model
library(randomForest)
set.seed(123)

#Specify the formula for the model
formula <- Bankrupt. ~ .
train_set <- na.omit(train_set)


#Creating the model
rf_model <- randomForest(formula, data = train_set, ntree = 800, mtry = 3)

#Predicting values for target variables 
predictions <- predict(rf_model,train_set)

# Extract the feature importance measures
feature_importance <- importance(rf_model)

# Sort the features by importance measure
sorted_feature_importance <- sort(feature_importance[,1 ], decreasing = TRUE)
str(feature_importance)

# Extract the top 15 features list from the sorted features importance
list_of_features_importance <- names(sorted_feature_importance[1:15]) # cHANGE THE list accordingly to add more relevant features
print(list_of_features_importance) 

# Print the class of sorted list of features and their importance measures
class(sorted_feature_importance)

# Plot the feature importance measures
varImpPlot(rf_model)


# Create data frame of feature importance
df_features_importance <- data.frame(
  Feature = names(sorted_feature_importance[1:15]),
  Importance = sorted_feature_importance[1:15]
)


# Print the data frame
print(df_features_importance)


#Installing library
library(dplyr)

# Select relevant variables from train_set
train_subset <- train_set %>% 
  select(Net.Value.Growth.Rate, Persistent.EPS.in.the.Last.Four.Seasons, Net.Income.to.Stockholder.s.Equity, 
         Net.profit.before.tax.Paid.in.capital, `Per.Share.Net.profit.before.tax..Yuan...`, Net.Value.Per.Share..C., 
         Borrowing.dependency, Equity.to.Liability, `Degree.of.Financial.Leverage..DFL.`, Net.Value.Per.Share..B., 
         Total.debt.Total.net.worth, Net.Income.to.Total.Assets, Interest.Expense.Ratio, Working.Capital.Equity, 
         Net.worth.Assets)

# Check the summary statistics of the variables in train_subset
summary(train_subset)

## Create a subset of the training set with the relevant variables
train_subset <- subset(train_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity", "Persistent.EPS.in.the.Last.Four.Seasons", "Net.Value.Growth.Rate", "Net.Income.to.Total.Assets", "Net.profit.before.tax.Paid.in.capital", "Per.Share.Net.profit.before.tax..Yuan...", "Equity.to.Liability", "Net.Value.Per.Share..B.", "Degree.of.Financial.Leverage..DFL.", "Interest.Expense.Ratio", "Borrowing.dependency", "Net.Value.Per.Share..C.", "Net.worth.Assets"))

test_subset <- subset(test_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity", "Persistent.EPS.in.the.Last.Four.Seasons", "Net.Value.Growth.Rate", "Net.Income.to.Total.Assets", "Net.profit.before.tax.Paid.in.capital", "Per.Share.Net.profit.before.tax..Yuan...", "Equity.to.Liability", "Net.Value.Per.Share..B.", "Degree.of.Financial.Leverage..DFL.", "Interest.Expense.Ratio", "Borrowing.dependency", "Net.Value.Per.Share..C.", "Net.worth.Assets"))


#Printing the list of sorted features
print(names(train_subset))
print(list_of_features_importance)

#Logistic Regression 

#Install Packages and Library
install.packages("cowplot", dependencies = TRUE)

#Printing names and computing weights 
names(train_set)
weights <- ifelse(train_subset$Bankrupt. == 1, 10, 1)


# Fit the logistic regression model
model <- glm(Bankrupt. ~ ., family = "binomial", data = train_subset, weights = weights)

# Print the summary of the model
summary(model)

# Test for overall statistical significance of the model
summary(model)$null.deviance - summary(model)$deviance
pchisq(summary(model)$null.deviance - summary(model)$deviance, summary(model)$df.null - summary(model)$df.residual)


# Check the values of Bankrupt. in the training set
table(train_set$Bankrupt.)

#Creating the test subset
test_subset <- subset(test_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity", "Persistent.EPS.in.the.Last.Four.Seasons", "Net.Value.Growth.Rate", "Net.Income.to.Total.Assets", "Net.profit.before.tax.Paid.in.capital", "Per.Share.Net.profit.before.tax..Yuan...", "Equity.to.Liability", "Net.Value.Per.Share..B.", "Debt.ratio..", "Degree.of.Financial.Leverage..DFL.", "Interest.Expense.Ratio", "Interest.Coverage.Ratio..Interest.expense.to.EBIT.", "Borrowing.dependency", "Net.Value.Per.Share..C.", "Net.worth.Assets"))


#Creating function for metrics
calculate_metrics <- function(model, data){
  # Predict the classes for the test data
  predictions <- predict(model, data, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  # Calculate the confusion matrix
  cm <- confusionMatrix(factor(predicted_classes), factor(data$Bankrupt.))
  # Calculate the accuracy and R-squared
  accuracy <- round(cm$overall["Accuracy"], 2)
  #rsq <- round(summary(model)$r.squared, 2)
  # Print the confusion matrix, accuracy, and R-squared
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  f1_score <- 2 * precision * recall / (precision + recall)
  
  print(paste("F1Score: ",f1_score))
  print(cm$table)
  print(paste("Accuracy:", accuracy))
}

#Calculating metrics
calculate_metrics(model, test_subset)


# Calculate Cox and Snell's R-squared
null_deviance <- model$null.deviance
residual_deviance <- model$deviance
cox_snell_r_squared <- 1 - (residual_deviance / null_deviance)

# Print the R-squared
print(paste("Cox and Snell's R-squared:", cox_snell_r_squared))


# Remove non-statistically significant variables from the model formula


# Create the train subset with selected variables
train_subset2 <- train_set[, c("Bankrupt.",
                               "Net.Income.to.Total.Assets",
                               "Net.profit.before.tax.Paid.in.capital",
                               "Per.Share.Net.profit.before.tax..Yuan...",
                               "Degree.of.Financial.Leverage..DFL.",
                               "Interest.Expense.Ratio")]


# Create the test subset with selected variables
test_subset2 <- test_set[, c("Bankrupt.",
                               "Net.Income.to.Total.Assets",
                               "Net.profit.before.tax.Paid.in.capital",
                               "Per.Share.Net.profit.before.tax..Yuan...",
                               "Degree.of.Financial.Leverage..DFL.",
                               "Interest.Expense.Ratio")]

# Fit the glm model with the selected variables
model2 <- glm(Bankrupt. ~ 
                Net.Income.to.Total.Assets +
                Net.profit.before.tax.Paid.in.capital +
               Per.Share.Net.profit.before.tax..Yuan... +
                Degree.of.Financial.Leverage..DFL. +
                Interest.Expense.Ratio,
             family = "binomial",
             data = train_subset2)


# Print the summary of the updated model
summary(model2)



# Test for overall statistical significance of the model
summary(model2)$null.deviance - summary(model2)$deviance
pchisq(summary(model2)$null.deviance - summary(model2)$deviance, summary(model2)$df.null - summary(model2)$df.residual)



# Check the values of Bankrupt. in the training set
table(train_subset2$Bankrupt.)

#Creating function for model 2
calculate_metrics <- function(model2, data){
  # Predict the classes for the test data
  predictions <- predict(model2, data, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  # Calculate the confusion matrix
  cm <- confusionMatrix(factor(predicted_classes), factor(data$Bankrupt.))
  # Calculate the accuracy and R-squared
  accuracy <- round(cm$overall["Accuracy"], 2)
  #rsq <- round(summary(model)$r.squared, 2)
  # Print the confusion matrix, accuracy, and R-squared
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  f1_score <- 2 * precision * recall / (precision + recall)
  
  print(paste("F1Score: ",f1_score))
  print(cm$table)
  print(paste("Accuracy:", accuracy))
}

#Calculating metrics for model 2
calculate_metrics(model2, test_subset2)


# Calculate Cox and Snell's R-squared
null_deviance <- model$null.deviance
residual_deviance <- model$deviance
cox_snell_r_squared <- 1 - (residual_deviance / null_deviance)

# Print the R-squared
print(paste("Cox and Snell's R-squared:", cox_snell_r_squared))

#Machine Learning Models

#Finding out the standrad deviation
sds <- apply(df, 2, sd)
print(sds)

##LDA

# Load libraries
library(caret)
library(MASS)
library(reshape2)

# Remove variables with near-zero variance
nzv <- nearZeroVar(train_set[, -1], saveMetrics = TRUE)
train_set <- train_set[, -which(nzv$nzv == TRUE)]

# Remove variables with zero variance
train_set <- train_set[, apply(train_set, 2, function(x) length(unique(x))) > 1]

# Calculate the correlation matrix
cor_mat <- cor(train_set[, -1], use = "pairwise.complete.obs")

# Melt the correlation matrix
cor_mat_melted <- melt(cor_mat)

# Plot the heatmap
ggplot(data = cor_mat_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


# Calculate the correlation matrix
cor_mat <- cor(train_set[, -1], use = "pairwise.complete.obs")

# Find the highly correlated variables
high_cor <- findCorrelation(cor_mat, cutoff = 0.8, verbose = TRUE)

# Select the highly correlated variables
high_cor_vars <- names(train_set)[-1][high_cor]


# Get the column indices for the selected variables
train_set_sel_cols <- c(1, high_cor)



#Load the MASS package for LDA
library(MASS)


# Select the variables for the LDA
LDA_subset <- subset(train_set, select = c("Bankrupt.", "ROA.A..before.interest.and...after.tax",
                                           "ROA.C..before.interest.and.depreciation.before.interest",
                                           "ROA.B..before.interest.and.depreciation.after.tax",
                                           "Persistent.EPS.in.the.Last.Four.Seasons", 
                                           "Per.Share.Net.profit.before.tax..Yuan...",
                                           "Net.profit.before.tax.Paid.in.capital", 
                                           "Operating.Profit.Per.Share..Yuan...",
                                           "Net.Value.Per.Share..A.", 
                                           "Net.Value.Per.Share..C.",
                                           "Debt.ratio..", 
                                           "Net.worth.Assets", 
                                           "Operating.Funds.to.Liability",
                                           "Gross.Profit.to.Sales", 
                                           "Operating.Gross.Margin", 
                                           "Current.Liabilities.Equity",
                                           "Current.Liability.to.Equity", 
                                           "Liability.to.Equity"))



# Create the LDA model
lda_model <- lda(Bankrupt. ~ ., data = LDA_subset , prior = c(0.8, 0.2))

#Creating test subset
LDA_test_subset <- subset(test_set, select = c("Bankrupt.", "ROA.A..before.interest.and...after.tax",
                                               "ROA.C..before.interest.and.depreciation.before.interest",
                                               "ROA.B..before.interest.and.depreciation.after.tax",
                                               "Persistent.EPS.in.the.Last.Four.Seasons", 
                                               "Per.Share.Net.profit.before.tax..Yuan...",
                                               "Net.profit.before.tax.Paid.in.capital", 
                                               "Operating.Profit.Per.Share..Yuan...",
                                               "Net.Value.Per.Share..A.", 
                                               "Net.Value.Per.Share..C.",
                                               "Debt.ratio..", 
                                               "Net.worth.Assets", 
                                               "Operating.Funds.to.Liability",
                                               "Gross.Profit.to.Sales", 
                                               "Operating.Gross.Margin", 
                                               "Current.Liabilities.Equity",
                                               "Current.Liability.to.Equity", 
                                               "Liability.to.Equity"))

#Creating Predictions
lda_pred <- predict(lda_model, newdata = LDA_test_subset)

# Extract the predicted class
lda_class <- ifelse(lda_pred$class == "11", 0, ifelse(lda_pred$class == "12", 1, lda_pred$class))

# Create a confusion matrix
conf_mat <- table(lda_class, LDA_test_subset$Bankrupt.)

# Calculate accuracy
accuracy <- sum(diag(conf_mat))/sum(conf_mat)

# Extract true positive, false positive, and false negative values from the confusion matrix
TP <- conf_mat[2, 2]
FP <- conf_mat[1, 2]
FN <- conf_mat[2, 1]

# Calculate precision and recall
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

# Calculate F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Print the confusion matrix and F1 score
print(conf_mat)
print(paste("F1 score:", round(f1_score, 2)))




##QDA

# Extract the top 15 features list from the sorted features importance
list_of_features_importance1 <- names(sorted_feature_importance[1:20]) # cHANGE THE list accordingly to add more relevant features
print(list_of_features_importance1)


# Subset the training set to include only the top 20 features
train_set_sub <- train_set[, c(1, which(names(train_set) %in% list_of_features_importance1))]

# Calculate the correlation matrix
cor_mat <- cor(train_set_sub[, -1], use = "pairwise.complete.obs")

#Printing names of variables in the training subset
names(train_set_sub)

# Print the correlation matrix
print(cor_mat)

#Installing library
library(reshape2)

# Melt the correlation matrix
cor_mat_melted <- melt(cor_mat)

# Plot the heatmap
ggplot(data = cor_mat_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


# Select the variables for the QDA
QDA_train_subset <- subset(train_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity",
                                               "Persistent.EPS.in.the.Last.Four.Seasons",
                                               "Net.Value.Growth.Rate",
                                               "Net.Income.to.Total.Assets", 
                                               "Net.profit.before.tax.Paid.in.capital",
                                               "Per.Share.Net.profit.before.tax..Yuan..."))

#Printing names of variables in the training subset
names(QDA_train_subset )


# Create the QDA model
qda_model <- qda(Bankrupt. ~ ., data = QDA_train_subset  , prior = c(0.8, 0.2))


QDA_test_subset <- subset(test_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity",
                                               "Persistent.EPS.in.the.Last.Four.Seasons",
                                               "Net.Value.Growth.Rate",
                                               "Net.Income.to.Total.Assets", 
                                               "Net.profit.before.tax.Paid.in.capital",
                                               "Per.Share.Net.profit.before.tax..Yuan..."))

#Creating predictions
qda_pred <- predict(qda_model, newdata = QDA_test_subset)

# Extract the predicted class
qda_class <- ifelse(qda_pred$class == "11", 0, ifelse(qda_pred$class == "12", 1, qda_pred$class))


# Create a confusion matrix
conf_mat <- table(qda_class, QDA_test_subset$Bankrupt.)


# Calculate accuracy
accuracy <- sum(diag(conf_mat))/sum(conf_mat)

precision <- conf_mat[2,2]/sum(conf_mat[,2])
recall <- conf_mat[2,2]/sum(conf_mat[2,])
f1_score <- 2*precision*recall/(precision+recall)

# Print F1 score
print(paste("F1 score:", f1_score))


#KNN

#Installing packages and libraries
install.packages("class")
library(class)
install.packages("caret")  
library(caret)            


# Select the variables for KNN
KNN_subset <- subset(train_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity",
                                           "Persistent.EPS.in.the.Last.Four.Seasons",
                                           "Net.Value.Growth.Rate",
                                           "Net.Income.to.Total.Assets", 
                                           "Net.profit.before.tax.Paid.in.capital",
                                           "Per.Share.Net.profit.before.tax..Yuan..."))

knn_test_set <- subset(test_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity",
                                            "Persistent.EPS.in.the.Last.Four.Seasons",
                                            "Net.Value.Growth.Rate",
                                            "Net.Income.to.Total.Assets", 
                                            "Net.profit.before.tax.Paid.in.capital",
                                            "Per.Share.Net.profit.before.tax..Yuan..."))

# Fit the KNN model
k <-2# number of neighbors to consider

knn_model <- knn(train = KNN_subset[, -1], test = knn_test_set[, -1], cl = train_set$Bankrupt., k = k)
knn_model <- factor(knn_model, levels = c(11, 12), labels = c("Not Bankrupt", "Bankrupt"))
knn_actual <- factor(test_set$Bankrupt., levels = c(0, 1), labels = c("Not Bankrupt", "Bankrupt"))

# Evaluate the performance of the KNN model
knn_pred <- ifelse(knn_model == 1, "Bankrupt", "Not Bankrupt")
table(knn_pred, test_set$Bankrupt.)

# Calculate the accuracy and confusion matrix
confusion_matrix <- confusionMatrix(knn_model, knn_actual)
accuracy <- confusion_matrix$overall['Accuracy']

# Print the results
confusion_matrix
accuracy

precision <- confusion_matrix$byClass['Pos Pred Value']
recall <- confusion_matrix$byClass['Sensitivity']
f1_score <- 2 * precision * recall / (precision + recall)

##Tried the above knn model with k=8 and other values of k as well