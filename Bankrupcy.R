 
#Group 1


# clearing object environment
rm(list = ls())

options(repos = "http://cran.rstudio.com")

#install.packages("ggplot2")
library(ggplot2)
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
setwd("C:/Users/Admin_2.LAPTOP-V5OO7B9P/OneDrive/Desktop/582 final project")

# Loading data file 
df <- read.csv("project_data.csv", header=TRUE)

#Viewing the data 
#View(df)
summary(df)
head(df)
tail(df)
names(df)

#Cleaning the data 

# Total number of missing values in data set
sum(is.na(df))

#Structure of the data
str(df)

# assuming your dataframe is named 'df'
# create a list of all column names
all_cols <- colnames(df)

# Set the plot margins
par(mar=c(3,3,1,1))

# loop over all columns and create a histogram for each
for (col in all_cols) {
  hist(df[[col]], main=col, xlab="Value")
}

# Set smaller margins
par(mfrow=c(9,11))
par(mar = c(0.5, 0.5, 0.5, 0.5))

# Set smaller plotting region
par(plt = c(0.05, 0.95, 0.05, 0.95))

# Create histograms for all features
for(i in 1:ncol(df)) {
  hist(df[,i], main = colnames(df)[i], xlab = "", ylab = "Frequency", col = "steelblue")
}

# Create scatterplot for all features
ggplot(data = df, aes(x = Accounts.Receivable.Turnover, y = Quick.Ratio)) + 
  geom_point() + 
  xlab("Accounts.Receivable.Turnover") + 
  ylab("Quick.Ratio") + 
  ggtitle("Scatter plot of Accounts Receivable Turnover vs. Quick Ratio")


# Identify continuous numerical variables
numerical_vars <- c("ROA.C..before.interest.and.depreciation.before.interest", 
                    "ROA.A..before.interest.and...after.tax",
                    "Operating.Gross.Margin",
                    "Realized.Sales.Gross.Margin")

# Create histograms for selected variables
par(mfrow = c(2, 2))
for (var in numerical_vars) {
  hist(df[, var], main = var, xlab = var)
}

# Summary statistics for all variables
skim(df)

# Histograms for all features 
df %>% 
  select(-Bankrupt.) %>% 
  map(~ ggplot(data = df, aes(x = .)) +
        geom_histogram(bins = 30) +
        ggtitle(names(df)[which(names(df) == names(.))]) +
        theme_bw())

# Normality tests for all variables

for (i in 1:ncol(df)) {
  n <- sum(!is.na(df[,i]))
  if (n >= 3 & n <= 5000) {
    pval_shapiro <- shapiro.test(df[,i])$p.value
    pval_ad <- ad.test(df[,i])$p.value
    cat(paste0("Variable: ", names(df)[i], "\n",
               "Shapiro-Wilk test p-value: ", pval_shapiro, "\n",
               "Anderson-Darling test p-value: ", pval_ad, "\n\n"))
  } else {
    cat(paste0("Variable: ", names(df)[i], " excluded due to sample size: ", n, "\n\n"))
  }
}

#Skewness and kurtosis for all variables
for (i in 1:ncol(df)) {
  cat(paste0("Variable: ", names(df)[i], "\n",
            "Skewness: ", skewness(df[,i]), "\n",
             "Kurtosis: ", kurtosis(df[,i]), "\n\n"))
}


#setting bankrupsy as target variable 
target_labels = df$Bankrupt 


#Removing Net.Income.Flag variable (same values in each row)
df <- df %>% select(-Net.Income.Flag)

#Standardisation of the features 
norm.data <- as.data.frame(scale(df))


#Dividing dataset in training and testing data 
library(caret)
set.seed(123)
train_indices <- sample(nrow(df), 0.8 * nrow(df))
train_set <- df[train_indices, ]
test_set <- df[-train_indices, ]

 
# Change Bankrupt. to a factor variable with levels 0 and 1
train_set$Bankrupt. <- factor(train_set$Bankrupt., levels = c(0, 1), labels("NO","YES"))


#Building Random Forest Model
library(randomForest)
set.seed(123)

#Specify the formula for the model
formula <- Bankrupt. ~ .
train_set <- na.omit(train_set)

#Creating the model
#rf_model <- randomForest(formula, data = train_set, ntree = 800, mtry = 3)
# Extract the feature importance measures
#feature_importance <- importance(rf_model, type = 1)
# Sort the features by importance measure
#sorted_feature_importance <- sort(feature_importance[, 1], decreasing = TRUE)

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

# Add the Bankrupt. variable to the data frame
#df_features_importance <- cbind(Bankrupt. = train_set$Bankrupt., df_features_importance)

# Print the data frame
print(df_features_importance)

#df_features_importance
# Create binary variable indicating bankruptcy status
#train_set$Bankrupt. <- ifelse(train_set$Bankrupt. == 1, "Yes", "No")
#nrow(train_set$Bankrupt.)
# Fit logistic regression model
#log_model <- glm(train_set$Bankrupt. ~ ., data = df_features_importance, family = "binomial")

# Print summary of model
#summary(log_model)



#Logistic Regression 

#Install Packages and Library
install.packages("cowplot", dependencies = TRUE)

# Create a subset of the training set with the relevant variables
train_subset <- subset(train_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity", "Persistent.EPS.in.the.Last.Four.Seasons", "Net.Value.Growth.Rate", "Net.Income.to.Total.Assets", "Net.profit.before.tax.Paid.in.capital", "Per.Share.Net.profit.before.tax..Yuan...", "Equity.to.Liability", "Net.Value.Per.Share..B.", "Debt.ratio..", "Degree.of.Financial.Leverage..DFL.", "Interest.Expense.Ratio", "Interest.Coverage.Ratio..Interest.expense.to.EBIT.", "Borrowing.dependency", "Net.Value.Per.Share..C.", "Net.worth.Assets"))

weights <- ifelse(train_subset$Bankrupt. == 1, 10, 1)
# Fit a logistic regression model with Bankrupt. as the response variable
LR <- glm(Bankrupt. ~ ., data = train_subset, family = "binomial", weights = weights)

# Print the summary of the model
summary(LR)

# Check the values of Bankrupt. in the training set
table(train_set$Bankrupt.)

test_subset <- subset(test_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity", "Persistent.EPS.in.the.Last.Four.Seasons", "Net.Value.Growth.Rate", "Net.Income.to.Total.Assets", "Net.profit.before.tax.Paid.in.capital", "Per.Share.Net.profit.before.tax..Yuan...", "Equity.to.Liability", "Net.Value.Per.Share..B.", "Debt.ratio..", "Degree.of.Financial.Leverage..DFL.", "Interest.Expense.Ratio", "Interest.Coverage.Ratio..Interest.expense.to.EBIT.", "Borrowing.dependency", "Net.Value.Per.Share..C.", "Net.worth.Assets"))


#lm_model <- lm(train_set$Bankrupt. ~ ., data= df_features_importance)
#summary(lm_model)


# Fit logistic regression model
#LR <- glm( Bankrupt., data = data.frame(list_of_features_importance, df$Bankrupt.), family = binomial())

# Print model summary
#summary(lm_model)

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
  #print(paste("R-squared:", rsq))
}

calculate_metrics(LR, test_subset)

#Machine Learning Models

#Finding out the standrad deviation
sds <- apply(df, 2, sd)
print(sds)

##LDA

# Load the caret package
library(caret)

library(MASS)



# Remove variables with near-zero variance
nzv <- nearZeroVar(train_set[, -1], saveMetrics = TRUE)
train_set <- train_set[, -which(nzv$nzv == TRUE)]

# Remove variables with zero variance
train_set <- train_set[, apply(train_set, 2, function(x) length(unique(x))) > 1]

# Calculate the correlation matrix
cor_mat <- cor(train_set[, -1], use = "pairwise.complete.obs")

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

lda_pred <- predict(lda_model, newdata = LDA_test_subset)

# Extract the predicted class
lda_class <- lda_pred$class

# Create a confusion matrix
conf_mat <- table(lda_class, LDA_test_subset$Bankrupt.)

# Calculate accuracy
accuracy <- sum(diag(conf_mat))/sum(conf_mat)






# Calculate the correlation matrix
#cor_mat <- cor(train_set[, -1], use = "pairwise.complete.obs")

# Sort the correlations in ascending order and get the values of the least collinear variables
#cor_ordered <- sort(cor_mat[1, -ncol(train_set):-1])[1:10]

# Get the names of the least collinear variables
#least_collinear_vars <- names(which(cor_mat[1, -ncol(train_set):-1] %in% cor_ordered))

# Get the column indices for the selected variables
#train_set_sel_cols <- c(1, which(colnames(train_set)[-1] %in% least_collinear_vars))

# Create the new dataset with the selected variables
#train_set_sel <- train_set[, train_set_sel_cols]

# Check the variable names of the selected dataset
#names(train_set_sel)



##QDA

# Extract the top 15 features list from the sorted features importance
list_of_features_importance1 <- names(sorted_feature_importance[1:20]) # cHANGE THE list accordingly to add more relevant features
print(list_of_features_importance1)



# Subset the training set to include only the top 20 features
train_set_sub <- train_set[, c(1, which(names(train_set) %in% list_of_features_importance1))]

# Calculate the correlation matrix
cor_mat <- cor(train_set_sub[, -1], use = "pairwise.complete.obs")

names(train_set_sub)

# Print the correlation matrix
print(cor_mat)

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
QDA_subset <- subset(train_set_sub, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity",
                                           "Persistent.EPS.in.the.Last.Four.Seasons",
                                           "Net.Value.Growth.Rate",
                                           "Net.Income.to.Total.Assets", 
                                           "Net.profit.before.tax.Paid.in.capital",
                                           "Per.Share.Net.profit.before.tax..Yuan..."))



names(QDA_subset)


# Create the QDA model
qda_model <- qda(Bankrupt. ~ ., data = QDA_subset , prior = c(0.8, 0.2))

#This error usually occurs when there are too many variables relative to the number of observations, causing perfect separation or multicollinearity issues. You may need to reduce the number of variables or obtain more data to build a reliable QDA model.

QDA_test_subset <- subset(test_set, select = c("Bankrupt.", "Net.Income.to.Stockholder.s.Equity",
                                               "Persistent.EPS.in.the.Last.Four.Seasons",
                                               "Net.Value.Growth.Rate",
                                               "Net.Income.to.Total.Assets", 
                                               "Net.profit.before.tax.Paid.in.capital",
                                               "Per.Share.Net.profit.before.tax..Yuan..."))

qda_pred <- predict(qda_model, newdata = QDA_test_subset)

# Extract the predicted class
qda_class <- qda_pred$class

length(QDA_test_subset$Bankrupt)

# Create a confusion matrix
conf_mat <- table(qda_class, QDA_test_subset$Bankrupt.)
#confusion_matrix <- confusionMatrix(qda_model, QDA_test_subset$Bankrupt.)

# Calculate accuracy
accuracy <- sum(diag(conf_mat))/sum(conf_mat)

precision <- conf_mat[2,2]/sum(conf_mat[,2])
recall <- conf_mat[2,2]/sum(conf_mat[2,])
f1_score <- 2*precision*recall/(precision+recall)

# Print F1 score
print(paste("F1 score:", f1_score))


#KNN

library(class)


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
k <- 2# number of neighbors to consider

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
f1_score

##SVM

library(e1071)


install.packages("xgboost")

library(xgboost)

# Split the training set into predictor variables and response variable
xgb_train_x <- as.matrix(KNN_subset[, -1])
xgb_train_y <- as.numeric(KNN_subset$Bankrupt.) - 1

# Convert test set to matrix format
xgb_test_x <- as.matrix(knn_test_set[, -1])
xgb_test_y <- as.numeric(knn_test_set$Bankrupt.) - 1

# Train the XGBoost model with default parameters
xgb_model <- xgboost(data = xgb_train_x, label = xgb_train_y, nrounds = 10, objective = "binary:logistic")

# Use the XGBoost model to predict on the test set
xgb_pred <- predict(xgb_model, xgb_test_x)

# Convert the probability predictions to class labels (0 or 1)
xgb_pred_class <- ifelse(xgb_pred > 0.5, 1, 0)

# Calculate the accuracy and F1 score
xgb_acc <- mean(xgb_pred_class == xgb_test_y) * 100
xgb_f1 <- 2 * sum(xgb_pred_class * xgb_test_y) / (sum(xgb_pred_class) + sum(xgb_test_y))

# Print the accuracy and F1 score
cat("Accuracy: ", round(xgb_acc, 2), "%\n")
cat("F1 score: ", round(xgb_f1, 2), "\n")
