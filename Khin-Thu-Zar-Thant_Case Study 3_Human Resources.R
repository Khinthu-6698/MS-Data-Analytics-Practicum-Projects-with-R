##################################### HUMAN RESOURCES ANALYTICS CASE STUDY #######################################

### VARAIBLE DESCRIPTION ###

# Satisfaction Level : Satisfaction Level of the employees (ranging from 0 to 1)

# Last_Evaluation : Latest Evaluation (evaluation conducted yearly)

# Number_project : Number of projects worked on

# Average_Monthly_Hours : Average Monthly hours

# Time_Spend_Company : Time spent at the company (in years)

# Work_Accident : Whether the employees have any work accidents (within the last 2 years)

# Left : Whether the employees have left the job

# Promotion_Last_5years : Whether they have had promotions in the last 5 years

# Sales : Department Name

# Salary : The salary 



### STEP 1 - Data Exploration ###

## Loading the required libraries 
library(dplyr)    # package for data wrangling
library(ggplot2)    # package for data visualization
library(ggpubr)   # package for data visualization
library(caret)    # package for confusion matrix
library(knitr)    # package for tables
library(naniar)   # package for missing data summarization
library(tidyr)  # package for dealing with missing data
library(ROSE)     # package for dealing with imbalance dataset
library(class)    # package for knn model
library(rpart)    # package for classification tree
library(rpart.plot)   # package for classification tree plot
library(e1071)    # package for tuning
library(dummies)    # package for dummy variables
library(corrplot)   # package for correlation matrix
library(randomForest)     #  package for random forest model
library(pROC)       # package for ROC metrics

## Loading the data
setwd("C:/Users/khint/Downloads")
HR <- read.csv("HR.csv")

## Exploring the general overview of the data
summary(HR)
str(HR)


## Checking for missing values
sum(is.na(HR))   # sum of missing values
kable(miss_var_summary(HR))    # summary of where missing values are located

  # --------- It can be seen that there is no missing values in the dataset. 

## Checking for null values
is.null(HR)

  # ---------- It can be seen that there are no null values in the dataset as well

###################################################*################################################


### STEP 2 - Dataset Re-altering ###

## Renaming the Sales column to Department
colnames(HR)[9] <- "Department"
colnames(HR)
tail(HR, 100)

###################################################*################################################


### STEP 3 - General Attribute Analysis With Visualization ###

    ## ------- Total employees (Left and not left) ------ ##

## Distribution of left and not left in the data set
ggplot(HR, aes(x = as.factor(left), fill = as.factor(left))) + 
  geom_bar() + 
  labs(x = "Leaving Status", 
       y = "Number of Employees", 
       title = "Leaving Status Distribution")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status") + 
  geom_text(aes(label = ..count..), stat = "count", 
          vjust = 0, color = "black", 
          position = position_dodge(width = 1))+ 
  scale_fill_discrete(breaks = c("0", "1"), 
                      labels = c("Not Left", "Left"))

HR %>%
  count(left)

  ## Numeric Variables
## Box plot for assessing possible outliers 

par(mfrow = c(2,3))   # setting template for box plot

boxplot(HR$satisfaction_level, main = "Satisfaction Level")
boxplot(HR$last_evaluation, main = "Last Evaluation")
boxplot(HR$number_project, main = "No. Of Projects")
boxplot(HR$average_montly_hours, main = "Avg Hours (Monthly)")
boxplot(HR$time_spend_company, main = "Years in Company")

## Histogram to assess distribution

par(mfrow = c(2,3))   # setting template for box plot

hist(HR$satisfaction_level, main = "Satisfaction Level")
hist(HR$last_evaluation, main = "Last Evaluation")
hist(HR$number_project, main = "No. Of Projects")
hist(HR$average_montly_hours, main = "Avg Hours (Monthly)")
hist(HR$time_spend_company, main = "Years in Company")

  ## Non- numeric variables 
## Bar Graph for remaining non-numeric variables 

a <- ggplot(HR, aes(x = as.factor(Work_accident), fill = as.factor(Work_accident))) + 
  geom_bar() + 
  labs(x = "Work accident", 
       y = "Number of Employees", 
       title = "Work Accident")+ 
  ggeasy::easy_center_title() +
  theme(legend.position = "none")

b <- ggplot(HR, aes(x = as.factor(promotion_last_5years), fill = as.factor(promotion_last_5years))) + 
  geom_bar() + 
  labs(x = "Promotion in Last 5 years", 
       y = "Number of Employees", 
       title = "Promotion")+ 
  ggeasy::easy_center_title() +
  theme(legend.position = "none")

c <- ggplot(HR, aes(x = Department, fill = Department)) + 
  geom_bar() + 
  labs(x = "Department", 
       y = "Number of Employees", 
       title = "Department")+ 
  ggeasy::easy_center_title() +
  theme(legend.position = "none")


d <- ggplot(HR, aes(x = factor(salary), fill = salary)) + 
  geom_bar() + 
  labs(x = "Salary", 
       y = "Number of Employees", 
       title = "Salary")+ 
  ggeasy::easy_center_title() +
  theme(legend.position = "none")

      # arranging the format of the plots
ggarrange(
  c, 
  ggarrange(a, b, d, ncol = 3), 
  nrow = 2
)

    ## ------- Separation of classes (Left versus Not left)------ ##

## Numeric Variables
## Histogram to assess distribution
par(mfrow = c(2,3)) 
left <- HR %>%
  filter(left == 1)

not_left <- HR %>%
  filter(left == 0)

hist(left$satisfaction_level, col = 'red', border = F, main = "Satisfaction Level")
hist(not_left$satisfaction_level, add = T, col = scales::alpha('blue', .5), border = F, main = "Satisfaction Level")
legend("bottom", inset = c(0, -0.62), legend = c("Left", "Not Left"), 
       cex = 0.55, col = c("red", "skyblue"), 
       pch = 15, xpd = TRUE, horiz = TRUE)

hist(left$last_evaluation, col = 'red', border = F, main = "Last Evaluation")
hist(not_left$last_evaluation, add = T, col = scales::alpha('blue', .5), border = F)
legend("bottom", inset = c(0, -0.62), legend = c("Left", "Not Left"), 
       cex = 0.5, col = c("red", "skyblue"), 
       pch = 15, xpd = TRUE, horiz = TRUE)

hist(left$number_project, col = 'red', border = F, main = "No. of Projects")
hist(not_left$number_project, add = T, col = scales::alpha('blue', .5), border = F)
legend("bottom", inset = c(0, -0.62), legend = c("Left", "Not Left"), 
       cex = 0.5, col = c("red", "skyblue"), 
       pch = 15, xpd = TRUE, horiz = TRUE)

hist(left$average_montly_hours, col = 'red', border = F, main = "Avg Hours (Monthly)")
hist(not_left$average_montly_hours, add = T, col = scales::alpha('blue', .5), border = F)
legend("bottom", inset = c(0, -0.62), legend = c("Left", "Not Left"), 
       cex = 0.5, col = c("red", "skyblue"), 
       pch = 15, xpd = TRUE, horiz = TRUE)

hist(left$time_spend_company, col = 'red', border = F, main = "Years in Company")
hist(not_left$time_spend_company, add = T, col = scales::alpha('blue', .5), border = F)
legend("bottom", inset = c(0, -0.62), legend = c("Left", "Not Left"), 
       cex = 0.5, col = c("red", "skyblue"), 
       pch = 15, xpd = TRUE, horiz = TRUE)


## Non- numeric variables 
## Bar Graph for remaining non-numeric variables 

a1 <- ggplot(HR, aes(x = as.factor(Work_accident), fill = as.factor(left))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Work accident", 
       y = "Number of Employees", 
       title = "Work Accident")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status") 
  

b1 <- ggplot(HR, aes(x = as.factor(promotion_last_5years), fill = as.factor(left))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Promotion in Last 5 years", 
       y = "Number of Employees", 
       title = "Promotion")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status") 


c1 <- ggplot(HR, aes(x = as.factor(Department), fill = as.factor(left))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Departments", 
       y = "Number of Employees", 
       title = "Department")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status") 


d1 <- ggplot(HR, aes(x = as.factor(salary), fill = as.factor(left))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Salary", 
       y = "Number of Employees", 
       title = "Salary")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status")


  # arranging the format of the plots
ggarrange(
  c1, 
  ggarrange(a1, b1, d1, ncol = 3), 
  nrow = 2
)  
  

## Plotting correlation matrix to analyze general correlations between the variables as well as to the target variables

  # Converting the variables as numeric to plot for correlation
HR_ <- sapply(HR[, -c(9, 10)], as.numeric)
str(HR_)

HR.cor <- cor(HR_)
dev.off()
corrplot(HR.cor)

  # Doing the same for left group to compare the overall variables correlation between the overall group and those who left 
HR_left <- sapply(left[,-c(7, 9, 10)], as.numeric)
HR.cor_left <- cor(HR_left)
corrplot(HR.cor_left)

###################################################*################################################


### STEP 4 - Testing Hypothesis ###

  ## Hypothesis 1 - Salary is the reason why the employees left the company ##
# Analyzing the number of employees left
HR %>%
  count(left)

# Re-plotting the bar graph with labels this time
ggplot(HR, aes(x = as.factor(salary), fill = as.factor(left))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Salary", y = "Number of Employees", 
       title = "Salary")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status") + 
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 0, color = "black", 
            position = position_dodge(width = 1))+ 
  scale_fill_discrete(breaks = c("0", "1"), 
                      labels = c("Not Left", "Left"))

# -- It can be seen that out of the 3,571 employees who left the company, more than half of them belongs
# to the low salary group with one third of them being in the medium salary group. Although this graph
# alone could not be the only reason why employees left the company. However, since the numbers could be
# be one of the leading factors of the employee turnover and hence, the hypothesis of stating salary is the
# reason why employees left the company is ACCEPTED--


  ## Hypothesis 2 - Employees leave the company because work is not safe ##
# -- By saying work is not safe, the variable that would be looked into will be the work accident. 
# The higher the work accident appears in the graph of the employees who left, the more
# it is likely to be a factor on why employees left --

# Retrieving the plot as well as re-plotting to only compare the left class
a1
ggplot(HR, aes(x = as.factor(Work_accident), fill = as.factor(left))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Work accident", 
       y = "Number of Employees", 
       title = "Work Accident")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status")+ 
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 0, color = "black", 
            position = position_dodge(width = 1))+ 
  scale_fill_discrete(breaks = c("0", "1"), 
                      labels = c("Not Left", "Left")) 

# -- It can be seen that out of the 3,571 employees who left the company, over 3,402 of them
# have not left the company and the employees who left are only 169. This proportion is not that
# strong enough to argue that the workplace is unsafe and in fact the work accident is not
# the best metric to measure if the workplace is safe or not. Work accident could be not only the
# workplace unsafe, it could also be the personal accident of the employees due to their own negligence. 
# However, since in this dataset, this variable is the closest, it is used to compare and the result show
# that work accident is not a major factor to leading employees to leave the company so the hypothesis of 
# stating the workplace is unsafe is REJECTED. 



  ## Hypothesis 3 - This company is a good place to grow##
# This section will emphasize in capturing the pattern of the left employees as well as the
# more capable employees who ended up leaving in order to analyze the reasons of why those
# employees left and based on this to assume the conditions of the workplace and determine if this 
# is a great place for the employees to stay

# -- here the work accident variable is excluded since it has been proven that it is
# not a factor for the employee turnover

# Filtering the left data with only more capable employees by filtering
# employees with evaluation more than average (0.7)
left_1 <- left %>%
  filter(last_evaluation > mean(last_evaluation))
    # --- this will give only the capable employees with higher score who have left

# First see how variables seem like with just the employees who have left
 
par(mfrow = c(2,4)) 
hist(left$satisfaction_level, col = 'red', border = F, main = "Satisfaction Level")
hist(left$number_project, col = 'red', border = F, main = "No. of Projects")
hist(left$average_montly_hours, col = 'red', border = F, main = "Avg Hours (Monthly)")
hist(left$time_spend_company, col = 'red', border = F, main = "Years in Company")
barplot(prop.table(table(left$promotion_last_5years)), 
        col = "red",
        main = "Promotion Last 5 Years")
barplot(prop.table(table(left$salary)), 
        col = "red",
        main = "Salary")

barplot(prop.table(table(left$Department)), 
        col = "red",
        main = "Department")

# Compare with those with only more capable employees
par(mfrow = c(2,4)) 
hist(left_1$satisfaction_level, col = 'blue', border = F, main = "Satisfaction Level")
hist(left_1$number_project, col = 'blue', border = F, main = "No. of Projects")
hist(left_1$average_montly_hours, col = 'blue', border = F, main = "Avg Hours (Monthly)")
hist(left_1$time_spend_company, col = 'blue', border = F, main = "Years in Company")
barplot(prop.table(table(left_1$promotion_last_5years)), 
        col = "blue",
        main = "Promotion Last 5 Years")
barplot(prop.table(table(left_1$salary)), 
        col = "blue",
        main = "Salary")
barplot(prop.table(table(left_1$Department)), 
        col = "blue",
        main = "Department")

dev.off()

ggplot(left, aes(x = as.factor(Department), fill = as.factor(Department))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Departments", 
       y = "Number of Employees", 
       title = "Departments")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Leaving Status")+ 
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 0, color = "black", 
            position = position_dodge(width = 1)) 

###################################################*################################################


### STEP 5 - Data Partitioning ###

  # Converting some variables to categorical
HR$Work_accident <- as.factor(HR$Work_accident)
HR$left <- as.factor(HR$left)
HR$promotion_last_5years <- as.factor(HR$promotion_last_5years)
HR$Department <- as.factor(HR$Department)
HR$salary <- as.factor(HR$salary)
str(HR)


HR %>%
  count(left)    # checking the target class proportions to see if it is imbalanced

# --- It seems that the ratio between the left and not left is around 2:8 which is not that heavily imbalanced
# so balancing the data is not necessary in this case since after partition, their ratio difference
# will be fewer, and this can be checked again after data partition. 

## Partitioning the data into 70% training and 30% testing
set.seed(12345)

HR_index <- createDataPartition(HR$left, p = 0.7, list = FALSE)
HR_train <- HR[HR_index,]   # training data

HR_test <- HR[-HR_index,]   # testing data


kable(HR_train %>%
        count(left))        # distribution for train data

kable(HR_test %>%
        count(left))         # distribution for test data

# --- It can be seen that the proportion between the two data- training and testing seems reasonable and not 
# imbalanced, so this makes another support ground for not performing the balancing of the data

###################################################*################################################


### STEP 6 - Classification Models: Model Fitting ###

# Setting train controls
mycontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = FALSE)


## Model 1: Classification Tree ## 
tree_mod <- train(left ~., data = HR_train, method = "rpart", trControl=mycontrol,
                  tuneLength = 10, maxdepth = 5)

  # * Plot the tree *# 
rpart.plot(tree_mod$finalModel)

plot(tree_mod$finalModel)
text(tree_mod$finalModel)

  # Check for CP values to see for possible pruning
plot(tree_mod)

#--- It can be seen that as the CP value increases, the accuracy decreases and this shows
# that as the larger the split, the larger the accuracy (since the split increases as the CP value decreases)
# However, pruning is necessary to avoid overfitting of the train data which will affect the accuracy
# of the testing data and eventually the accuracy of the new data, so in this case, 
# the tree will be pruned to cp value of 0.07 in compromise to having favorable accuracy and at the same 
# time to produce a decent plot. 

tree_mod_pruned <- prune(tree_mod$finalModel, cp = 0.07)
rpart.plot(tree_mod_pruned)
tree_mod_pruned$variable.importance

  # Predicting the outcomes - train data
pred_tree_train <- predict(tree_mod, newdata = HR_train)
str(pred_tree_train)
  # Predicting the outcomes - test data
pred_tree_test <- predict(tree_mod, newdata = HR_test)



## Model 2: Logistic Regression ##
log_mod <- train(left ~., data = HR_train, method = "glm", trControl = mycontrol, 
                 family = "binomial")

  # Predicting the outcomes - train data
pred_log_train <- predict(log_mod, newdata = HR_train)

  # Predicting the outcomes - test data
pred_log_test <- predict(log_mod, newdata = HR_test)


# --- Comparing these two models, the tree model definitely has better metrics. Not only the accuracy and specificity 
# rate lower, but also significantly the sensitivity rate is way too low with both the sensitivity rate for the testing
# and training data is only over 35%, which is not favorable to correctly predict the employees who are bound to leave. 
# --- Therefore, random forest model will be used to compare with the tree model before
# finalizing which model will be used for deployment


## RANDOM FOREST MODEL ##
# randfor_mod <- train(left ~., data = HR_train, method = "rf", trControl = mycontrol)

randfor_mod <- randomForest(left ~., data = HR_train)

  # Predicting the outcomes - train data
pred_rand_train <- predict(randfor_mod, newdata = HR_train)

  # Predicting the outcomes - test data
pred_rand_test <- predict(randfor_mod, newdata = HR_test)


###################################################*################################################


### STEP 7 - Evaluating the model performance ###

      ## confusion Matrix ##

## Model 1: Classification Tree ## 

  # Evaluating the model performance - train data
confusionMatrix(pred_tree_train, HR_train$left, positive = "1")

  # Evaluating the model performance - test data
confusionMatrix(pred_tree_test, HR_test$left, positive = "1")



## Model 2: Logistic Regression ##

  # Evaluating the model performance - train data
confusionMatrix(pred_log_train, HR_train$left, positive = "1")

  # Evaluating the model performance - test data
confusionMatrix(pred_log_test, HR_test$left, positive = "1")


## RANDOM FOREST MODEL ##

  # Evaluating the model performance - train data
confusionMatrix(pred_rand_train, HR_train$left, positive = "1")

  # Evaluating the model performance - test data
confusionMatrix(pred_rand_test, HR_test$left, positive = "1")



    ## ROC Curve ## 

## Model 1: Classification Tree ## 
par(mfrow = c(3,2))
## ROC for Classification Tree - Training data
pred_tree_train
roc_tree_train <- roc(as.numeric(HR_train$left), as.numeric(pred_tree_train), 
                     percent = TRUE)
plot(roc_tree_train, print.auc = TRUE, col = "red", main = "ROC Curve for Classification Tree_Training Data")


## ROC for Classification Tree - Testing data
pred_tree_test
roc_tree_test <- roc(as.numeric(HR_test$left), as.numeric(pred_tree_test), 
                     percent = TRUE)
plot(roc_tree_test, print.auc = TRUE, col = "red", main = "ROC Curve for Classification Tree_Testing Data")



## Model 2: Logistic Regression ##

## ROC for Logistic Regression - Training data
pred_log_train
roc_log_train <- roc(as.numeric(HR_train$left), as.numeric(pred_log_train), 
                     percent = TRUE)
plot(roc_log_train, print.auc = TRUE, col = "red", main = "ROC Curve for Logistic Regression_Training Data")


## ROC for Logistic Regression - Testing Data
roc_log_test <- roc(as.numeric(HR_test$left), as.numeric(pred_log_test), 
                    percent = TRUE)
plot(roc_log_test, print.auc = TRUE, col = "red", main = "ROC Curve for Logistic Regression_Testing Data")



## RANDOM FOREST MODEL ##

## ROC for Random forest - Training data
pred_rand_train
roc_rand_train <- roc(as.numeric(HR_train$left), as.numeric(pred_rand_train), 
                     percent = TRUE)
plot(roc_rand_train, print.auc = TRUE, col = "red", main = "ROC Curve for Random Forest_Training Data")


## ROC for Random forest - Testing data
pred_rand_test
roc_rand_test <- roc(as.numeric(HR_test$left), as.numeric(pred_rand_test), 
                     percent = TRUE)
plot(roc_rand_test, print.auc = TRUE, col = "red", main = "ROC Curve for Random Forest_Testing Data")

