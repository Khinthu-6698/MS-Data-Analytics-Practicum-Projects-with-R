##################################### MORTGAGE PAYBACK CASE STUDY #######################################

### VARAIBLE DESCRIPTION ###

# ID : Borrower ID

# Time : Time Stamp of observation

# Orig_time : Time stamp for origination

# First_time : Time stamp for first observation

# Mat_time : Time stamp for maturity

# Balance_time: Outstanding balance at observation time

# LTV_time : Loan To Value ratio at observation time (in %)
# -------- calculated by amount borrowed / appraised value

# Interest_rate_time : Interest rate at observation time (in %)

# Hpi_time : House price index at observation time (base year = 10)

# GDP_time : Gross domestic product (GDP) growth at observation time (in %)

# Uer_time : Unemployement rate at observation time (in %)

# REtype_CO_orig_time : Real estate type 
# (condominium = 1, otherwise = 0)

# REtype_PU_orig_time: Real estate type planned 
# (urban development = 1, otherwise = 0)

# REtype_SF_orig_time: Single-family home = 1, otherwise = 0

#	Investor_orig_time: Investor borrower = 1, otherwise = 0

#	Balance_orig_time: Outstanding balance at origination time

#	FICO_orig_time: FICO score at origination time, in %

#	LTV_orig_time: Loan-to-value ratio at origination time, in %

#	Interest_Rate_orig_time: Interest rate at origination time, in %

#	Hpi_orig_time: House price index at origination time (base year = 100)

# Default_time: Default observation at observation time 
# (Default = 1, otherwise = 0)

#	payoff_time: Payoff observation at observation time 
# (Paid off = 1, otherwise = 0)

#	status_time: observation at observation time 
# (Default = 1, payoff = 2, and nondefault/nonpayoff = 0) 



### STEP 1 - Data Exploration ###

## Loading the required libraries 
library(dplyr)    # package for data wrangling
library(ggplot2)    # package for data visualization
library(caret)    # package for confusion matrix
library(knitr)    # package for tables
library(naniar)   # package for missing data summarization
library(mice)     # package for dealing with missing data
library(tidyr)  # package for dealing with missing data
library(imbalance)   # package for dealing with imbalance dataset
library(ROSE)     # package for dealing with imbalance dataset
library(class)    # package for knn model
library(rpart)    # package for classification tree
library(rpart.plot)   # package for classification tree plot
library(e1071)    # package for tuning
library(neuralnet)   # package for neural net model
library(dummies)    # package for dummy variables
library(corrplot)   # package for correlation matrix


## Load the file 
setwd("C:/Users/khint/Documents/Course Materials/Graduate (St. Louis)/2022/Fall Semester_2022/CSDA 6010 Analytic Practicum/Project 2 - Mortgage Payback")
mortgage <- read.csv("Mortgage.csv")


## Exploring the general overview of the data
summary(mortgage)
str(mortgage)


## Checking for missing values
sum(is.na(mortgage))   # sum of missing values
kable(miss_var_summary(mortgage))    # summary of where missing values are located

# ---------- it can be seen that all the missing values are from the LTV_time column


## Checking for null values
is.null(mortgage)

# ---------- there are no null values in the data


## Analyzing the column with missing values
m <- mortgage %>%
  filter(is.na(LTV_time))


## Dealing with missing value
md.pattern(mortgage)
imp <- mice(mortgage, m = 1, maxit = 2, seed = 12345)
imp
mortgage_new <- mice::complete(imp)
summary(mortgage)
summary(mortgage_new)


###################################################*################################################


### STEP 2 - Dataset Re-altering ###

# Adding the count of the ID for every observations
mortgage_new <- mortgage_new %>%
  add_count(id)

  # rename the column to count
colnames(mortgage_new)[24] <- "count"

# remove the two default and payoff columns
mortgage_new <- subset(mortgage_new, select = -c(21,22))

# Subset for dataset with default observations only
mortgage_def <- mortgage_new %>%
  filter(status_time == 1)

# Subset for dataset with payoff observations only
mortgage_payoff <- mortgage_new %>%
  filter(status_time == 2)


# Subset for the rest of the data with 0
#--- IDs which have been declared as default/payoff will be removed
d <- mortgage_def$id
p <- mortgage_payoff$id

default_id_obs <- subset(mortgage_new, id %in% d)  # data with default IDs and their respective observations
payoff_id_obs <- subset(mortgage_new, id %in% p) # data with payoff IDs and their respective observations


# new data (with neither default nor payoff IDs)
unknown_obs <- anti_join(mortgage_new, default_id_obs, by = c("id"))
unknown_obs <- anti_join(unknown_obs, payoff_id_obs, by = c("id"))
# --- now that unknown_obs is the data with IDs which has neither been defaulted nor paid off and
# --- this will be used to predict if they are to be defaulted (1) or payoff (2)

u <- setdiff(mortgage_new, default_id_obs)
u <- setdiff(u, payoff_id_obs)
###################################################*################################################


### STEP 3 - General Attribute Analysis With Visualization ###


## Class of mortgage status
barplot(prop.table(table(mortgage_new$status_time)), 
        col = rainbow(3),
        main = "Mortgage Status Distribution")

mortgage_new %>%
  count(status_time)

### It can be clearly seen that the 0 class distribution outnumbered the other two which is
### unrealistic, and this shows that some of the 0 class belongs to the other two classes (since
### before it is declared as the either 1 or 2 class, there are observations which have class 0). Therefore, 
### for the actual status distribution, it has to be filtered out. 

  # Actual Mortgage Status Distribution
    # -- creating a fake data to create plot
def <- nrow(mortgage_def)
pay <- nrow(mortgage_payoff)
none <- 50000 - def - pay


Count <- c(rep(1, def), rep(2, pay), rep(0, none))
ID <- 1:50000

df <- data.frame(ID, Count)
df$Count <- as.factor(df$Count)

b <- barplot(prop.table(table(df$Count)), 
             col = c("red", "green", "blue"),
             main = "Actual Mortgage Status Distribution") 

legend("topleft", legend = c("None", "Defaulted", "Payoff"), 
       col = c("red", "green", "blue"), 
       bty = "n", pch = 20, pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))
text(b, labels = c(8253, 15158,26589), xpd = TRUE)  #######

df%>%
  count(Count)
## Box plot for accessing possible outliers

par(mfrow = c(2, 4))   # setting template for box plot

boxplot(mortgage$id, main = "Borrower ID")
boxplot(mortgage$time, main = "Observation Time")
boxplot(mortgage$orig_time, main = "Orignation Time")
boxplot(mortgage$first_time, main = "First Observation")
boxplot(mortgage$mat_time, main = "Maturity Timestamp")
boxplot(mortgage$balance_time, main = "Outstanding Bal at Observation Time")
boxplot(mortgage$LTV_time, main = "LTV Ratio at Observation Time")
boxplot(mortgage$interest_rate_time, main = "Interest Rate at Observation Time")
boxplot(mortgage$hpi_time, main = "HPI at Observation Time")
boxplot(mortgage$gdp_time, main = "GDP Growth at Observation Time")
boxplot(mortgage$uer_time, main = "Unemploy Rate at Observation Time")
boxplot(mortgage$balance_orig_time, main = "Outstanding Bal at Origination Time")
boxplot(mortgage$FICO_orig_time, main = "FICO at Origination Time")
boxplot(mortgage$LTV_orig_time, main = "LTV Ratio at Origination Time")
boxplot(mortgage$Interest_Rate_orig_time, main = "Interest Rate at Origination Time")
boxplot(mortgage$hpi_orig_time, main = "HPI at Origination Time")



## Histograms / Bar Graphs for variables
dev.off()
par(mfrow = c(2, 4))   # setting template for histogram

  # Histogram
hist(mortgage$id, main = "Borrower ID", col = "blue", border = F)
hist(mortgage$time, main = "Observation Time", col = "blue", border = F)
hist(mortgage$orig_time, main = "Orignation Time", col = "blue", border = F)
hist(mortgage$first_time, main = "First Observation", col = "blue", border = F)
hist(mortgage$mat_time, main = "Maturity Timestamp", col = "blue", border = F)
hist(mortgage$balance_time, main = "Outstanding Bal at Observation Time", col = "blue", border = F)
hist(mortgage$LTV_time, main = "LTV Ratio at Observation Time", col = "blue", border = F)
hist(mortgage$interest_rate_time, main = "Interest Rate at Observation Time", col = "blue", border = F)
hist(mortgage$hpi_time, main = "HPI at Observation Time", col = "blue", border = F)
hist(mortgage$gdp_time, main = "GDP Growth at Observation Time", col = "blue", border = F)
hist(mortgage$uer_time, main = "Unemploy Rate at Observation Time", col = "blue", border = F)
hist(mortgage$balance_orig_time, main = "Outstanding Bal at Origination Time", col = "blue", border = F)
hist(mortgage$FICO_orig_time, main = "FICO at Origination Time", col = "blue", border = F)
hist(mortgage$LTV_orig_time, main = "LTV Ratio at Origination Time", col = "blue", border = F)
hist(mortgage$Interest_Rate_orig_time, main = "Interest Rate at Origination Time", col = "blue", border = F)
hist(mortgage$hpi_orig_time, main = "HPI at Origination Time", col = "blue", border = F)


  # Bar Graph
par(mfrow = c(2, 4))  # Setting template for bar graph

  # setting all these variables into categorical
mortgage$REtype_CO_orig_time <- as.factor(mortgage$REtype_CO_orig_time)
mortgage$REtype_PU_orig_time <- as.factor(mortgage$REtype_PU_orig_time)
mortgage$REtype_SF_orig_time <- as.factor(mortgage$REtype_SF_orig_time)
mortgage$investor_orig_time <- as.factor(mortgage$investor_orig_time)
mortgage$default_time <- as.factor(mortgage$default_time)
mortgage$payoff_time <- as.factor(mortgage$payoff_time)
mortgage$status_time <- as.factor(mortgage$status_time)

plot(mortgage$REtype_CO_orig_time, main = "RealEstate Type at Origination Time",col = c("red", "blue"))
plot(mortgage$REtype_CO_orig_time, main = "RealEstate Type at Origination Time",col = c("red", "blue"))
plot(mortgage$REtype_PU_orig_time, main = "RealEstate Type Planned at Orig Time",col = c("red", "blue"))
plot(mortgage$REtype_SF_orig_time, main = "Single Family Home",col = c("red", "blue"))
plot(mortgage$investor_orig_time, main = "Investor Borrower",col = c("red", "blue"))
plot(mortgage$default_time, main = "Default Observation",col = c("red", "blue"))
plot(mortgage$payoff_time, main = "Payoff Observation",col = c("red", "blue"))
plot(mortgage$status_time, main = "Observation Status",col = c("red", "blue", "green"))


## Analyzing pattern for default, payoff and neither ##

## Default Observation
m1 <- mortgage
m1 <- m1 %>%
  filter(status_time == 1)

par(mfrow = c(2, 9))

# Origination time
hist(m1$orig_time, main = "Origination Time")    
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- originated in time stamp around 25 to 30


# Time stamp at first observation
hist(m1$first_time, main = "Time stamp_First Obs")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- first observed within the time stamp between 20 and 30


# Maturity timestamp
hist(m1$mat_time, main = "Maturity Time_Def")     
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- matured around time stamp of 150


# Outstanding Balance at Default Time
summary(m1$balance_time)
plot(m1$balance_time, ylim = c(0, 2000000), xlab = "Balance",
     main = "Outstanding Bal_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of below $500,000

# LTV ratio at Default Time
hist(m1$LTV_time, main = "LTV ratio_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 80 to 120 percent of LTV ratio

# Interest rate at Default Time
hist(m1$interest_rate_time, main = "Interest Rate_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them  
# --- fall within the range of 6 to 9 percent


# HPI 
hist(m1$hpi_time, main = "HPI_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 150 to 170 HPI


# GDP
hist(m1$gdp_time, main = "GDP_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 1 to 3 GDP, with some significant numbers also spotted
# --- around the range of somewhat lower at around -4 to -2.5 GDP


# Umemployment rate
hist(m1$uer_time, main = "Unempl Rate_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of 4.5 to 5.5, with also some signigicant numbers around 8.5 to 10 percent


# Real Estate Type
m1$REtype_CO_orig_time <- as.factor(m1$REtype_CO_orig_time)

plot(m1$REtype_CO_orig_time, main = "Real Estate Type", col = c("red", "blue"))
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- not condominiums


# Real Estate Type Planned
m1$REtype_PU_orig_time <- as.factor(m1$REtype_PU_orig_time)

plot(m1$REtype_PU_orig_time, main = "Real Estate Type Plan", col = c("red", "blue"))
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- not for urban development


# Investor borrower
m1$REtype_SF_orig_time <- as.factor(m1$REtype_SF_orig_time)

plot(m1$REtype_SF_orig_time, main = "Investor borrower", col = c("red", "blue"))
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- investor borrower


# Outstanding Balance at Origination Time
plot(m1$balance_orig_time, ylim = c(0, 1500000), xlab = "Balance",
     main = "Outstanding Bal_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of below $400,000


# FICO at Origination Time
plot(m1$FICO_orig_time, ylim = c(500, 800), xlab = "Balance",
     main = "FICO score_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of around 600 to 700


# LTV at Origination Time
plot(m1$LTV_orig_time, xlab = "Balance",
     main = "LTV ratio_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of around 70 to 80


# Interest rate at Origination Time
plot(m1$Interest_Rate_orig_time, xlab = "Balance",
     main = "Interest Rate_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of around 5 to 10


# HPI at Origination Time
hist(m1$hpi_orig_time, main = "HPI_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 210 to 220 HPI


# ------------------------------*-------------------------- # 

## Payoff Observation
m2 <- mortgage
m2 <- m2 %>%
  filter(status_time == 2)

par(mfrow = c(2, 9))

# Origination time
hist(m2$orig_time, main = "Origination time")    
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- originated in time stamp around 20


# Time stamp at first observation
hist(m2$first_time, main = "Time stamp_First Obs")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- first observed within the time stamp between 20 and 30


# Maturity timestamp
hist(m2$mat_time, main = "Maturity Time_Def")     
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- matured around time stamp of around 140 to 150


# Outstanding Balance at Default Time
summary(m2$balance_time)
plot(m2$balance_time, ylim = c(0, 500000), xlab = "Balance",
     main = "Outstanding Bal_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of below $500,000 (100,000 to 200,000)


# LTV ratio at Default Time
hist(m2$LTV_time, main = "LTV ratio_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 60 to 80 percent of LTV ratio


# Interest rate at Default Time
hist(m2$interest_rate_time, main = "Interest Rate_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them  
# --- fall within the range of 4 to 8 percent


# HPI 
hist(m2$hpi_time, main = "HPI_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 230 to 250 HPI


# GDP
hist(m2$gdp_time, main = "GDP_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 2 to 3.5 GDP


# Umemployment rate
hist(m2$uer_time, main = "Unempl Rate_Def")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of 4.5 to 5


# Real Estate Type
m2$REtype_CO_orig_time <- as.factor(m2$REtype_CO_orig_time)

plot(m2$REtype_CO_orig_time, main = "Real Estate Type", col = c("red", "blue"))
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- not condominiums


# Real Estate Type Planned
m2$REtype_PU_orig_time <- as.factor(m2$REtype_PU_orig_time)

plot(m2$REtype_PU_orig_time, main = "Real Estate Type Planned", col = c("red", "blue"))
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- not for urban development


# Investor borrower
m2$REtype_SF_orig_time <- as.factor(m2$REtype_SF_orig_time)

plot(m2$REtype_SF_orig_time, main = "Investor borrower", col = c("red", "blue"))
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- investor borrower


# Outstanding Balance at Origination Time
plot(m2$balance_orig_time, ylim = c(0, 500000), xlab = "Balance",
     main = "Outstanding Bal_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of below $400,000


# FICO at Origination Time
plot(m2$FICO_orig_time, xlab = "Balance",
     main = "FICO score_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of around 650 to 700


# LTV at Origination Time
plot(m2$LTV_orig_time, ylim = c(50, 100), xlab = "Balance",
     main = "LTV ratio_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of around 70 to 80


# Interest rate at Origination Time
plot(m2$Interest_Rate_orig_time, ylim = c(0, 15), xlab = "Balance",
     main = "Interest Rate_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them are 
# --- are within the range of around 6 to 8


# HPI at Origination Time
hist(m2$hpi_orig_time, main = "HPI_Original")
# --- among the borrower whose loan has been defaulted, it can be seen that most of them 
# --- fall within the range of around 210 to 230 HPI

###################################################*################################################


### STEP 4 - Balancing the data  ###

mortgage_unbalanced <- rbind(mortgage_def, mortgage_payoff)
mortgage_unbalanced$status_time <- as.factor(mortgage_unbalanced$status_time)

mortgage_balanced <- ovun.sample(status_time ~., data = mortgage_unbalanced, method = "both", seed = 12345)$data
kable(mortgage_balanced %>%
  count(status_time))

###################################################*################################################



## STEP 5 - Dimension Reduction ## 

  # 1. Logistic Regression
str(mortgage_balanced)

## Converting to correct data format for each variable
mortgage_balanced$REtype_CO_orig_time <- as.factor(mortgage_balanced$REtype_CO_orig_time)
mortgage_balanced$REtype_PU_orig_time <- as.factor(mortgage_balanced$REtype_PU_orig_time)
mortgage_balanced$REtype_SF_orig_time <- as.factor(mortgage_balanced$REtype_SF_orig_time)
mortgage_balanced$investor_orig_time <- as.factor(mortgage_balanced$investor_orig_time)
mortgage_balanced$status_time <- as.factor(mortgage_balanced$status_time)


## Reshuffling the data since it is ordered
set.seed(12345)
rows <- sample(nrow(mortgage_balanced))
mortgage_balanced <- mortgage_balanced[rows,]


# Run the glm model
log_dimred <- glm(status_time ~., data = mortgage_balanced, family = "binomial")
summary(log_dimred)

  # 2. Correlation Matrix
# Converting the variables as numeric to plot for correlation
mort_ <- sapply(mortgage_unbalanced, as.numeric)
str(mort_)

mort.cor <- cor(mort_)
corrplot(mort.cor)


  # 3. Heatmap
heatmap(x = mort.cor)

## Removing the unwanted/ insignificant variables
  # --- remove the REtype_CO_Orig_time, REtype_PU_Orig_Time and Retype_SF_Orig_Time
mortgage_unbalanced <- subset(mortgage_unbalanced, select = -c(12, 13, 14))

###################################################*################################################

## STEP 5 - Data Partitioning ## 

set.seed(12345)

mort_index <- createDataPartition(mortgage_unbalanced$status_time, p = 0.7, list = FALSE)
mortgage_train_imbalanced <- mortgage_unbalanced[mort_index,]   # training data

mortgage_test <- mortgage_unbalanced[-mort_index,]   # testing data


kable(mortgage_train_imbalanced %>%
        count(status_time))        # distribution for imbalanced train data

kable(mortgage_test %>%
        count(status_time))         # distribution for test data

###################################################*################################################

## STEP 6 - Different Data Allocation

## 1. Original uncleaned data 
## mortgage

## 2. Balanced original data
## mortgage_balanced

## 3. Imbalanced train data
## mortgage_train_imbalanced

## 4. Test data
## mortgage_test

## 5. Balanced train data
mortgage_train_balanced <- ovun.sample(status_time ~., data = mortgage_unbalanced, method = "both", seed = 12345)$data

kable(mortgage_train_balanced %>%
        count(status_time))        # distribution for balanced train data

###################################################*################################################


### STEP 7 - Modelling with Original Imbalanced Data ###


## Model 1: K-Nearest Neighbor ##

## Converting variables to numeric
  # ---- upon checking the string of the data, it seems like all variables are fitted for knn modeling except for the id

# Train data
set.seed(12345)
knn_train_o <- mortgage_train_imbalanced
knn_train_o <- subset(knn_train_o, select = -c(1))
str(knn_train_o)

# Test data
knn_test_o <- mortgage_test
knn_test_o <- subset(knn_test_o, select = -c(1))
str(knn_test_o)

# Checking on the proportion of two classes again
knn_train_o %>%
  count(status_time)   # test data class proportion

knn_test_o %>%
  count(status_time)   # test data class proportion


  # altering the target variable to factor
knn_train_o$status_time <- factor(knn_train_o$status_time, levels = c(1,2), ordered = TRUE)
knn_test_o$status_time <- factor(knn_test_o$status_time, levels = c(1,2), ordered = TRUE)

## Normalizing the data

# Function to normalise
normalise<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

## Apply the function to train and test data
o_knn_train_n<-as.data.frame(lapply(knn_train_o[,-21], normalise))
head(o_knn_train_n)

o_knn_test_n<-as.data.frame(lapply(knn_test_o[,-21], normalise))
head(o_knn_test_n)

  ## Changing the target Variable - since it becomes NA after normalising
o_knn_train_n$status_time <- mortgage_train_imbalanced$status_time
o_knn_train_n$status_time <- as.factor(o_knn_train_n$status_time)

o_knn_test_n$status_time <- mortgage_test$status_time
o_knn_test_n$status_time <- as.factor(o_knn_test_n$status_time)

str(o_knn_train_n)
str(o_knn_test_n)

## Train the model
control <- trainControl(method = "repeatedcv", number = 5)
set.seed(12345)
o_knn_mod <- train(status_time ~., data = o_knn_train_n, method = "knn", 
                   trControl = control, tuneLength = 10)

o_knn_mod

## Evaluating performance on the training data using the best k tuning suggested - k = 23
o_knn_pred_23_train <- predict(o_knn_mod, newdata = o_knn_train_n)
confusionMatrix(o_knn_pred_23_train, o_knn_train_n$status_time, positive = "1")

## Evaluating performance using the best k tuning suggested - k = 23
o_knn_pred_23 <- predict(o_knn_mod, newdata = o_knn_test_n)
confusionMatrix(o_knn_pred_23, o_knn_test_n$status_time, positive = "1")




## Model 2 : Classification Tree ##

## Train data
tree_train_o <- mortgage_train_imbalanced
str(tree_train_o)

  # Converting variable to categorical
tree_train_o$investor_orig_time <- as.factor(tree_train_o$investor_orig_time)

## Test data
tree_test_o <- mortgage_test
str(tree_train_o)

  # Converting variable to categorical
tree_test_o$investor_orig_time <- as.factor(tree_test_o$investor_orig_time)


## Train the model
set.seed(12345)

tree_mod_o <- rpart(status_time ~., data = tree_train_o, 
      method = "class")

## Plot the tree
par(mfrow= c(1,1))
summary(tree_mod_o)
rpart.plot(tree_mod_o)


## Performance on training data
o_tree_pred <- predict(tree_mod_o, tree_train_o, type = 'class')
confusionMatrix(o_tree_pred, tree_train_o$status_time, positive = "1")


## Performance on testing data
o_tree_pred1 <- predict(tree_mod_o, tree_test_o, type = 'class')
confusionMatrix(o_tree_pred1, tree_test_o$status_time, positive = "1")

## Best pruned tree
printcp(tree_mod_o)
plotcp(tree_mod_o)
min.cp_o <- tree_mod_o$cptable[which.min(tree_mod_o$cptable[,"xerror"]), "CP"]
min.cp_o
best.pruned_tree_o <- prune(tree_mod_o, cp = min.cp_o)
rpart.plot(best.pruned_tree_o)

  #-- Apparently, the best pruned tree is the same tree as the initial tree -- #



## Model 3 : Logistic Regression ##

## Train data

log_train_o <- mortgage_train_imbalanced
str(log_train_o)

## Test data
log_test_o <- mortgage_test
str(log_test_o)

## Converting categorical variables 
log_train_o$investor_orig_time <- as.factor(log_train_o$investor_orig_time)
log_test_o$investor_orig_time <- as.factor(log_test_o$investor_orig_time)

str(log_train_o)
str(log_test_o)

## Fit the logistic regression model
logmod_o <- glm(status_time ~., data = log_train_o, family = binomial)
options(scipen = 999)
summary(logmod_o)


## Test on the training data itself
log_pred_oo <- predict(logmod_o, log_train_o, type = "response")

# set the cutoff value as 0.5
log_pred_oo_result <- ifelse(log_pred_oo > 0.5, 2, 1)
str(log_pred_oo_result)
log_pred_oo_result
log_pred_oo_result <- as.factor(log_pred_oo_result)
confusionMatrix(log_pred_oo_result, log_train_o$status_time, positive = "1")


## Test on the testing data 
log_pred_o <- predict(logmod_o, log_test_o, type = "response")

# set the cutoff value as 0.5
log_pred_o_result <- ifelse(log_pred_o > 0.5, 2, 1)
str(log_pred_o_result)
log_pred_o_result
log_pred_o_result <- as.factor(log_pred_o_result)
confusionMatrix(log_pred_o_result, log_test_o$status_time, positive = "1")

###########################*#############################


### STEP 8 - Modelling with Balanced Data ###

## Model 1: K-Nearest Neighbor ##

## Converting variables to numeric
# ---- upon checking the string of the data, it seems like all variables are fitted for knn modeling except for the id

# Train data
set.seed(12345)
knn_train_b <- mortgage_train_balanced
knn_train_b <- subset(knn_train_b, select = -c(1))
str(knn_train_b)

# Test data
knn_test_b <- mortgage_test
knn_test_b <- subset(knn_test_b, select = -c(1))
str(knn_test_b)

# Checking on the proportion of two classes again
knn_train_b %>%
  count(status_time)   # test data class proportion

knn_test_b %>%
  count(status_time)   # test data class proportion


# altering the target variable to factor
knn_train_b$status_time <- factor(knn_train_b$status_time, levels = c(1,2), ordered = TRUE)
knn_test_b$status_time <- factor(knn_test_b$status_time, levels = c(1,2), ordered = TRUE)

## Normalizing the data

# Function to normalise
normalise<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

## Apply the function to train and test data
b_knn_train_n<-as.data.frame(lapply(knn_train_b[,-21], normalise))
head(b_knn_train_n)

b_knn_test_n<-as.data.frame(lapply(knn_test_b[,-21], normalise))
head(b_knn_test_n)

## Changing the target Variable - since it becomes NA after normalising
b_knn_train_n$status_time <- mortgage_train_balanced$status_time
b_knn_train_n$status_time <- as.factor(b_knn_train_n$status_time)

b_knn_test_n$status_time <- mortgage_test$status_time
b_knn_test_n$status_time <- as.factor(b_knn_test_n$status_time)

str(b_knn_train_n)
str(b_knn_test_n)

## Train the model
control <- trainControl(method = "repeatedcv", number = 5)
set.seed(12345)
b_knn_mod <- train(status_time ~., data = b_knn_train_n, method = "knn", 
                   trControl = control, tuneLength = 10)
             
             # * takes around 4 minutes to run * #
b_knn_mod

## Evaluating performance on the training data using the best k tuning suggested - k = 9
b_knn_pred_train <- predict(b_knn_mod, newdata = b_knn_train_n)
confusionMatrix(b_knn_pred_train, b_knn_train_n$status_time, positive = "1")


## Evaluating performance using the best k tuning suggested - k = 9
b_knn_pred_9 <- predict(b_knn_mod, newdata = b_knn_test_n)
confusionMatrix(b_knn_pred_9, b_knn_test_n$status_time, positive = "1")




## Model 2 : Classification Tree ##

## Train data
tree_train_b <- mortgage_train_balanced
str(tree_train_b)

# Converting variable to categorical
tree_train_b$investor_orig_time <- as.factor(tree_train_b$investor_orig_time)

## Test data
tree_test_b <- mortgage_test
str(tree_train_b)

# Converting variable to categorical
tree_test_b$investor_orig_time <- as.factor(tree_test_b$investor_orig_time)


## Train the model
set.seed(12345)

tree_mod_b <- rpart(status_time ~., data = tree_train_b, 
                    method = "class")

## Plot the tree
par(mfrow= c(1,1))
summary(tree_mod_b)
rpart.plot(tree_mod_b)


## Performance on training data
b_tree_pred <- predict(tree_mod_b, tree_train_b, type = 'class')
confusionMatrix(b_tree_pred, tree_train_b$status_time, positive = "1")

## Performance on testing data
b_tree_pred1 <- predict(tree_mod_b, tree_test_b, type = 'class')
confusionMatrix(b_tree_pred1, tree_test_b$status_time, positive = "1")

## Best pruned tree
printcp(tree_mod_b)
plotcp(tree_mod_b)
min.cp_b <- tree_mod_b$cptable[which.min(tree_mod_b$cptable[,"xerror"]), "CP"]
min.cp_b
best.pruned_tree_b <- prune(tree_mod_b, cp = min.cp_b)
rpart.plot(best.pruned_tree_b)

#-- Apparently, the best pruned tree is the same tree as the initial tree again-- #



## Model 3 : Logistic Regression ##

## Train data

log_train_b <- mortgage_train_balanced
str(log_train_b)

## Test data
log_test_b <- mortgage_test
str(log_test_b)

## Converting categorical variables 
log_train_b$investor_orig_time <- as.factor(log_train_b$investor_orig_time)
log_test_b$investor_orig_time <- as.factor(log_test_b$investor_orig_time)

str(log_train_b)
str(log_test_b)

## Fit the logistic regression model
logmod_b <- glm(status_time ~., data = log_train_b, family = binomial)
options(scipen = 999)
summary(logmod_b)


## Test on the training data itself
log_pred_bb <- predict(logmod_b, log_train_b, type = "response")

# set the cutoff value as 0.5
log_pred_bb_result <- ifelse(log_pred_bb > 0.5, 2, 1)
str(log_pred_bb_result)
log_pred_bb_result
log_pred_bb_result <- as.factor(log_pred_bb_result)
confusionMatrix(log_pred_bb_result, log_train_b$status_time, positive = "1")

## Test on the testing data itself
log_pred_b <- predict(logmod_b, log_test_b, type = "response")

# set the cutoff value as 0.5
log_pred_b_result <- ifelse(log_pred_b > 0.5, 2, 1)
str(log_pred_b_result)
log_pred_b_result
log_pred_b_result <- as.factor(log_pred_b_result)
confusionMatrix(log_pred_b_result, log_test_b$status_time, positive = "1")


###########################*#############################

## STEP 9 - Testing on random data from original data using selected better performing models

  # --- Model 2 (Balanced KNN model), Model 4 (Balanced Tree Model) and Model 5 (Unbalanced Logistic Regression Model)

  # Creating the randomized data (one of the known borrower observations before the declared observations) from original data

rand <- mortgage_new %>%
  distinct(id, .keep_all = TRUE)    # brings the first observations for each ID



rand <- anti_join(rand, unknown_obs, by = c("id"))    # brings the first observation for each ID with known result
                                                # either 1 or 2

rand$status_time <- as.factor(rand$status_time)
str(rand)


  # creating the result data from the original data
mortgage_results <- rbind(mortgage_def, mortgage_payoff)

    #--- since it is ordered with the defaulted observations then the payoff observations
    #--- it has to be ordered with id number to match with randomized data to test the results
mortgage_results <- mortgage_results %>%
  distinct(id, .keep_all = TRUE)

mortgage_results <- mortgage_results %>%
  arrange(id)

mortgage_results$status_time <- as.factor(mortgage_results$status_time)
str(mortgage_results)

## Testing the model with the data

    # 1. Model 2 KNN

knn_rand <- subset(rand, select = -c(1, 12, 13, 14))

normalise<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

    # Apply the function to the random data
b_knn_rand<-as.data.frame(lapply(knn_rand[,-17], normalise))
head(b_knn_rand)

    # Changing the target Variable
b_knn_rand$status_time <- factor(knn_rand$status_time, levels = c(1, 2))
str(b_knn_rand)

    # Test the model with the data
b_knn_pred_rand <- predict(b_knn_mod, newdata = b_knn_rand)
b_knn_pred_rand <- as.factor(b_knn_pred_rand)
confusionMatrix(b_knn_pred_rand, mortgage_results$status_time, positive = "1")


    # 2. Model 4 Classification Tree
tree_rand <- rand
tree_rand <- subset(tree_rand, select = -c(12, 13, 14))

tree_rand$investor_orig_time <- as.factor(tree_rand$investor_orig_time)
str(tree_rand)

b_tree_pred_rand <- predict(best.pruned_tree_b, tree_rand, type = 'class')
confusionMatrix(b_tree_pred_rand, mortgage_results$status_time, positive = "1")



    # 3. Model 5 Logistic Regression
log_rand <- tree_rand


## Test on the testing data itself
log_pred_rand <- predict(logmod_o, log_rand, type = "response")

# set the cutoff value as 0.5
log_pred_rand_result <- ifelse(log_pred_rand > 0.5, 2, 1)
str(log_pred_rand_result)
log_pred_rand_result <- as.factor(log_pred_rand_result)
confusionMatrix(log_pred_rand_result, mortgage_results$status_time, positive = "1")

###########################*#############################


### STEP 10 - Performance Evaluation (For the models selected to test the testing data and the first observations of the known IDs outcome) ###
# ------------------ ROC CURVE -------------------------

## Models in final selection
# KNN : b_knn_mod
# Classification tree : best.pruned_tree_b
# Logistic Regression : logmod_o

## ROC for KNN model - Testing data
library(pROC)
roc_knn_test <- roc(as.numeric(b_knn_test_n$status_time), as.numeric(b_knn_pred_9), 
               percent = TRUE)
plot(roc_knn_test, print.auc = TRUE, col = "red", main = "ROC Curve for KNN Model_Testing Data")


## ROC for KNN model - Random data
roc_knn_rand <- roc(as.numeric(mortgage_results$status_time), as.numeric(b_knn_pred_rand), 
               percent = TRUE)
plot(roc_knn_rand, print.auc = TRUE, col = "red", main = "ROC Curve for KNN Model_Original Data (1st obs)")




## ROC for Classification Tree - Testing data
b_tree_pred1
roc_tree_test <- roc(as.numeric(tree_test_b$status_time), as.numeric(b_tree_pred1), 
                percent = TRUE)
plot(roc_tree_test, print.auc = TRUE, col = "red", main = "ROC Curve for Classification Tree_Testing Data")


## ROC for Classification Tree - Random data
b_tree_pred_rand
roc_tree_rand <- roc(as.numeric(mortgage_results$status_time), as.numeric(b_tree_pred_rand), 
                     percent = TRUE)
plot(roc_tree_rand, print.auc = TRUE, col = "red", main = "ROC Curve for Classification Tree_Original Data (1st obs)")




## ROC for Logistic Regression - Testing Data

roc_log_test <- roc(as.numeric(log_test_o$status_time), as.numeric(log_pred_o_result), 
               percent = TRUE)
plot(roc_log_test, print.auc = TRUE, col = "red", main = "ROC Curve for Logistic Regression_Testing Data")


## ROC for Logistic Regression - Random Data

roc_log_rand <- roc(as.numeric(mortgage_results$status_time), as.numeric(log_pred_rand_result), 
                    percent = TRUE)
plot(roc_log_rand, print.auc = TRUE, col = "red", main = "ROC Curve for Logistic Regression_Original Data (1st obs)")

###########################*#############################

### STEP 11 - Predicting the outcomes for unknown data with Model 5 ###

## Model 5 Logistic Regression - logmod_o

## Unknown data - unknown_obs

## Having the same alterations to use on the model

  # Removing insignificant columns
unknown_obs1 <- subset(unknown_obs, select = -c(12, 13, 14))

  # Keeping only the first observations for each borrower ID
unknown_obs_1 <- unknown_obs1 %>%
  distinct(id, .keep_all = TRUE)
unknown_obs_1 %>%
  count(id)

  # converting to categorical variables
unknown_obs_1$investor_orig_time <- as.factor(unknown_obs_1$investor_orig_time)
unknown_obs_1$status_time <- as.factor(unknown_obs_1$status_time)
str(unknown_obs_1)

## Test the model on the unknown data
log_pred_unknown <- predict(logmod_o, unknown_obs_1, type = "response")

  # set the cutoff value as 0.5
log_pred_unknown_result <- ifelse(log_pred_unknown > 0.5, 2, 1)
unknown_obs_1$pred_result <- log_pred_unknown_result
unknown_obs_1 %>%
  count(pred_result)

barplot(prop.table(table(unknown_obs_1$pred_result)), 
        col = c("red", "blue"),
        main = "Predicted Result Status Distribution_Logistic Regression")

legend("topright", legend = c("Defaulted", "Payoff"), 
       col = c("red", "blue"), 
       bty = "n", pch = 20, pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))

unknown_obs_1 %>%
  count(pred_result)

pred <- predict(tree_mod_b, unknown_obs_1[,-20], type = "class")
pred

barplot(prop.table(table(pred)), 
        col = c("blue", "red"),
        main = "Predicted Result Status Distribution_Classification Tree")

legend("topright", legend = c("Payoff", "Defaulted"), 
       col = c("blue", "red"), 
       bty = "n", pch = 20, pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))

p <- as.data.frame(pred)
p %>% 
  count(pred)

