##################################### TAXI CANCELLATION CASE STUDY #######################################

* ### Variables Description ### *
  # Row ID: This is the unique identifier of this dataset. It is a number identifying each record. 
  
  # User ID: This is the identifier of each customer. There are many duplicates in this subset, meaning there are any customers who have called multiple rides. 
  
  # Vehicle Model ID: This is an ID that represents the type of vehicle driven for each ride. 
  
  # Travel Type ID: This is an ID that represents the type of travel (1= long distance, 2= point to point, 3= hourly rental). 
  
  # Package ID: This is an ID that represents the type of travel package, with the following descriptions: 
  # 1=4hrs & 40kms, 2=8hrs & 80kms, 3=6hrs & 60kms, 4= 10hrs & 100kms, 5=5hrs & 50kms, 6=3hrs & 30kms, 7=12hrs & 120kms. 
  
# From Area: This is an identifier of the starting area. Available only for point-to-point travel. 

# To Area: This is an identifier of the ending area. Available only for point-to-point travel. 

# From City ID: Unique identifier of the starting city. 

# To City ID: Unique identifier of the ending city. 

# From Date: Date and time of the requested trip start. 

# To Date: Time stamp of trip end. 

# Online Booking: A binary (0,1) variable representing whether the booking was made online or not. 0 represents no, 1 represents yes. 

# Mobile Site Booking: A binary (0,1) variable representing whether the booking was made on their mobile site or not. 0 represents no, 1 represents yes. 

# Booking Created: Date and time of booking created.

# From Lat: The latitude of the start area. 

# From Long: The longitude of the start area. 

# To Lat: The latitude of the end area. 

# To Long: The longitude of the end area. 

# Car Cancellation: The target variable. A binary (0,1) variable representing whether or not the ride was cancelled. 0 means no, 1 means yes. 


### STEP 1 - Data Exploration ###

## Loading the required packages
library(dplyr)  # package for data wrangling
library(caret)  # package for confusion matrix
library(ggplot2)  # package for data visualization
library(corrplot)  # package for correlation matrix
library(tibble)  # package for simple data frames
library(visdat)  # package for whole data visualisation
library(naniar)  # package for visualisation missing data
library(stringr)  # package for splitting strings
library(tidyr)  # package for dealing with missing data
library(imbalance)   # package for dealing with imbalance dataset
library(ROSE)     # package for dealing with imbalance dataset
library(class)    # package for knn model
library(rpart)    # package for classification tree
library(rpart.plot)   # package for classification tree plot
library(e1071)    # package for tuning
library(neuralnet)   # package for neural net model
library(dummies)    # package for dummy variables
library(knitr)    # package for table

## Load the data
setwd("C:/Users/khint/Documents/Course Materials/Graduate (St. Louis)/2022/Fall Semester_2022/CSDA 6010 Analytic Practicum/Project 1- Taxi Cancellation")
taxi <- read.csv("Taxi-cancellation-case.csv")


## Explore the general overview of the data
str(taxi)
summary(taxi)


## Checking for missing values
sum(is.na(taxi))   # sum of missing values
which(is.na(taxi))   # exact location of all the missing values
vis_miss(taxi, sort_miss = TRUE)   # visualization of missing value
miss_var_summary(taxi)   # summary of where missing values are located


## Checking for null values
is.null(taxi)  # checking null values for the whole dataset
    # Checking null values for individual
is.null(taxi$row.)
is.null(taxi$user_id)
is.null(taxi$vehicle_model_id)
is.null(taxi$package_id)
is.null(taxi$from_area_id)
is.null(taxi$to_area_id)
is.null(taxi$from_city_id)
is.null(taxi$to_city_id)
is.null(taxi$from_date)
is.null(taxi$to_date)
is.null(taxi$online_booking)
is.null(taxi$mobile_site_booking)
is.null(taxi$booking_created)
is.null(taxi$from_lat)
is.null(taxi$from_long)
is.null(taxi$to_lat)
is.null(taxi$to_long)

###########################*#############################


### STEP 2 - Attribute Manipulation  ###

## Removing the totally irrelevant columns - Row and city IDs
taxi_data <- subset(taxi, select = -c(1,8,9))
summary(taxi_data)

  # Removing to_date columns with too much missing values and the entered values are mostly similar
taxi_data <- subset(taxi_data, select = -c(8))
str(taxi_data)

## Altering the date columns

  # Booking_created
taxi_data$booking_created <- strptime(taxi_data$booking_created, format = "%m/%d/%Y %H:%M")
class(taxi_data$booking_created)

taxi_data$Day_Of_Week_booking_created <- weekdays(taxi_data$booking_created)  # creating another column to identify the day of the ride has taken place
taxi_data$Day_Of_Week_booking_created <- factor(taxi_data$Day_Of_Week_booking_created, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

      # Seperating booking created into day and minutes
taxi_data$booking_created_date <- as.Date(taxi_data$booking_created)
taxi_data$booking_created_time <- format(as.POSIXct(taxi_data$booking_created), 
                                         format = "%H:%M:%S")


    # From_Date
taxi_data$from_date <- strptime(taxi_data$from_date, format = "%m/%d/%Y %H:%M")
class(taxi_data$from_date)

taxi_data$Day_Of_Week_FromDate <- weekdays(taxi_data$from_date)  # creating another column to identify the day of the ride has taken place
taxi_data$Day_Of_Week_FromDate <- factor(taxi_data$Day_Of_Week_FromDate, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


      # Seperating from_date into day and minutes
taxi_data$from_date_date <- as.Date(taxi_data$from_date)
taxi_data$from_date_time <- format(as.POSIXct(taxi_data$from_date), 
                                   format = "%H:%M:%S")


## Creating another column for gap time between the time taxi is booked and the actual travel starting time
taxi_data$gap_time <- as.numeric(difftime(taxi_data$from_date, taxi_data$booking_created, units = "hours"))


## Creating another column for another booking method - Call In Booking
taxi_data$Call_In_Booking <- ifelse(taxi_data$online_booking == 0 & taxi_data$mobile_site_booking == 0, 1,0)


## Creating another column for travel package
taxi_data$travel_package <- ifelse(is.na(taxi_data$package_id), 0, 1)


## Dealing with missing data
miss_var_summary(taxi_data)   # summary of where missing values are located

  # Removing observations with all NA values in the latitude and longitude columns
taxi_data <- taxi_data %>%
  drop_na(c("from_lat", "from_long"))

  # Replace the NA values of to_area_id with their corresponding from_area_id
# *assuming that missing values is resulted from being in the same area_id all along the trip* #

taxi_data <- taxi_data %>%
  mutate(to_area_id = coalesce(to_area_id, from_area_id))


## Creating another column for distance
#_________________ compute trip distance from GPS data __________________________________________________________
# Create the distance calculation function. This function gets: 
# the latitude and longitudes of two points on earth
# and returns the distance in kilometers

sdist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad 
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145     #earth radius 
  d <- R * c
  return(d)
}

# use the function to calculate all distances and add them to the dataset
taxi_data$trip_distance<- sdist(taxi_data$from_long, taxi_data$from_lat, taxi_data$to_long, taxi_data$to_lat)

# replace those distances with NA values of the to lat and to long with 0
# *assuming that missing values is resulted from being in the same coordinates all along the trip* #

taxi_data$trip_distance[is.na(taxi_data$trip_distance)] <- 0.00

   
## Removing the redundant variables (those which separate columns have been created)
str(taxi_data)
taxi_data$from_date <- NULL 
taxi_data$booking_created <- NULL
taxi_data$package_id <- NULL

  # Longitude and latitude variables are to be removed after calculating the distance
taxi_data$from_lat <- NULL
taxi_data$from_long <- NULL
taxi_data$to_lat <- NULL
taxi_data$to_long <- NULL

sum(is.na(taxi_data))  # to ensure that there is no missing values before balancing the data

###########################*#############################


### STEP 3 - General Attribute Analysis Against the Target Variable: Car Cancellation ###


## Class distribution
taxi_data %>%
  count(Car_Cancellation)   # count of cancelled/ not cancelled

ggplot(taxi_data, aes(x = as.factor(Car_Cancellation), fill = as.factor(Car_Cancellation))) + 
  geom_bar() + 
  labs(x = "Car Cancellation", 
       y = "Number of Cancellation", 
       title = "Car Cancellation Distribution")+ 
  ggeasy::easy_center_title() + 
  labs(fill = "Car_Cancellation") + 
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 0, color = "black", 
            position = position_dodge(width = 1)) + 
  scale_fill_discrete(breaks = c("0", "1"), 
                    labels = c("Not Cancelled", "Cancelled"))


## Histogram and Bar Graph for variables 
  ##### date and time variable excluded since they are not numeric #####
str(taxi_data)
a <- table(taxi_data$Day_Of_Week_booking_created)
b <- table(taxi_data$Day_Of_Week_FromDate)

par(mfrow = c(2, 7))   # setting template for Histogram/Bar Graph

hist(taxi_data$user_id, main = "User_ID", 
     col = "darkgreen")
hist(taxi_data$vehicle_model_id, main = "Vehicle_Model_ID", 
     col = "darkgreen")
hist(taxi_data$travel_type_id, main = "Travel_Type_ID", 
     col = "darkgreen")
hist(taxi_data$from_area_id, main = "From_Area_ID", 
     col = "darkgreen")
hist(taxi_data$to_area_id, main = "To_Area_ID", 
     col = "darkgreen")
hist(taxi_data$online_booking, main = "Online_Booking", 
     col = "darkgreen")
hist(taxi_data$mobile_site_booking, main = "Mobile_Site_Booking", 
     col = "darkgreen")
hist(taxi_data$Call_In_Booking, main = "Call_In_Booking", 
     col = "darkgreen")
barplot(a, main = "Booking_Created (Day)", col = "darkgreen")
barplot(b, main = "Trip (Day)", col = "darkgreen")
hist(taxi_data$gap_time, main = "Time_Diff (Booking & Trip)", 
     col = "darkgreen")
hist(taxi_data$travel_package, main = "Travel_Package", 
     col = "darkgreen")
hist(taxi_data$trip_distance, main = "Trip_Distance", 
     col = "darkgreen")

## Box plot for numeric variables

par(mfrow = c(2, 3))   # setting template for box plot

boxplot(taxi_data$user_id, main = "User_ID")
boxplot(taxi_data$vehicle_model_id, main = "Vehicle_Model")
boxplot(taxi_data$from_area_id, main = "From_Area_ID")
boxplot(taxi_data$to_area_id, main = "To_Area_ID")
boxplot(taxi_data$gap_time, main = "Time_Diff (Booking & Trip)" )
boxplot(taxi_data$trip_distance, main = "Trip_Distance")


## Variables Against the Cancellation ##
## Cancellation rate by user_id
user <- taxi_data %>%
  group_by(user_id) %>%
  summarise(Cancellation = Car_Cancellation, count = n())

user <- user %>%
  filter(count > 20)
user$user_id <- as.factor(user$user_id)  # making the user_id into categorical to create a bar graph
  
ggplot(user, aes(x = as.factor(user_id), fill = as.factor(Cancellation))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "User ID", 
       y = "Trips", 
       title = "Cancellation Rate by User ID (with more than 20 Trips)")+ 
  ggeasy::easy_center_title() + 
  scale_fill_discrete(name = "Cancellation", 
                      breaks = c("0", "1"), 
                      labels = c("Not Cancelled", "Cancelled")) +
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 0, color = "black", 
            position = position_dodge(width = 1))

### It can be seen that there are some user IDs with no cancellations, and therefore for the purpose of a better visual representation, the 
### IDs with no cancellations will be filter and replot

user <- user %>%
  filter(user_id != 20598 & user_id != 30290 & user_id != 33002 & user_id != 37192 & user_id != 40512)

ggplot(user, aes(x = as.factor(user_id), fill = as.factor(Cancellation))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "User ID", 
       y = "Trips", 
       title = "Cancellation rate by User ID")+ 
  ggeasy::easy_center_title()+ 
  scale_fill_discrete(name = "Cancellation", 
                      breaks = c("0", "1"), 
                      labels = c("Not Cancelled", "Cancelled")) +
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 0, color = "black", size = 5,
            position = position_dodge(width = 1))


## Cancellation rate by vehicle model
vehicle <- taxi_data %>% 
  group_by(vehicle_model_id) %>% 
  summarise(Rides = length(vehicle_model_id), 
            Cancellation = sum(Car_Cancellation)/length(Car_Cancellation)) %>% 
  ungroup()
vehicle <- filter(vehicle, Rides >= 100) #Filtering out cars with less than 100 rides
vehicle$vehicle_model_id = factor(vehicle$vehicle_model_id)
    
    # Bar graph visualisation
ggplot(data = vehicle, aes(x = vehicle_model_id, y = Cancellation, 
                           label = scales::percent(Cancellation))) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Cancellation rate by vehicles with more than 100 rides") +
  ggeasy::easy_center_title() +
  geom_text(vjust = 0, color = "black", size = 5, 
            position = position_dodge(width = 1))

## Cancellation rate by travel type

travel_type <- taxi_data %>%
  group_by(travel_type_id) %>%
  summarise(Cancellation = sum(Car_Cancellation)/length(Car_Cancellation))

travel_type$travel_type_id <- as.factor(travel_type$travel_type_id) # making travel_type to categorical

    # Bar graph visualisation
ggplot(data = travel_type, aes(x = travel_type_id, y = Cancellation, 
                               fill = travel_type_id, label = scales::percent(Cancellation))) + 
  geom_bar(stat = "identity", position = "Stack") +
  scale_fill_brewer(palette = "Blues") + 
  ggtitle("Cancellation rate by Travel Type") +
  ggeasy::easy_center_title() + 
  geom_text(vjust = 0, color = "black", size = 5, 
            position = position_dodge(width = 1))


## Cancellation rate by methods of booking
c  <- taxi_data
c$booking_method = NA
c$booking_method[which(c$mobile_site_booking == 1)] = "Mobile Site"
c$booking_method[which(c$online_booking == 1)] = "Online"
c$booking_method[which(c$Call_In_Booking == 1)] = "Call_In_Booking"

bookmed <- c %>%
  group_by(booking_method) %>%
  summarise(Cancellation = sum(Car_Cancellation)/length(Car_Cancellation))
    
    # Bar graph visualisation
ggplot(data = bookmed, aes(x = booking_method, y = Cancellation, 
                           fill = booking_method, label = scales::percent(Cancellation))) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Blues") + 
  ggtitle("Cancellation rate by Booking Method") +
  ggeasy::easy_center_title() + 
  geom_text(vjust = 0, color = "black", size = 5, 
            position = position_dodge(width = 1))



## Cancellation rate by Day of the Trip - Day Of Week_FromDate
day_week <- taxi_data %>%
  group_by(Day_Of_Week_FromDate) %>%
  summarise(Cancellation = sum(Car_Cancellation)/length(Car_Cancellation))

    # Bar graph visualisation
ggplot(data = day_week, aes(x = Day_Of_Week_FromDate, y = Cancellation, 
                            fill = Day_Of_Week_FromDate, label = scales::percent(Cancellation))) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Blues") + 
  ggtitle("Cancellation rate by Day of the trip") +
  ggeasy::easy_center_title() + 
  geom_text(vjust = 0, color = "black", size = 5, 
            position = position_dodge(width = 1))

###########################*#############################


### STEP 4 - Balancing the data  ###

## Checking the class distribution again
taxi_data %>%
  count(Car_Cancellation) 

## Balance the data
set.seed(12345)
taxi_balance_original <- ovun.sample(Car_Cancellation ~., data = taxi_data, method = "both", seed = 12345)$data

knitr::kable(taxi_balance_original %>%
        count(Car_Cancellation))      # probability table
###########################*#############################


## STEP 5 - Dimension Reduction ## 

str(taxi_balance_original)
glm_dimred <- glm(taxi_balance_original$Car_Cancellation ~. , data = taxi_balance_original, family = "binomial")

summary(glm_dimred)


  # From Area_ID and booking created time are certainly not significant at all
taxi_balance_original <- subset(taxi_balance_original, select = -c(4,11))

## Model re-run with significant variables only
glm_dimred_1<- glm(taxi_balance_original$Car_Cancellation ~. , data = taxi_balance_original, family = "binomial")
summary(glm_dimred_1)


## Calling in the original dataset again to remove the variables which are not significant from the glm model
taxi_data <- subset(taxi_data, select = -c(4, 11))
str(taxi_data)

###########################*#############################


### STEP 6 - Different data allocation ###

## 1. Original uncleaned data 
## taxi

## 2. Balanced original data
## taxi_balance_original


 # Partitioning the data
set.seed(12345)
index <- sample(2, nrow(taxi_data), replace = TRUE, prob = c(0.7, 0.3))
taxi_train_imbalanced <- taxi_data[index == 1,]      # train data
taxi_test <- taxi_data[index == 2,]       # test data

taxi_train_imbalanced %>%
  count(Car_Cancellation)   # train data class proportion

taxi_test %>%
  count(Car_Cancellation)   # test data class proportion

prop.table(table(taxi_data$Car_Cancellation))   # probability table

## 3. Imbalanced train data
## taxi_train_imbalanced

## 4. Test data
## taxi_test


    # Balancing the train data
set.seed(12345)
taxi_train_balanced <- ovun.sample(Car_Cancellation ~., data = taxi_train_imbalanced, method = "both", seed = 12345)$data

knitr::kable(taxi_train_balanced %>%
               count(Car_Cancellation))      # probability table

## 5. Balanced train data
## taxi_train_balanced

###########################*#############################


### STEP 7 - Modelling with Original Imbalanced Data ###


## Model 1: K-Nearest Neighbor ##

## removing irrelevant variable for the model (non-numeric variables)

# Train data
set.seed(12345)
str(taxi_train_imbalanced)
knn_train_o <- subset(taxi_train_imbalanced, select = -c(1, 2, 3, 4, 8, 9, 10, 11, 12))
str(knn_train_o)

# Test data
knn_test_o <- subset(taxi_test, select = -c(1, 2, 3, 4, 8, 9, 10, 11, 12))
str(knn_test_o)

# Checking on the proportion of two classes again
knn_train_o %>%
  count(Car_Cancellation)   # test data class proportion

knn_test_o %>%
  count(Car_Cancellation)   # test data class proportion

## normalizing the data

# Function to normalise
normalise<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Apply the function to train and test data
o_knn_train_n<-as.data.frame(lapply(knn_train_o, normalise))
head(o_knn_train_n)

o_knn_test_n<-as.data.frame(lapply(knn_test_o, normalise))
head(o_knn_test_n)

## Changing the target variable to categorical
o_knn_train_n$Car_Cancellation <- as.factor(o_knn_train_n$Car_Cancellation)
o_knn_test_n$Car_Cancellation <- as.factor(o_knn_test_n$Car_Cancellation)

## Changing other necessary variables to categorical
    # Train data
o_knn_train_n$online_booking <- as.factor(o_knn_train_n$online_booking)
o_knn_train_n$mobile_site_booking <- as.factor(o_knn_train_n$mobile_site_booking)
o_knn_train_n$Call_In_Booking <- as.factor(o_knn_train_n$Call_In_Booking)
o_knn_train_n$travel_package <- as.factor(o_knn_train_n$travel_package)

    # Test data
o_knn_test_n$online_booking <- as.factor(o_knn_test_n$online_booking)
o_knn_test_n$mobile_site_booking <- as.factor(o_knn_test_n$mobile_site_booking)
o_knn_test_n$Call_In_Booking <- as.factor(o_knn_test_n$Call_In_Booking)
o_knn_test_n$travel_package <- as.factor(o_knn_test_n$travel_package)

str(o_knn_test_n)
str(o_knn_train_n)

## Train a model
o_knnmod_7 <- knn(train = o_knn_train_n[,-3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 7 )
o_knnmod_7

## Evaluate the model performance
confusionMatrix(o_knnmod_7, o_knn_test_n$Car_Cancellation, positive = "1")

## Improving the model performance - tuning
set.seed(12345)
o_knntuning <- tune.knn(x= o_knn_train_n[, -3], y = o_knn_train_n$Car_Cancellation, k = 1:30)
summary(o_knntuning)
par(mfrow = c(1, 1))
plot(o_knntuning)


## Evaluating performance using the best k tuning suggested
o_knnmod_27 <- knn(train = o_knn_train_n[,-3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 27 )
confusionMatrix(o_knnmod_27, o_knn_test_n$Car_Cancellation, positive = "1")

## Testing alternative k to get the best sensitivity
o_knnmod_1 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 1 )
confusionMatrix(o_knnmod_1, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_2 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 2 )
confusionMatrix(o_knnmod_2, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_3 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 3 )
confusionMatrix(o_knnmod_3, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_4 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 4 )
confusionMatrix(o_knnmod_4, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_5 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 5 )
confusionMatrix(o_knnmod_5, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_6 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 6 )
confusionMatrix(o_knnmod_6, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_8 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 8 )
confusionMatrix(o_knnmod_8, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_9 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 9 )
confusionMatrix(o_knnmod_9, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_10 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 10 )
confusionMatrix(o_knnmod_10, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_11 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 11 )
confusionMatrix(o_knnmod_11, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_12 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 12 )
confusionMatrix(o_knnmod_12, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_13 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 13 )
confusionMatrix(o_knnmod_13, o_knn_test_n$Car_Cancellation, positive = "1")

o_knnmod_14 <- knn(train = o_knn_train_n[, -3], test = o_knn_test_n[,-3], cl = o_knn_train_n[,3], k = 14 )
confusionMatrix(o_knnmod_14, o_knn_test_n$Car_Cancellation, positive = "1")



## Model 2 : Classification Tree ##

## Train the model
set.seed(12345)
tree_taxi_train_o <- taxi_train_imbalanced

tree_taxi_train_o %>%
  count(Car_Cancellation)   # check the probability of the class in train data again
#--------------------------------------------------------------------------
## Convert the booking methods into a single column - BOOKING_METHOD
tree_taxi_train_o$booking_method <- ifelse(tree_taxi_train_o$online_booking == 1, "Online", 
                                           ifelse(tree_taxi_train_o$mobile_site_booking == 1, "Mobile", "Call"))

    # Remove the three columns with the booking methods
# --------- for Online, Mobile and Call In Booking columns --------- #
tree_taxi_train_o <- subset(tree_taxi_train_o, select = -c(5, 6, 14))
str(tree_taxi_train_o)

## Changing necessary variables to categorical
tree_taxi_train_o$travel_type_id <- as.factor(tree_taxi_train_o$travel_type_id)
tree_taxi_train_o$Car_Cancellation <- as.factor(tree_taxi_train_o$Car_Cancellation)     # Converting the target Car_Cancellation into categorical
tree_taxi_train_o$travel_package <- as.factor(tree_taxi_train_o$travel_package)
tree_taxi_train_o$booking_method <- as.factor(tree_taxi_train_o$booking_method)
str(tree_taxi_train_o)


## Adding additional column for time column - TRIP PERIOD
tree_taxi_train_o <- extract(tree_taxi_train_o, from_date_time, into = c("Hour", "Minute"), "(.{2})(.{6})", remove = FALSE)
str(tree_taxi_train_o$Hour)
tree_taxi_train_o$Hour <- as.numeric(tree_taxi_train_o$Hour)
  # Create a new column 
tree_taxi_train_o$trip_period <- ifelse(tree_taxi_train_o$Hour <= 4, "Midnight", 
                                        ifelse(tree_taxi_train_o$Hour <= 11, "Morning", 
                                               ifelse(tree_taxi_train_o$Hour <= 15, "Afternoon", 
                                                      ifelse(tree_taxi_train_o$Hour <= 19, "Evening", "Night"))))

    # Changing the trip period into categorical
tree_taxi_train_o$trip_period <- as.factor(tree_taxi_train_o$trip_period)

    # Removing the unwanted from_date_time column and its subdivided columns
tree_taxi_train_o <- subset(tree_taxi_train_o, select = -c(10, 11, 12))
str(tree_taxi_train_o)

## Altering Date columns - BOOK_MONTH, BOOK_DAY, TRIP_MONTH and TRIP_DAY

    # Book_month and Book_day
tree_taxi_train_o <- extract(tree_taxi_train_o, booking_created_date, into = c("Booking_Year", "Booking_Month", "Booking_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_train_o <- extract(tree_taxi_train_o, Booking_Month, into = c("m","Book_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_o <- extract(tree_taxi_train_o, Booking_Day, into = c("d","Book_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_o <- subset(tree_taxi_train_o, select = -c(7, 8, 9, 10, 12, 13))

    # Trip_month and Trip_day
tree_taxi_train_o <- extract(tree_taxi_train_o, from_date_date, into = c("From_Year", "From_Month", "From_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_train_o <- extract(tree_taxi_train_o, From_Month, into = c("m","Trip_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_o <- extract(tree_taxi_train_o, From_Day, into = c("d","Trip_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_o <- subset(tree_taxi_train_o, select = -c(10, 11, 12, 13, 15, 16))

str(tree_taxi_train_o)

    # Converting these new columns into numeric
tree_taxi_train_o$Book_Month <- as.factor(tree_taxi_train_o$Book_Month)
tree_taxi_train_o$Book_Day <- as.numeric(tree_taxi_train_o$Book_Day)
tree_taxi_train_o$Trip_Month <- as.factor(tree_taxi_train_o$Trip_Month)
tree_taxi_train_o$Trip_Day <- as.numeric(tree_taxi_train_o$Trip_Day)

str(tree_taxi_train_o)

#-------------------------------------------------------------------------

## Making the same alteration for the testing data
tree_taxi_test_o <- taxi_test

  # Convert the booking methods into a single column - BOOKING_METHOD
tree_taxi_test_o$booking_method <- ifelse(tree_taxi_test_o$online_booking == 1, "Online", 
                                           ifelse(tree_taxi_test_o$mobile_site_booking == 1, "Mobile", "Call"))

    # Remove the three columns with the booking methods
# --------- for Online, Mobile and Call In Booking columns --------- #
tree_taxi_test_o <- subset(tree_taxi_test_o, select = -c(5, 6, 14))
str(tree_taxi_test_o)

## Changing necessary variables to categorical
tree_taxi_test_o$travel_type_id <- as.factor(tree_taxi_test_o$travel_type_id)
tree_taxi_test_o$Car_Cancellation <- as.factor(tree_taxi_test_o$Car_Cancellation)     # Converting the target Car_Cancellation into categorical
tree_taxi_test_o$travel_package <- as.factor(tree_taxi_test_o$travel_package)
tree_taxi_test_o$booking_method <- as.factor(tree_taxi_test_o$booking_method)
str(tree_taxi_test_o)

## Adding additional column for time column - TRIP_PERIOD
tree_taxi_test_o <- extract(tree_taxi_test_o, from_date_time, into = c("Hour", "Month"), "(.{2})(.{6})", remove = FALSE)
str(tree_taxi_test_o$Hour)
tree_taxi_test_o$Hour <- as.numeric(tree_taxi_test_o$Hour)
# Create a new column 
tree_taxi_test_o$trip_period <- ifelse(tree_taxi_test_o$Hour <= 4, "Midnight", 
                                        ifelse(tree_taxi_test_o$Hour <= 11, "Morning", 
                                               ifelse(tree_taxi_test_o$Hour <= 15, "Afternoon", 
                                                      ifelse(tree_taxi_test_o$Hour <= 19, "Evening", "Night"))))

    # Changing the trip period into categorical
tree_taxi_test_o$trip_period <- as.factor(tree_taxi_test_o$trip_period)
str(tree_taxi_test_o)

    # Removing the unwanted from_date_time column and its subdivided columns
tree_taxi_test_o <- subset(tree_taxi_test_o, select = -c(10, 11, 12))
str(tree_taxi_test_o)

## Altering Date columns - BOOK_MONTH, BOOK_DAY, TRIP_MONTH and TRIP_DAY

# Book_month and Book_day
tree_taxi_test_o <- extract(tree_taxi_test_o, booking_created_date, into = c("Booking_Year", "Booking_Month", "Booking_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_test_o <- extract(tree_taxi_test_o, Booking_Month, into = c("m","Book_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_o <- extract(tree_taxi_test_o, Booking_Day, into = c("d","Book_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_o <- subset(tree_taxi_test_o, select = -c(7, 8, 9, 10, 12, 13))

# Trip_month and Trip_day
tree_taxi_test_o <- extract(tree_taxi_test_o, from_date_date, into = c("From_Year", "From_Month", "From_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_test_o <- extract(tree_taxi_test_o, From_Month, into = c("m","Trip_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_o <- extract(tree_taxi_test_o, From_Day, into = c("d","Trip_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_o <- subset(tree_taxi_test_o, select = -c(10, 11, 12, 13, 15, 16))

str(tree_taxi_test_o)

# Converting these new columns data type
tree_taxi_test_o$Book_Month <- as.factor(tree_taxi_test_o$Book_Month)
tree_taxi_test_o$Book_Day <- as.numeric(tree_taxi_test_o$Book_Day)
tree_taxi_test_o$Trip_Month <- as.factor(tree_taxi_test_o$Trip_Month)
tree_taxi_test_o$Trip_Day <- as.numeric(tree_taxi_test_o$Trip_Day)

str(tree_taxi_test_o)

# -------------------------------------------------------------------------
## Train the model
treemod_o <- rpart(Car_Cancellation ~., data = tree_taxi_train_o, 
                 method = "class")

## Plot the tree
summary(treemod_o)
rpart.plot(treemod_o)
prp(treemod_o, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4,split.font = 1, varlen = 8)

## Performance on training data itself
o_tree_pred <- predict(treemod_o, tree_taxi_train_o, type = 'class')
o_tree_pred
confusionMatrix(o_tree_pred, tree_taxi_train_o$Car_Cancellation, positive = "1")

## Performance on testing data
o_tree_pred1 <- predict(treemod_o, tree_taxi_test_o, type = 'class')
confusionMatrix(o_tree_pred1, tree_taxi_test_o$Car_Cancellation, positive = "1")

## Best pruned tree
printcp(treemod_o)
plotcp(treemod_o)
min.cp_o <- treemod_o$cptable[which.min(treemod_o$cptable[,"xerror"]), "CP"]
min.cp_o
best.pruned_tree_o <- prune(treemod_o, cp = min.cp_o)
rpart.plot(best.pruned_tree_o)

## Performance on the testing data using the best pruned tree
o_tree_pred2 <- predict(best.pruned_tree_o, tree_taxi_test_o, type = 'class')
confusionMatrix(o_tree_pred2, tree_taxi_test_o$Car_Cancellation, positive = "1")



## Model 3 : Logistic Regression ##

## Loading the train data

log_train_o <- taxi_train_imbalanced

log_train_o %>%
  count(Car_Cancellation)    # checking probability of class in log train data

str(log_train_o)

## Converting categorical variables 
log_train_o$travel_type_id <- as.factor(log_train_o$travel_type_id)
log_train_o$online_booking <- as.factor(log_train_o$online_booking)
log_train_o$mobile_site_booking <- as.factor(log_train_o$mobile_site_booking)
log_train_o$Car_Cancellation <- as.factor(log_train_o$Car_Cancellation)
log_train_o$Call_In_Booking <- as.factor(log_train_o$Call_In_Booking)
log_train_o$travel_package <- as.factor(log_train_o$travel_package)
str(log_train_o)

## Fit the logistic regression model
logmod_o <- glm(Car_Cancellation ~., data = log_train_o, family = binomial)
options(scipen = 999)
summary(logmod_o)

## Test on the training data itself
log_pred_o <- predict(logmod_o, log_train_o, type = "response")

  # set the cutoff value as 0.5
log_pred_o_result <- ifelse(log_pred_o > 0.5, 1, 0)
str(log_pred_o_result)
log_pred_o_result
log_pred_o_result <- as.factor(log_pred_o_result)
confusionMatrix(log_pred_o_result, log_train_o$Car_Cancellation, positive = "1")

# Use the model on the test data
  # Load and alter the test data
log_test_o <- taxi_test
log_test_o$travel_type_id <- as.factor(log_test_o$travel_type_id)
log_test_o$online_booking <- as.factor(log_test_o$online_booking)
log_test_o$mobile_site_booking <- as.factor(log_test_o$mobile_site_booking)
log_test_o$Car_Cancellation <- as.factor(log_test_o$Car_Cancellation)
log_test_o$Call_In_Booking <- as.factor(log_test_o$Call_In_Booking)
log_test_o$travel_package <- as.factor(log_test_o$travel_package)

  # Run the model on test data
log_pred1_o <- predict(logmod_o, log_test_o, type = 'response')

  # Set the cutoff value as 0.5
log_pred1_o_result <- ifelse(log_pred1_o > 0.5, 1, 0)
log_pred1_o_result <- as.factor(log_pred1_o_result)
confusionMatrix(log_pred1_o_result, log_test_o$Car_Cancellation, positive = "1")



## Model 4 : Neural Network ##

## Train data
nn_train_o <- tree_taxi_train_o
nn_train_dum_o <- dummy.data.frame(nn_train_o, sep = "_")

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

nn_train_dum_norm_o <- as.data.frame(lapply(nn_train_dum_o, normalize))
str(nn_train_dum_norm_o)

## Test data
nn_test_o <- tree_taxi_test_o
nn_test_dum_o <- dummy.data.frame(nn_test_o, sep = "_")

nn_test_dum_norm_o <- as.data.frame(lapply(nn_test_dum_o, normalize))
str(nn_test_dum_norm_o)

## Run the model
set.seed(12345)
nn_mod_o <- neuralnet(Car_Cancellation_0 + Car_Cancellation_1 ~., 
                      data = nn_train_dum_norm_o)
plot(nn_mod_o)

`# Predict on train
pred_0_o <- neuralnet::compute(nn_mod_o, nn_train_dum_norm_o[, -c(7,8)])

confusionMatrix(factor(ifelse(pred_0_o$net.result[,2] > 0.5, 1, 0), levels = c(1,0)), 
                factor(nn_train_dum_norm_o$Car_Cancellation_1, levels = c(1,0)))


## Predict on test 
pred_o <- neuralnet::compute(nn_mod_o, nn_test_dum_norm_o[, -c(7,8)])
head(pred_o$net.result)

confusionMatrix(factor(ifelse(pred_o$net.result[,2] > 0.5, 1, 0), levels = c(1,0)), 
                factor(nn_test_dum_norm_o$Car_Cancellation_1, levels = c(1,0)))

## More hidden layers
set.seed(12345)
nn_mod_o_1 <- neuralnet(Car_Cancellation_0 + Car_Cancellation_1 ~., 
                        data = nn_train_dum_norm_o, 
                        hidden = 2)
plot(nn_mod_o_1)

  # predict on test data for new model
pred_o_1 <- neuralnet::compute(nn_mod_o_1, nn_test_dum_norm_o[, -c(7,8)])


confusionMatrix(factor(ifelse(pred_o_1$net.result[,2] > 0.5, 1, 0), levels = c(1,0)), 
                factor(nn_test_dum_norm_o$Car_Cancellation_1, levels = c(1,0)))

# ------- the model performance remains the same with more hidden layers --------

###########################*#############################


### STEP 8 - Modelling with Balanced Data ###

## Model 1: K-Nearest Neighbor ##

## removing irrelevant variable for the model (non-numeric variables)

    # Train data
str(taxi_train_balanced)
knn_train_b <- subset(taxi_train_balanced, select = -c(1, 2, 3, 4, 8, 9, 10, 11, 12))

    # Test data
knn_test_b <- subset(taxi_test, select = -c(1, 2, 3, 4, 8, 9, 10, 11, 12))
str(knn_test_b)

    # Checking on the proportion of two classes again
knn_train_b %>%
  count(Car_Cancellation)   # test data class proportion

knn_test_b %>%
  count(Car_Cancellation)   # test data class proportion

head(knn_test_b)

## normalizing the data

    # Function to normalise
normalise<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

    # Apply the function to train and test data
b_knn_train_n<-as.data.frame(lapply(knn_train_b, normalise))
head(b_knn_train_n)

b_knn_test_n<-as.data.frame(lapply(knn_test_b, normalise))
head(b_knn_test_n)

## Changing the target variable to categorical
b_knn_train_n$Car_Cancellation <- as.factor(b_knn_train_n$Car_Cancellation)  # training data
b_knn_test_n$Car_Cancellation <- as.factor(b_knn_test_n$Car_Cancellation)   # testing data


## Changing other necessary variables to categorical
    # Train data
b_knn_train_n$online_booking <- as.factor(b_knn_train_n$online_booking)
b_knn_train_n$mobile_site_booking <- as.factor(b_knn_train_n$mobile_site_booking)
b_knn_train_n$Call_In_Booking <- as.factor(b_knn_train_n$Call_In_Booking)
b_knn_train_n$travel_package <- as.factor(b_knn_train_n$travel_package)

    # Test data
b_knn_test_n$online_booking <- as.factor(b_knn_test_n$online_booking)
b_knn_test_n$mobile_site_booking <- as.factor(b_knn_test_n$mobile_site_booking)
b_knn_test_n$Call_In_Booking <- as.factor(b_knn_test_n$Call_In_Booking)
b_knn_test_n$travel_package <- as.factor(b_knn_test_n$travel_package)

str(b_knn_test_n)
str(b_knn_train_n)


## Train a model
b_knnmod_7 <- knn(train = b_knn_train_n[,-3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 7 )
b_knnmod_7

## Evaluate the model performance
confusionMatrix(b_knnmod_7, b_knn_test_n$Car_Cancellation, positive = "1")

# Improving the model performance - tuning
library(e1071)

b_knntuning <- tune.knn(x= b_knn_train_n[, -3], y = b_knn_train_n$Car_Cancellation, k = 1:30)
summary(b_knntuning)
plot(b_knntuning)


# Evaluating performance using the best k tuning suggested
b_knnmod_1 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 1 )
confusionMatrix(b_knnmod_1, b_knn_test_n$Car_Cancellation, positive = "1")

# Testing alternative k to get the best sensitivity
b_knnmod_2 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 2 )
confusionMatrix(b_knnmod_2, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_3 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 3 )
confusionMatrix(b_knnmod_3, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_4 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 4 )
confusionMatrix(b_knnmod_4, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_5 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 5 )
confusionMatrix(b_knnmod_5, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_6 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 6 )
confusionMatrix(b_knnmod_6, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_8 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 8 )
confusionMatrix(b_knnmod_8, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_9 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 9 )
confusionMatrix(b_knnmod_9, b_knn_test_n$Car_Cancellation, positive = "1")

b_knnmod_10 <- knn(train = b_knn_train_n[, -3], test = b_knn_test_n[,-3], cl = b_knn_train_n[,3], k = 10 )
confusionMatrix(b_knnmod_10, b_knn_test_n$Car_Cancellation, positive = "1")



## Model 2 : Classification Tree


    # Train the model
set.seed(12345)
tree_taxi_train_b <- taxi_train_balanced

tree_taxi_train_b %>%
  count(Car_Cancellation)   # check the probability of the class in train data again

# --------------------------------------------------------------------------
  ## Convert the booking methods into a single column - BOOKING_METHOD
tree_taxi_train_b$booking_method <- ifelse(tree_taxi_train_b$online_booking == 1, "Online", 
                                             ifelse(tree_taxi_train_b$mobile_site_booking == 1, "Mobile", "Call"))

# Remove the three columns with the booking methods
# --------- for Online, Mobile and Call In Booking columns --------- #
tree_taxi_train_b <- subset(tree_taxi_train_b, select = -c(5, 6, 14))
str(tree_taxi_train_b)

## Changing necessary variables to categorical
tree_taxi_train_b$travel_type_id <- as.factor(tree_taxi_train_b$travel_type_id)
tree_taxi_train_b$Car_Cancellation <- as.factor(tree_taxi_train_b$Car_Cancellation)     # Converting the target Car_Cancellation into categorical
tree_taxi_train_b$travel_package <- as.factor(tree_taxi_train_b$travel_package)
tree_taxi_train_b$booking_method <- as.factor(tree_taxi_train_b$booking_method)
str(tree_taxi_train_b)


## Adding additional column for time column - TRIP PERIOD
tree_taxi_train_b <- extract(tree_taxi_train_b, from_date_time, into = c("Hour", "Minute"), "(.{2})(.{6})", remove = FALSE)
str(tree_taxi_train_b$Hour)
tree_taxi_train_b$Hour <- as.numeric(tree_taxi_train_b$Hour)
# Create a new column 
tree_taxi_train_b$trip_period <- ifelse(tree_taxi_train_b$Hour <= 4, "Midnight", 
                                        ifelse(tree_taxi_train_b$Hour <= 11, "Morning", 
                                               ifelse(tree_taxi_train_b$Hour <= 15, "Afternoon", 
                                                      ifelse(tree_taxi_train_b$Hour <= 19, "Evening", "Night"))))

# Changing the trip period into categorical
tree_taxi_train_b$trip_period <- as.factor(tree_taxi_train_b$trip_period)

# Removing the unwanted from_date_time column and its subdivided columns
tree_taxi_train_b <- subset(tree_taxi_train_b, select = -c(10, 11, 12))
str(tree_taxi_train_b)

## Altering Date columns - BOOK_MONTH, BOOK_DAY, TRIP_MONTH and TRIP_DAY

# Book_month and Book_day
tree_taxi_train_b <- extract(tree_taxi_train_b, booking_created_date, into = c("Booking_Year", "Booking_Month", "Booking_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_train_b <- extract(tree_taxi_train_b, Booking_Month, into = c("m","Book_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_b <- extract(tree_taxi_train_b, Booking_Day, into = c("d","Book_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_b <- subset(tree_taxi_train_b, select = -c(7, 8, 9, 10, 12, 13))

# Trip_month and Trip_day
tree_taxi_train_b <- extract(tree_taxi_train_b, from_date_date, into = c("From_Year", "From_Month", "From_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_train_b <- extract(tree_taxi_train_b, From_Month, into = c("m","Trip_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_b <- extract(tree_taxi_train_b, From_Day, into = c("d","Trip_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_train_b <- subset(tree_taxi_train_b, select = -c(10, 11, 12, 13, 15, 16))

str(tree_taxi_train_b)

# Converting these new columns into numeric
tree_taxi_train_b$Book_Month <- as.factor(tree_taxi_train_b$Book_Month)
tree_taxi_train_b$Book_Day <- as.numeric(tree_taxi_train_b$Book_Day)
tree_taxi_train_b$Trip_Month <- as.factor(tree_taxi_train_b$Trip_Month)
tree_taxi_train_b$Trip_Day <- as.numeric(tree_taxi_train_b$Trip_Day)

str(tree_taxi_train_b)

#-------------------------------------------------------------------------
## Making the same alteration for the testing data
tree_taxi_test_b <- taxi_test

  # Convert the booking methods into a single column - BOOKING_METHOD
tree_taxi_test_b$booking_method <- ifelse(tree_taxi_test_b$online_booking == 1, "Online", 
                                          ifelse(tree_taxi_test_b$mobile_site_booking == 1, "Mobile", "Call"))

  # Remove the three columns with the booking methods
# --------- for Online, Mobile and Call In Booking columns --------- #
tree_taxi_test_b <- subset(tree_taxi_test_b, select = -c(5, 6, 14))
str(tree_taxi_test_b)

## Changing necessary variables to categorical
tree_taxi_test_b$travel_type_id <- as.factor(tree_taxi_test_b$travel_type_id)
tree_taxi_test_b$Car_Cancellation <- as.factor(tree_taxi_test_b$Car_Cancellation)     # Converting the target Car_Cancellation into categorical
tree_taxi_test_b$travel_package <- as.factor(tree_taxi_test_b$travel_package)
tree_taxi_test_b$booking_method <- as.factor(tree_taxi_test_b$booking_method)
str(tree_taxi_test_b)

## Adding additional column for time column - TRIP_PERIOD
tree_taxi_test_b <- extract(tree_taxi_test_b, from_date_time, into = c("Hour", "Month"), "(.{2})(.{6})", remove = FALSE)
str(tree_taxi_test_b$Hour)
tree_taxi_test_b$Hour <- as.numeric(tree_taxi_test_b$Hour)
  # Create a new column 
tree_taxi_test_b$trip_period <- ifelse(tree_taxi_test_b$Hour <= 4, "Midnight", 
                                       ifelse(tree_taxi_test_b$Hour <= 11, "Morning", 
                                              ifelse(tree_taxi_test_b$Hour <= 15, "Afternoon", 
                                                     ifelse(tree_taxi_test_b$Hour <= 19, "Evening", "Night"))))

  # Changing the trip period into categorical
tree_taxi_test_b$trip_period <- as.factor(tree_taxi_test_b$trip_period)
str(tree_taxi_test_b)

  # Removing the unwanted from_date_time column and its subdivided columns
tree_taxi_test_b <- subset(tree_taxi_test_b, select = -c(10, 11, 12))
str(tree_taxi_test_b)

## Altering Date columns - BOOK_MONTH, BOOK_DAY, TRIP_MONTH and TRIP_DAY

  # Book_month and Book_day
tree_taxi_test_b <- extract(tree_taxi_test_b, booking_created_date, into = c("Booking_Year", "Booking_Month", "Booking_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_test_b <- extract(tree_taxi_test_b, Booking_Month, into = c("m","Book_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_b <- extract(tree_taxi_test_b, Booking_Day, into = c("d","Book_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_b <- subset(tree_taxi_test_b, select = -c(7, 8, 9, 10, 12, 13))

  # Trip_month and Trip_day
tree_taxi_test_b <- extract(tree_taxi_test_b, from_date_date, into = c("From_Year", "From_Month", "From_Day"), "(.{4})(.{3})(.{3})", remove = FALSE)
tree_taxi_test_b <- extract(tree_taxi_test_b, From_Month, into = c("m","Trip_Month"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_b <- extract(tree_taxi_test_b, From_Day, into = c("d","Trip_Day"), "(.{1})(.{2})", remove = FALSE)
tree_taxi_test_b <- subset(tree_taxi_test_b, select = -c(10, 11, 12, 13, 15, 16))

str(tree_taxi_test_b)

# Converting these new columns data type
tree_taxi_test_b$Book_Month <- as.factor(tree_taxi_test_b$Book_Month)
tree_taxi_test_b$Book_Day <- as.numeric(tree_taxi_test_b$Book_Day)
tree_taxi_test_b$Trip_Month <- as.factor(tree_taxi_test_b$Trip_Month)
tree_taxi_test_b$Trip_Day <- as.numeric(tree_taxi_test_b$Trip_Day)

str(tree_taxi_test_b)

#-------------------------------------------------------------------------

## Fit the model
treemod_b <- rpart(Car_Cancellation ~., data = tree_taxi_train_b, 
                 method = "class", xval = 5)

    # Plot the tree
summary(treemod_b)
prp(treemod_b, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4,split.font = 1, varlen = 8, cex = 0.7)
rpart.plot(treemod_b)

    # Performance on training data itself
b_tree_pred <- predict(treemod_b, tree_taxi_train_b, type = 'class')
b_tree_pred
confusionMatrix(b_tree_pred, tree_taxi_train_b$Car_Cancellation, positive = "1")

    # Performance on testing data
b_tree_pred1 <- predict(treemod_b, tree_taxi_test_b, type = 'class')
confusionMatrix(b_tree_pred1, tree_taxi_test_b$Car_Cancellation, positive = "1")

    # Best pruned tree
printcp(treemod_b)
plotcp(treemod_b)
min.cp_b <- treemod_b$cptable[which.min(treemod_b$cptable[,"xerror"]), "CP"]
min.cp_b
best.pruned_tree_b <- prune(treemod_b, cp = min.cp_b)
rpart.plot(best.pruned_tree_b, digits = 5, fallen.leaves = T, type= 1,cex = 0.6)

    # Performance on the testing data using the best pruned tree
b_tree_pred2 <- predict(best.pruned_tree_b, tree_taxi_test_b, type = 'class')
confusionMatrix(b_tree_pred2, tree_taxi_test_b$Car_Cancellation, positive = "1")

# Changing controls (Improve model performance)
set.seed(1234)
control <- rpart.control(cp = 0.01, xval = 10, minsplit = 2)
treemod_b_2 <-  rpart(Car_Cancellation ~ ., data = tree_taxi_train_b, control = control)


# Pruned using controls
pruned_tree_2 <- prune(treemod_b_2, cp = 0.01)
rpart.plot(treemod_b_2)


b_tree_pred3 <- predict(pruned_tree_2, tree_taxi_test_b, type = 'class')
confusionMatrix(b_tree_pred3, tree_taxi_test_b$Car_Cancellation, positive = "1")
# ----- the model performance remains the same after changing controls



## Model 3 : Logistic Regression

## Loading the train data

log_train_b <- taxi_train_balanced

log_train_b %>%
  count(Car_Cancellation)    # checking probability of class in log train data

str(log_train_b)

## Converting categorical variables 
log_train_b$travel_type_id <- as.factor(log_train_b$travel_type_id)
log_train_b$online_booking <- as.factor(log_train_b$online_booking)
log_train_b$mobile_site_booking <- as.factor(log_train_b$mobile_site_booking)
log_train_b$Car_Cancellation <- as.factor(log_train_b$Car_Cancellation)
log_train_b$Call_In_Booking <- as.factor(log_train_b$Call_In_Booking)
log_train_b$travel_package <- as.factor(log_train_b$travel_package)
str(log_train_b)

## Fit the logistic regression model
logmod_b <- glm(Car_Cancellation ~., data = log_train_b, family = binomial)
options(scipen = 999)
summary(logmod_b)

## Test on the training data itself
log_pred_b <- predict(logmod_b, log_train_b, type = "response")

## set the cutoff value as 0.5
log_pred_b_result <- ifelse(log_pred_b > 0.5, 1, 0)
str(log_pred_b_result)
log_pred_b_result
log_pred_b_result <- as.factor(log_pred_b_result)
confusionMatrix(log_pred_b_result, log_train_b$Car_Cancellation, positive = "1")

## Use the model on the test data
  # Load and alter the test data
log_test_b <- taxi_test
log_test_b$travel_type_id <- as.factor(log_test_b$travel_type_id)
log_test_b$online_booking <- as.factor(log_test_b$online_booking)
log_test_b$mobile_site_booking <- as.factor(log_test_b$mobile_site_booking)
log_test_b$Car_Cancellation <- as.factor(log_test_b$Car_Cancellation)
log_test_b$Call_In_Booking <- as.factor(log_test_b$Call_In_Booking)
log_test_b$travel_package <- as.factor(log_test_b$travel_package)
str(log_test_b)
str(log_train_b)

  # Run the model on the test data
log_pred1_b <- predict(logmod_b, log_test_b, type = "response")

# Set the cutoff value as 0.5
log_pred1_b_result <- ifelse(log_pred1_b > 0.5, 1, 0)
log_pred1_b_result <- as.factor(log_pred1_b_result)
confusionMatrix(log_pred1_b_result, log_test_b$Car_Cancellation, positive = "1")



## Model 4 : Neural Network ##

## Train data
nn_train_b <- tree_taxi_train_b
nn_train_dum_b <- dummy.data.frame(nn_train_b, sep = "_")

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

nn_train_dum_norm_b <- as.data.frame(lapply(nn_train_dum_b, normalize))
str(nn_train_dum_norm_b)

## Test data
nn_test_b <- tree_taxi_test_b
nn_test_dum_b <- dummy.data.frame(nn_test_b, sep = "_")

nn_test_dum_norm_b <- as.data.frame(lapply(nn_test_dum_b, normalize))
str(nn_test_dum_norm_b)

## Run the model
set.seed(12345)
nn_mod_b <- neuralnet(Car_Cancellation_0 + Car_Cancellation_1 ~., 
                    data = nn_train_dum_norm_b)
plot(nn_mod_b)

`# Predict on train
pred_0_b <- neuralnet::compute(nn_mod_b, nn_train_dum_norm_b[, -c(7,8)])

confusionMatrix(factor(ifelse(pred_0_b$net.result[,2] > 0.5, 1, 0), levels = c(1,0)), 
                factor(nn_train_dum_norm_b$Car_Cancellation_1, levels = c(1,0)))


## Predict on test 
pred_b <- neuralnet::compute(nn_mod_b, nn_test_dum_norm_b[, -c(7,8)])
head(pred_b$net.result)

confusionMatrix(factor(ifelse(pred_b$net.result[,2] > 0.5, 1, 0), levels = c(1,0)), 
                factor(nn_test_dum_norm_b$Car_Cancellation_1, levels = c(1,0)))



###########################*#############################
### STEP 9 - Performance Evaluation (Original versus Balanced) ###
# ---- Balanced data models are selected. Refer to paper for more discussion. -----------

###########################*#############################


### STEP 10 - Performance Evaluation (Among the four models from balanced dataset) ###
# ------------------ ROC CURVE -------------------------

## Models in final selection
# KNN : b_knn_mod_1
# Classification tree : best.pruned_tree_b
# Logistic Regression : log_mod_b
# Neural Network : nn_mod_b

## ROC for KNN model
library(pROC)
ROC_KNN <- roc(as.numeric(b_knn_test_n$Car_Cancellation), as.numeric(b_knnmod_1), 
               plot = TRUE, percent = TRUE)

plot(ROC_KNN, print.auc = TRUE, col = "red", main = "ROC Curve for KNN")


## ROC for Classification Tree
b_tree_pred2
ROC_Tree <- roc(as.numeric(tree_taxi_test_b$Car_Cancellation), as.numeric(b_tree_pred2), 
                plot = TRUE, percent = TRUE)

plot(ROC_Tree, print.auc = TRUE, col = "red", main = "ROC Curve for Classification Tree")


## ROC for Logistic Regression

ROC_Log <- roc(log_test_b$Car_Cancellation, log_pred1_b, 
               plot= TRUE, percent = TRUE)
plot(ROC_Log, print.auc = TRUE, col = "red", main = "ROC Curve for Logistic Regression")


## ROC for Neural Network
pred_b$net.result
nn_result <- ifelse(pred_b$net.result[,2] > 0.5, 1, 0)
nn_result  
ROC_NeuralNet <- roc(nn_test_dum_norm_b$Car_Cancellation_1, nn_result,
                     plot= TRUE, percent = TRUE)
plot(ROC_NeuralNet, print.auc = TRUE, col = "red", main = "ROC Curve for Neural Network")

## Combined plot for Neural Network
par(mfrow = c(2, 2))
plot(ROC_KNN, print.auc = TRUE, col = "red", main = "ROC Curve for KNN")
plot(ROC_Tree, print.auc = TRUE, col = "red", main = "ROC Curve for Classification Tree")
plot(ROC_Log, print.auc = TRUE, col = "red", main = "ROC Curve for Logistic Regression")
plot(ROC_NeuralNet, print.auc = TRUE, col = "red", main = "ROC Curve for Neural Network")

##################################################################

## Variable Importance

# Tree Model
best.pruned_tree_b$variable.importance

# Logisitic Regression for variable significance
summary(logmod_b)

# Neural Network
nn_mod_b$weights
nn_test_dum_norm_b[, c(45, 44, 25, 47, 21)]

