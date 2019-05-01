
library(jsonlite)
library(curl)
library(ggplot2)
# install.packages("devtools")
library(devtools)
library(lattice)
library(plyr)
require(randomForest)
library(lubridate)


json_file <- stream_in(file("/home/vignesh/Documents/ASM/Business_Toronto_Restaurant.json"))


class(json_file$categories)
head(json_file$categories)
cat_total <- unlist(json_file$categories)

## Which categories are most popular?
cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
cat_names <- names(cat_names_sort)[2:11] ## 1 is Restaurants - we don't need this

cat_bus_ind_mat <- sapply(json_file$categories, function(y) as.numeric(cat_names %in% y))
cat_bus_ind_mat <- t(cat_bus_ind_mat)
colnames(cat_bus_ind_mat) <- cat_names
df_TO_tidy_cat <- cbind(json_file, cat_bus_ind_mat)
# df_TO_tidy_cat


drops <- c("categories")
df_category_removed = df_TO_tidy_cat[ , !(names(df_TO_tidy_cat) %in% drops)]
json_data_flattened <- flatten(df_category_removed)

attributes = c('hours.Monday','hours.Tuesday','hours.Wednesday','hours.Thursday','hours.Saturday','hours.Sunday')
attributes1 = c('hours.Monday','hours.Tuesday','hours.Wednesday','hours.Thursday','hours.Friday','hours.Saturday','hours.Sunday')
x_train = json_data_flattened[!is.na(json_data_flattened$hours.Friday),attributes1]
x_train1 = x_train[!is.na(x_train$hours.Tuesday),]
# y_train = json_data_flattened[!is.na(json_data_flattened$hours.Friday),'hours.Friday']
# x_test = json_data_flattened[is.na(json_data_flattened$hours.Friday),attributes]
# # y_test = json_data_flattened[is.na(json_data_flattened$hours.Friday),'hours.Friday']
x_train1$hours.Friday = factor(x_train1$hours.Friday)
x_train1$hours.Tuesday = factor(x_train1$hours.Tuesday)

getHours <- function(x){
    print(length(x))
    Time <- factor(x)
    a <- hms(as.character(Time))
    hour(a)
}

getDurationBucket <- function(x){
    print(length(x))
    Time <- factor(x)
    a <- hms(as.character(Time))
    hour <- hour(a)
#     duration = "midday"
#     if (is.na(x)) {
#         duration = "None"
#     }
#     if (hour>=5 && hour<=8){
#         duration = "early morning 5am - 7:59am"
#     }
#     else if (hour>8 && hour<=12){
#         duration = "moring 8am-11:59am"
#     }
#     else if (hour>12 && hour<=4){
#         duration = "afternoon 12pm-3:59pm"
#     }
#     else{
#         duration = "evening 4pm-8pm"
#     }
#     duration
    duration = c()
#     print(hour)
    for (i in hour){
#         print(paste(i," #####"))
        if (is.na(i)) {
            duration = c(duration,"None")
        }
        else if (i>=5 && i<=8){
            duration = c(duration,"early morning 5am - 7:59am")
        }
        else if (i>8 && i<=12){
            duration = c(duration,"moring 8am-11:59am")
        }
        else if (i>12 && i<=4){
            duration = c(duration,"afternoon 12pm-3:59pm")
        }
        else if (i>4 && i<=8){
            duration = c(duration,"evening 4pm-7pm")
        }
        else{
            duration = c(duration,"evening 7pm-12pm")
        }
    }
#     hour
    duration
}
# apply(unique(m$hours.Friday))

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Monday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattened$Monday_opening =  hours$X1
json_data_flattened$Monday_closing =  hours$X2
json_data_flattened$Monday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Monday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Tuesday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattenedTuesday_opening =  hours$X1
json_data_flattened$Tuesday_closing =  hours$X2
json_data_flattened$Tuesday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Tuesday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Wednesday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattened$Wednesday_opening =  hours$X1
json_data_flattened$Wednesday_closing =  hours$X2
json_data_flattened$Wednesday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Wednesday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Thursday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattened$Thursday_opening =  hours$X1
json_data_flattened$Thursday_closing =  hours$X2
json_data_flattened$Thursday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Thursday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Friday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattened$Friday_opening =  hours$X1
json_data_flattened$Friday_closing =  hours$X2
json_data_flattened$Friday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Friday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Saturday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattened$Saturday_opening =  hours$X1
json_data_flattened$Saturday_closing =  hours$X2
json_data_flattened$Saturday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Saturday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split <- data.frame(do.call(rbind, strsplit(as.vector((json_data_flattened$hours.Sunday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
json_data_flattened$Sunday_opening =  hours$X1
json_data_flattened$Sunday_closing =  hours$X2
json_data_flattened$Sunday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
json_data_flattened$Sunday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1


x_train = json_data_flattened[json_data_flattened$Monday_duration_bucket != 'None',c('Monday_duration_bucket',
                                                                                   'Tuesday_duration_bucket',
                                                                                   'Wednesday_duration_bucket',
                                                                                   'Thursday_duration_bucket',
                                                                                   'Friday_duration_bucket',
                                                                                   'Saturday_duration_bucket',
                                                                                   'Sunday_duration_bucket')]

x_test = json_data_flattened[json_data_flattened$Monday_duration_bucket == 'None',c(
                                                                                   'Tuesday_duration_bucket',
                                                                                   'Wednesday_duration_bucket',
                                                                                   'Thursday_duration_bucket',
                                                                                   'Friday_duration_bucket',
                                                                                   'Saturday_duration_bucket',
                                                                                   'Sunday_duration_bucket')]



x_train$Monday_duration_bucket = droplevels(x_train$Monday_duration_bucket)
x_train$Monday_duration_bucket = factor(x_train$Monday_duration_bucket)
rf <- randomForest(Monday_duration_bucket~.,data=x_train)

length(predict(rf,x_test))

json_data_flattened[json_data_flattened$Monday_duration_bucket=='None','Monday_duration_bucket'] = predict(rf,x_test)

x_train = json_data_flattened[json_data_flattened$Sunday_duration_bucket != 'None',c('Monday_duration_bucket',
                                                                                   'Tuesday_duration_bucket',
                                                                                   'Wednesday_duration_bucket',
                                                                                   'Thursday_duration_bucket',
                                                                                   'Friday_duration_bucket',
                                                                                   'Saturday_duration_bucket',
                                                                                   'Sunday_duration_bucket')]

x_test = json_data_flattened[json_data_flattened$Sunday_duration_bucket == 'None',c('Monday_duration_bucket',
                                                                                   'Tuesday_duration_bucket',
                                                                                   'Wednesday_duration_bucket',
                                                                                   'Thursday_duration_bucket',
                                                                                   'Friday_duration_bucket',
                                                                                   'Saturday_duration_bucket')]



x_train$Sunday_duration_bucket = droplevels(x_train$Sunday_duration_bucket)
x_train$Sunday_duration_bucket = factor(x_train$Sunday_duration_bucket)
rf <- randomForest(Sunday_duration_bucket~.,data=x_train)
rf

json_data_flattened[json_data_flattened$Sunday_duration_bucket=='None','Sunday_duration_bucket'] = predict(rf,x_test)

attributes = c(
                'attributes.BusinessAcceptsCreditCards',
                'attributes.Alcohol',
                'attributes.HasTV',
                'attributes.NoiseLevel',
                'attributes.BikeParking',
                'attributes.BusinessParking.validated',
                'attributes.RestaurantsAttire',
                'attributes.WiFi',
                'attributes.RestaurantsPriceRange2',
                'stars'
              )

attributes1 = c(
                'attributes.BusinessAcceptsCreditCards',
                'attributes.Alcohol',
                'attributes.HasTV',
                'attributes.NoiseLevel',
                'attributes.BikeParking',
                'attributes.BusinessParking.validated',
                'attributes.RestaurantsAttire',
                'attributes.WiFi',
                'stars'
              )

X_train = json_data_flattened[!is.na(json_data_flattened$attributes.RestaurantsPriceRange2),attributes]
X_test = json_data_flattened[is.na(json_data_flattened$attributes.RestaurantsPriceRange2),attributes1]

X_train = X_train[complete.cases(X_train), ]
X_train$attributes.RestaurantsPriceRange2 = factor(X_train$attributes.RestaurantsPriceRange2)
X_train$attributes.BusinessAcceptsCreditCards = factor(X_train$attributes.BusinessAcceptsCreditCards)
X_train$attributes.Alcohol = factor(X_train$attributes.Alcohol)
X_train$attributes.HasTV = factor(X_train$attributes.HasTV)
X_train$attributes.NoiseLevel = factor(X_train$attributes.NoiseLevel)
X_train$attributes.BikeParking = factor(X_train$attributes.BikeParking)
X_train$attributes.BusinessParking.validated = factor(X_train$attributes.BusinessParking.validated)
X_train$attributes.RestaurantsAttire = factor(X_train$attributes.RestaurantsAttire)


X_test$attributes.BusinessAcceptsCreditCards = factor(X_test$attributes.BusinessAcceptsCreditCards)
X_test$attributes.Alcohol = factor(X_test$attributes.Alcohol)
X_test$attributes.HasTV = factor(X_test$attributes.HasTV)
X_test$attributes.NoiseLevel = factor(X_test$attributes.NoiseLevel)
X_test$attributes.BikeParking = factor(X_test$attributes.BikeParking)
X_test$attributes.BusinessParking.validated = factor(X_test$attributes.BusinessParking.validated)

# rf <- randomForest(attributes.RestaurantsPriceRange2~
#                    attributes.BusinessAcceptsCreditCards
#                    +attributes.Alcohol
#                    +attributes.HasTV
#                    +attributes.NoiseLevel
#                    +attributes.BikeParking
#                    +attributes.BusinessParking.validated
#                    ,data=X_train)

# rf

json_data_flattened[json_data_flattened$Monday_duration_bucket=="None",]

X_train[1:3,]

# predict(rf,X_test)

# json_data_flattened[is.na(json_data_flattened$attributes.RestaurantsPriceRange2),'attributes.RestaurantsPriceRange2'] = predict(rf,X_test)

library(rpart)

dt <- rpart(attributes.RestaurantsPriceRange2~.,data=X_train)

json_data_flattened[is.na(json_data_flattened$attributes.RestaurantsPriceRange2),'attributes.RestaurantsPriceRange2'] = predict(dt,X_test,type="class")

dt

table(predict(dt,X_train[,attributes1],type='class'),X_train$attributes.RestaurantsPriceRange2)

review_data <- read.csv('Review_posterior.csv')

library(dplyr)

review_data$business_id = factor(review_data$business_id)
json_data_flattened_merged = merge(json_data_flattened, review_data, by="business_id",all.x=TRUE)
json_data_flattened_merged$posterior = coalesce(json_data_flattened_merged$expected_means,json_data_flattened_merged$stars)

json_data_flattened[is.na(json_data_flattened$attributes.RestaurantsPriceRange2),'attributes.RestaurantsPriceRange2']

json_data_flattened_merged[is.na(json_data_flattened_merged$attributes.GoodForMeal.latenight),'attributes.GoodForMeal.latenight'] = "not applicable"
json_data_flattened_merged[is.na(json_data_flattened_merged$attributes.GoodForMeal.lunch),'attributes.GoodForMeal.lunch'] = "not applicable"
json_data_flattened_merged[is.na(json_data_flattened_merged$attributes.GoodForMeal.breakfast),'attributes.GoodForMeal.breakfast'] = "not applicable"

# train[is.na(train$attributes.GoodForMeal.latenight),'attributes.GoodForMeal.latenight']

train = json_data_flattened_merged[,
                   c('Food','Nightlife','Bars',
                     'Canadian (New)','Sandwiches','Breakfast & Brunch',
                     'Italian','Chinese','Cafes','Pizza',
                     'posterior','attributes.RestaurantsPriceRange2',
                     'Monday_duration_bucket','Monday_hours',
                     'Sunday_duration_bucket',
                     'attributes.GoodForMeal.latenight',
                     'attributes.GoodForMeal.lunch','attributes.GoodForMeal.breakfast','neighborhood',
                     'attributes.Ambience.casual','attributes.BikeParking','stars','latitude','longitude',
                     'attributes.RestaurantsDelivery'
                    ,'is_open')
                   ]

train$Food <- as.numeric(train$Food)
train$Nightlife <- as.numeric(train$Nightlife)
train$Bars <- as.numeric(train$Bars)
train[['Canadian (New)']] <- as.numeric(train[['Canadian (New)']])
train$Sandwiches <- as.numeric(train$Sandwiches)
train$Italian <- as.numeric(train$Italian)
train$Chinese <- as.numeric(train$Chinese)
train$Cafes <- as.numeric(train$Cafes)
train$Pizza <- as.numeric(train$Pizza)
train$attributes.GoodForMeal.latenight <- factor(train$attributes.GoodForMeal.latenight)
train$attributes.GoodForMeal.lunch <- factor(train$attributes.GoodForMeal.lunch)
train$attributes.GoodForMeal.breakfast <- factor(train$attributes.GoodForMeal.breakfast)
train$neighborhood <- as.factor(train$neighborhood)
train$attributes.Ambience.casual <- as.factor(train$attributes.Ambience.casual)
train$attributes.BikeParking <- as.factor(train$attributes.BikeParking)
train$attributes.RestaurantsDelivery <- as.factor(train$attributes.RestaurantsDelivery)




train$attributes.RestaurantsPriceRange2 <- factor(train$attributes.RestaurantsPriceRange2)

train$attributes.RestaurantsPriceRange2 <- factor(train$attributes.RestaurantsPriceRange2)
train$Monday_duration_bucket <- factor(train$Monday_duration_bucket)
train$Sunday_duration_bucket <- factor(train$Sunday_duration_bucket)


train$Monday_hours <- as.numeric(train$Monday_hours)

require(caTools)
set.seed(101) 
sample = sample.split(train$is_open, SplitRatio = .70)
train_d = subset(train, sample == TRUE)
test_d  = subset(train, sample == FALSE)

sapply(train, function(y) sum(length(which(is.na(y)))))


logitMod <- glm(is_open~
                +Food+Nightlife+Chinese+Cafes+Pizza+Italian+Sandwiches
                +Food*posterior+Nightlife*posterior+Bars*posterior+Italian*posterior+Chinese*posterior
                +Cafes*posterior + Pizza*posterior
                +Sandwiches*posterior
                +attributes.RestaurantsPriceRange2
                +posterior
                +Monday_duration_bucket
                +Sunday_duration_bucket
                +attributes.GoodForMeal.latenight
                +Nightlife*posterior*attributes.GoodForMeal.latenight
                +attributes.GoodForMeal.lunch
                +neighborhood*posterior
                ,data=train_d)
logitMod

# l = c('Food','Nightlife','Bars',
#                      'Canadian (New)','Sandwiches','Breakfast & Brunch',
#                      'Italian','Chinese','Cafes','Pizza',
#                      'posterior','attributes.RestaurantsPriceRange2',
#                      'Monday_duration_bucket','Monday_hours',
#                      'Sunday_duration_bucket',
#                      'attributes.GoodForMeal.latenight',
#                      'attributes.GoodForMeal.lunch','attributes.GoodForMeal.breakfast','neighborhood',
#                      'attributes.Ambience.casual','attributes.BikeParking','stars','latitude','longitude',
#                      'attributes.RestaurantsDelivery'
#                     ,'is_open')


# logitMod <- glm(is_open~
#                 +Food+Nightlife+Chinese+Cafes+Pizza
#                 +Food*stars+Nightlife*stars+Bars*posterior+Italian*stars+Chinese*stars
#                 +Cafes*stars + Pizza*stars+attributes.RestaurantsPriceRange2
#                 +stars
#                 +Monday_duration_bucket
#                 +attributes.GoodForMeal.latenight
#                 +Nightlife*stars*attributes.GoodForMeal.latenight
#                 +attributes.GoodForMeal.lunch
#                 +neighborhood*stars
#                 ,data=train)

library(caret)
# Use your model to make predictions, in this example newdata = training set, but replace with your test set   
logitMod$xlevels$neighborhood <- union(logitMod$xlevels$description, levels(train$neighborhood))
pdata <- predict(logitMod, newdata = test_d, type = "response",se.fit=FALSE)

# use caret and compute a confusion matrix
confusionMatrix(data = factor(as.numeric(pdata>0.5)), reference = factor(test_d$is_open))

res <- model.matrix(~attributes.RestaurantsPriceRange2, data = json_data_flattened_merged)

library(MCMCpack)

# bayesLogistic <- MCMClogit(is_open~
#                 +Food+Nightlife+Chinese+Cafes+Pizza+Italian+Sandwiches
#                 +Food*posterior+Nightlife*posterior+Bars*posterior+Italian*posterior+Chinese*posterior
#                 +Cafes*posterior + Pizza*posterior
#                 +Sandwiches*posterior
#                 +attributes.RestaurantsPriceRange2
#                 +posterior
#                 +Monday_duration_bucket
#                 +Sunday_duration_bucket
#                 +attributes.GoodForMeal.latenight
#                 +Nightlife*posterior*attributes.GoodForMeal.latenight
#                 +attributes.GoodForMeal.lunch
#                 +neighborhood*posterior
#                       , data=train, burnin = 10, mcmc=5000, thin = 1, tune = 0.5, beta.start = 0)

# plot(bayesLogistic)

library('glmnet')
library(glmnetUtils)



regularized <- glmnet(is_open~
                Food+Nightlife+Chinese+Cafes+Pizza+Italian+Sandwiches
                +Food*posterior+Nightlife*posterior+Bars*posterior+Italian*posterior+Chinese*posterior
                +Cafes*posterior + Pizza*posterior
                +Sandwiches*posterior
                +attributes.RestaurantsPriceRange2
                +posterior
                +Monday_duration_bucket
                +Sunday_duration_bucket
                +attributes.GoodForMeal.latenight
                +Nightlife*posterior*attributes.GoodForMeal.latenight
                +attributes.GoodForMeal.lunch
                +neighborhood*posterior
                ,data=train_d,family = "binomial",lambda=0.004)

# glmnet(train[,c('Nightlife','Food')],train$is_open)

# train[,c('posterior','Food')]

regularized

library(caret)
# Use your model to make predictions, in this example newdata = training set, but replace with your test set   
regularized$xlevels$neighborhood <- union(regularized$xlevels$description, levels(train$neighborhood))
pdata <- predict(regularized, newdata = test_d, type = "response",se.fit=FALSE)

# use caret and compute a confusion matrix
confusionMatrix(data = factor(as.numeric(pdata>0.5)), reference = factor(test_d$is_open))

coef(regularized)

plot(regularized, label = TRUE)

regularized_cv <- cv.glmnet(is_open~
                Food+Nightlife+Chinese+Cafes+Pizza+Italian+Sandwiches
                +Food*posterior+Nightlife*posterior+Bars*posterior+Italian*posterior+Chinese*posterior
                +Cafes*posterior + Pizza*posterior
                +Sandwiches*posterior
                +attributes.RestaurantsPriceRange2
                +posterior
                +Monday_duration_bucket
                +Sunday_duration_bucket
                +attributes.GoodForMeal.latenight
                +Nightlife*posterior*attributes.GoodForMeal.latenight
                +attributes.GoodForMeal.lunch
                +neighborhood*posterior
                ,train_d,family = "binomial")

plot(regularized_cv)

regularized


