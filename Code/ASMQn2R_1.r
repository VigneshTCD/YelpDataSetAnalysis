

library(jsonlite)
library(curl)
library(ggplot2)
# install.packages("devtools")
library(devtools)
library(lattice)
library(plyr)

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

json_data_flattened$attributes.RestaurantsPriceRange2



review_data <- read.csv('Review_posterior.csv')

review_data$business_id = factor(review_data$business_id)
m = merge(json_data_flattened, review_data, by="business_id",all.x=TRUE)

m$is_open = as.factor(m$is_open)
ggplot(m) + geom_boxplot(aes(is_open, expected_means, fill = is_open)) + labs(y = "Posterior mean rating")
# + geom_jitter(aes(is_open, stars, shape = json_data_flattened$is_open))

m$is_open = as.factor(m$is_open)
m$attributes.RestaurantsPriceRange2 = as.numeric(m$attributes.RestaurantsPriceRange2)
ggplot(m) + geom_boxplot(aes(is_open, attributes.RestaurantsPriceRange2, fill = is_open)) 
# + geom_jitter(aes(is_open, stars, shape = json_data_flattened$is_open))

m

compare_2_gibbs <- function(y, ind, mu0 = 3, tau0 = 1/(0.67*0.67), del0 = 0, gamma0 = 1/(0.67*0.67), a0 = 2, b0 = 2*0.67*0.67, maxiter = 5000)
{
y1 <- y[ind == 1]
y2 <- y[ind == 0]

n1 <- length(y1) 
n2 <- length(y2)

##### starting values
mu <- (mean(y1) + mean(y2)) / 2
del <- (mean(y1) - mean(y2)) / 2

mat_store <- matrix(0, nrow = maxiter, ncol = 3)
#####

##### Gibbs sampler
an <- a0 + (n1 + n2)/2

for(s in 1 : maxiter) 
{
  
  ##update tau
  bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
  tau <- rgamma(1, an, bn)
  ##
  
  ##update mu
  taun <-  tau0 + tau * (n1 + n2)
  mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
  mu <- rnorm(1, mun, sqrt(1/taun))
  ##
  
  ##update del
  gamman <-  gamma0 + tau*(n1 + n2)
  deln <- ( del0 * gamma0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
  del<-rnorm(1, deln, sqrt(1/gamman))
  ##
  
  ## store parameter values
  mat_store[s, ] <- c(mu, del, tau)
}
colnames(mat_store) <- c("mu", "del", "tau")
return(mat_store)
}

library(MCMCpack)

m1 <- m[!is.na(m$expected_means),]
fit <- compare_2_gibbs(m1$expected_means, as.factor(m1$is_open))

plot(as.mcmc(fit))

y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))
sum(y1_sim>y2_sim)/5000

Food
    0
Nightlife
    0
Bars
    0
Canadian (New)
    0
Sandwiches
    0
Breakfast & Brunch
    0
Italian
    0
Chinese
    0
Cafes
    0
Pizza



ggplot(m, aes(y=as.numeric(expected_means), x=factor(Food), fill=factor(is_open))) +
  geom_boxplot() + ylab("posterior mean rating") + xlab("Food")
count(m, c('Food','is_open'))
aggregate(expected_means~Food+is_open,m,median)

ggplot(m, aes(y=as.numeric(expected_means), x=factor(Nightlife), fill=factor(is_open))) +
  geom_boxplot() + ylab("posterior mean rating") + xlab("Nightlife")
# count(m, c('Food','is_open'))
ggsave("Night.png", width = 8, height = 5, dpi = 100)

ggplot(m, aes(y=as.numeric(expected_means), x=factor(attributes.RestaurantsReservations), fill=factor(is_open))) +
  geom_boxplot()
count(m, c('RestaurantsGoodForGroups','is_open'))
aggregate(expected_means~RestaurantsGoodForGroups+is_open,m,median)

ggplot(m, aes(y=as.numeric(expected_means), x=factor(Italian), fill=factor(is_open))) +
  geom_boxplot()
count(m, c('Italian','is_open'))
aggregate(expected_means~Italian+is_open,m,median)
ggsave("Italian.png", width = 8, height = 5, dpi = 100)

ggplot(m, aes(y=as.numeric(expected_means), x=factor(attributes.RestaurantsPriceRange2), fill=factor(is_open))) +
  geom_boxplot() + ylab('posterior mean rating') + xlab('Restaurant Price Range')
count(m, c('attributes.RestaurantsPriceRange2','is_open'))
aggregate(expected_means~attributes.RestaurantsPriceRange2+is_open,m,median)
ggsave("RestaurantPriceRange.png", width = 8, height = 5, dpi = 100)

Downtown = m[m$neighborhood=='Downtown Core',]

count(Downtown, c('is_open'))


ggplot(Downtown, aes(y=as.numeric(expected_means), x=factor(Sandwiches), fill=factor(is_open))) +
  geom_boxplot()
count(Downtown, c('Sandwiches','is_open'))
aggregate(expected_means~Sandwiches+is_open,Downtown,median)

gpb <- ggplot(json_data_flattened, aes(x=factor(attributes.RestaurantsAttire), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge")

library(dplyr)
p = count(json_data_flattened, c('attributes.RestaurantsPriceRange2','attributes.RestaurantsDelivery','is_open'))
basic_summ_t2 = dcast(p, RestaurantsPriceRange2 ~ attributes.RestaurantsDelivery, value.var = "freq")
basic_summ_t2

library(dplyr)
library(reshape2)
dcast(p, attributes.RestaurantsPriceRange2 ~ attributes.RestaurantsDelivery+is_open, value.var = "freq",fun.aggregate=sum)

count(m, c('attributes.RestaurantsPriceRange2','attributes.RestaurantsGoodForGroups','is_open'))



# p = count(json_data_flattened, c('attributes.RestaurantsPriceRange2','attributes.RestaurantsAttire','is_open'))
# p$attributes.RestaurantsAttire = as.factor(p$attributes.RestaurantsAttire)
# p$attributes.RestaurantsPriceRange2 = as.factor(p$attributes.RestaurantsPriceRange2)
# p$is_open = as.factor(p$is_open)

# p[ordered(c(p$attributes.RestaurantsPriceRange2,p$attributes.RestaurantsAttire)),]
# p
# p

p

sort(unique(m$hours.Friday))

library(lubridate)
Time <- factor("11:01")


# parese date
a <- hms(as.character(Time))

# get hours
hour(a)

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
        else{
            duration = c(duration,"evening 4pm-8pm")
        }
    }
#     hour
    duration
}
# apply(unique(m$hours.Friday))

time_split <- data.frame(do.call(rbind, strsplit(as.vector((m$hours.Friday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
m$Friday_opening =  hours$X1
m$Friday_closing =  hours$X2
m$Friday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
m$Friday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

time_split$X1

# m[,c('Friday_opening','Friday_closing','Friday_hours')]
# max(m[!is.na(m$Friday_opening),c('Friday_opening')])
# m$Friday_opening = as.factor(m$Friday_opening)
m[is.na(m)] = -1
# m[m$Friday_opening==1,c('Friday_opening','Friday_closing','Friday_hours')]
m[m$Friday_opening>19,c('hours.Friday')]

# apply(time_split,2,getDurationBucket)                                 

gpb <- ggplot(m, aes(x=factor(Friday_duration_bucket), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge")

ggplot(m, aes(y=as.numeric(Friday_hours), x = as.factor(Friday_opening),fill=factor(is_open))) +
  geom_boxplot() + coord_flip()

gpb <- ggplot(m, aes(x=factor(Friday_opening), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge") + coord_flip()

ggplot(m, aes(y=as.numeric(expected_means), x = as.factor(neighborhood),fill=factor(is_open))) +
  geom_boxplot() + coord_flip()
ggsave("Neighborhood.png", width = 8, height = 10, dpi = 100) + ylab("posterior mean rating") + xlab("neighborhood")

time_split <- data.frame(do.call(rbind, strsplit(as.vector((m$hours.Monday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
m$Monday_opening =  hours$X1
m$Monday_closing =  hours$X2
m$Monday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
m$Monday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

ggplot(m, aes(y=as.numeric(Monday_hours), x = as.factor(Monday_opening),fill=factor(is_open))) +
  geom_boxplot() + coord_flip() + ylab('Monday opening time') + xlab('Monday opening hours')
ggsave("Monday.png", width = 8, height = 5, dpi = 100)

ggplot(m, aes(y=as.numeric(expected_means), x = as.factor(Friday_opening),fill=factor(is_open))) +
  geom_boxplot() + coord_flip()

m[,c('hours.Friday','hours.Monday')]

m2 <- m[!is.na(m$Wednesday_hours),]
fit_1 <- compare_2_gibbs(m2$Wednesday_hours, m2$is_open, mu0 = 10, tau0 = 1/(0.33*0.33), del0 = 0, gamma0 = 1/(0.33*0.33), a0 = 2, b0 = 2*0.33*0.33, maxiter = 5000)
plot(as.mcmc(fit_1))

time_split <- data.frame(do.call(rbind, strsplit(as.vector((m$hours.Tuesday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
m$Tuesday_opening =  hours$X1
m$Tuesday_closing =  hours$X2
m$Tuesday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
m$Tuesday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

ggplot(m, aes(y=as.numeric(Tuesday_hours), x = as.factor(Tuesday_opening),fill=factor(is_open))) +
  geom_boxplot() + coord_flip()



time_split <- data.frame(do.call(rbind, strsplit(as.vector((m$hours.Wednesday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
m$Wednesday_opening =  hours$X1
m$Wednesday_closing =  hours$X2
m$Wednesday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
m$Wednesday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

ggplot(m, aes(y=as.numeric(Wednesday_hours), x = as.factor(Wednesday_opening),fill=factor(is_open))) +
  geom_boxplot() + coord_flip()

time_split <- data.frame(do.call(rbind, strsplit(as.vector((m$hours.Thursday)), split = "-")))
hours <-data.frame((apply(time_split[1:2],2,getHours)))
m$Thursday_opening =  hours$X1
m$Thursday_closing =  hours$X2
m$Thursday_hours = abs(as.numeric(hours$X1)-as.numeric(hours$X2))
m$Thursday_duration_bucket = data.frame(apply(time_split,2,getDurationBucket))$X1

ggplot(m, aes(y=as.numeric(Thursday_hours), x = as.factor(Thursday_opening),fill=factor(is_open))) +
  geom_boxplot() + coord_flip()

ggplot(m, aes(y=as.numeric(Thursday_hours), x = as.factor(is_open))) +
  geom_boxplot() 

strsplit(as.vector(unique(m$hours.Friday)), split = "-")

dat1 <- data.frame(do.call(rbind, strsplit(as.vector(unique(m$hours.Friday)), split = "-")))

s <- data.frame((apply(dat1[1:2],2,getHours)))


abs(s$X2-s$X1)

library(missForest)
data(iris)
summary(iris)

## The data contains four continuous and one categorical variable.

## Artificially produce missing values using the 'prodNA' function:
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)

## Impute missing values providing the complete matrix for
## illustration. Use 'verbose' to see what happens between iterations:
iris.imp <- missForest(iris.mis, xtrue = iris, verbose = TRUE)

## The imputation is finished after five iterations having a final
## true NRMSE of 0.143 and a PFC of 0.036. The estimated final NRMSE
## is 0.157 and the PFC is 0.025 (see Details for the reason taking
## iteration 4 instead of iteration 5 as final value).

## The final results can be accessed directly. The estimated error:
iris.imp$OOBerror

## The true imputation error (if available):
iris.imp$error

na_count <-sapply(json_data_flattened, function(y) sum(length(which(is.na(y)))))

na_count_less_than_3000 <- na_count[na_count<3000]

na_count_less_than_3000[23:length(na_count_less_than_3000)]

# json_data_flattened[,23:length(na_count_less_than_3000)]
json_data_flattened$hours.Friday = as.factor(json_data_flattened$hours.Friday)
json_data_flattened_summ <- missForest(data.frame(as.factor(json_data_flattened$hours.Sunday)), verbose = TRUE)


json_data_flattened[,23:24]


