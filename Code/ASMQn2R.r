
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

nrow(json_data_flattened)

# library('DataExplorer')
# plot_missing(json_data_flattened)
na_count <-sapply(json_data_flattened, function(y) sum(length(which(is.na(y)))))
# na_count[na_count<5000]

missing_counts_d = data.frame("col_name" = colnames(json_data_flattened),"missing_counts" = na_count)
p <- missing_counts_d[missing_counts_d$missing_counts>3000,'col_name']
json_data_flattened1 = json_data_flattened[ , !(names(json_data_flattened) %in% p)]
json_data_flattened1 = json_data_flattened1[ , !(names(json_data_flattened1) %in% c('name','address','city','state','postal_code'))]                  
names(json_data_flattened1)


table(json_data_flattened$attr,json_data_flattened$attributes.RestaurantsDelivery)

na_count[na_count<3000]

json_data_flattened[is.na(json_data_flattened$attributes.GoodForMeal.dessert),c('attributes.GoodForMeal.dessert','attributes.GoodForMeal.latenight')]

# missing_count_f = na_count[na_count<3000]
# # djson_data_flattened[,c(13:length(missing_count_f))]
# cols = missing_count_f[13:length(missing_count_f)]
# # json_data_flattened$stars = factor(json_data_flattened$stars)
# for (c in names(cols)){
#     print(ggplot(json_data_flattened) + geom_boxplot(aes_string
#                                 (y="stars",x=factor("attributes.RestaurantsAttire"),group=factor("is_open"))))
# }
# # json_data_flattened[['is_open']] = as.factor(json_data_flattened[['is_open']])

# boxplot(stars~attributes.RestaurantsDelivery*is_open, data=json_data_flattened, notch=TRUE,
#   col=(c("gold","darkgreen"))) 

ggplot(json_data_flattened, aes(y=as.numeric(stars), x=factor(is_open), fill=factor(attributes.RestaurantsDelivery))) +
  geom_boxplot()

ggplot(json_data_flattened, aes(y=as.numeric(stars), x=factor(is_open), fill=factor(attributes.RestaurantsReservations))) +
  geom_boxplot()

attributes.RestaurantsTakeOut
ggplot(json_data_flattened, aes(y=as.numeric(stars), x=factor(is_open), fill=factor(attributes.RestaurantsReservations))) +
  geom_boxplot()

missing_counts_d[missing_counts_d$missing_counts<3000,]['col_name']

q <- count(json_data_flattened, c('attributes.GoodForMeal.dessert','attributes.GoodForMeal.lunch  '))
q
json_data_flattened.data = table(json_data_flattened$attributes.GoodForMeal.dessert, json_data_flattened$attributes.GoodForMeal.lunch  ) 
# # print(json_data_flattened.data)

# # Perform the Chi-Square test.
print(chisq.test(json_data_flattened.data)$p.value)

attr = c('attributes.GoodForMeal.dessert','attributes.GoodForMeal.latenight','attributes.GoodForMeal.lunch','attributes.GoodForMeal.dinner','attributes.GoodForMeal.breakfast','attributes.GoodForMeal.breakfast','attributes.GoodForMeal.brunch')

for (i in attr){
    for (j in attr){
        json_data_flattened.data = table(json_data_flattened[[i]], json_data_flattened[[j]] ) 
#         print(json_data_flattened.data)
        if (chisq.test(json_data_flattened.data)$p.value<0.005)
        print(paste0(chisq.test(json_data_flattened.data)$p.value," ",i," ",j))
    }
}



attr = c('attributes.GoodForMeal.dessert','attributes.GoodForMeal.latenight','attributes.GoodForMeal.lunch','attributes.GoodForMeal.dinner','attributes.GoodForMeal.breakfast','attributes.GoodForMeal.breakfast','attributes.GoodForMeal.brunch')
for (i in attr){
        j = 'is_open'
#     for (j in attr){
        json_data_flattened.data = table(json_data_flattened[[i]], json_data_flattened[[j]] ) 
        print(json_data_flattened.data)
#         if (chisq.test(json_data_flattened.data)$p.value<0.005)
        print(paste0(chisq.test(json_data_flattened.data)$p.value," ",i," ",j))
        
#     }
}


table(json_data_flattened$Nightlife*json_data_flattened$attributes.GoodForMeal.latenight,json_data_flattened$is_open)


attr = c('attributes.BikeParking','attributes.Ambience.romantic','attributes.Ambience.intimate','attributes.Ambience.classy','attributes.Ambience.hipster','attributes.Ambience.touristy','attributes.Ambience.trendy','attributes.Ambience.upscale','attributes.Ambience.casual')
for (i in attr){
        j = 'is_open'
#     for (j in attr){
        json_data_flattened.data = table(json_data_flattened[[i]], json_data_flattened[[j]] ) 
        print(json_data_flattened.data)
#         if (chisq.test(json_data_flattened.data)$p.value<0.005)
        print(paste0(chisq.test(json_data_flattened.data)$p.value," ",i," ",j))
        
#     }
}



attr = c('Food','Nightlife','Bars','Canadian (New)','Sandwiches','Breakfast & Brunch','Italian','Chinese','Cafes','Pizza')
for (i in attr){
        j = 'is_open'
#     for (j in attr){
        json_data_flattened.data = table(json_data_flattened[[i]], json_data_flattened[[j]] ) 
        print(json_data_flattened.data)
#         if (chisq.test(json_data_flattened.data)$p.value<0.005)
        print(paste0(chisq.test(json_data_flattened.data)$p.value," ",i," ",j))
        
#     }
}

json_data_flattened$is_open = as.factor(json_data_flattened$is_open)
ggplot(json_data_flattened) + geom_boxplot(aes(is_open, stars, fill = is_open)) 
# + geom_jitter(aes(is_open, stars, shape = json_data_flattened$is_open))
ggsave("stars.png", width = 5, height = 2, dpi = 100)

xyplot(longitude~latitude,data=json_data_flattened,groups=is_open,auto.key=list(corner=c(1,1)))

###### There is no correlation between latitude, longitude and opened/closed restaurants

boxplot(stars~attributes.HasTV*is_open, data=json_data_flattened, notch=TRUE,
  col=(c("gold","darkgreen"))) 

gpb <- ggplot(json_data_flattened, aes(x=factor(stars), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge")

gpb <- ggplot(json_data_flattened, aes(x=factor(Food), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge")

gpb <- ggplot(json_data_flattened, aes(x=factor(Nightlife), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge")

for (i in colnames(json_data_flattened)[13:22]){
    print(i)
#     print(ggplot(json_data_flattened, aes(x=factor(json_data_flattened[[i]]), fill=factor(is_open))) + geom_bar(stat="count",position="dodge"))
#     plot(gpb1)
}

gpb <- ggplot(json_data_flattened, aes(x=factor(Pizza), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge")

# gpb <- ggplot(json_data_flattened, aes(x=factor(Food),y=stars, fill=factor(is_open)))
# gpb + geom_boxplot(outlier.colour="black", outlier.shape=16,
#              outlier.size=2, notch=FALSE)

json_data_flattened_Nightlife <- json_data_flattened[json_data_flattened$Sandwiches==1,]
ggplot(json_data_flattened_Nightlife, aes(x = stars,fill=as.factor(is_open))) + geom_density(bw=0.3, alpha = 0.4)
# scale_fill_manual(values=c("red","blue"))

json_data_flattened.data = table(json_data_flattened$Food, json_data_flattened$Chinese) 
print(json_data_flattened.data)

# Perform the Chi-Square test.
print(chisq.test(json_data_flattened.data))

json_data_flattened.data = table(json_data_flattened$Food, json_data_flattened$Cafes) 
print(json_data_flattened.data)

# Perform the Chi-Square test.
print(chisq.test(json_data_flattened.data))

json_data_flattened.data = table(json_data_flattened$attributes.GoodForMeal.latenight, json_data_flattened$attributes.GoodForMeal.dinner) 
print(json_data_flattened.data)

# Perform the Chi-Square test.
print(chisq.test(json_data_flattened.data))

library(plyr)

q <- count(json_data_flattened, c('neighborhood','is_open'))
q

library(forcats)
library(ggplot2)
gpb <- ggplot(json_data_flattened, aes(x=factor(neighborhood), fill=factor(is_open)))
gpb + geom_bar(stat="count",position="dodge") + coord_flip ()

json_data_flattened.data = table(json_data_flattened$neighborhood, json_data_flattened$is_open) 
# print(json_data_flattened.data)

# Perform the Chi-Square test.
print(chisq.test(json_data_flattened.data))

library(forcats)

# is_open
#     0
# Food
#     0
# Nightlife
#     0
# Bars
#     0
# Canadian (New)
#     0
# Sandwiches
#     0
# Breakfast & Brunch
#     0
# Italian
#     0
# Chinese
#     0
# Cafes
#     0
# Pizza
#     0

json_data_flattened$is_open = as.factor(json_data_flattened$is_open)
ggplot(json_data_flattened) + geom_boxplot(aes(is_open, stars, fill = is_open)) 
# + geom_jitter(aes(is_open, stars, shape = json_data_flattened$is_open))

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

fit <- compare_2_gibbs(json_data_flattened$stars, as.factor(json_data_flattened$is_open))

plot(as.mcmc(fit))

y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))
sum(y1_sim>y2_sim)/5000

col_names <- colnames(json_data_flattened)
for (i in 1:length(col_names)){
    print(paste0(i," ",col_names[i]))
}

for (i in 23:95){
    gpb <- ggplot(json_data_flattened, aes_string(x=col_names[i], fill="is_open"))
    print(gpb + geom_bar(stat="count",position="dodge"))
    
}

col_names[1]

library(purrr)

library(tidyr)
json_data_flattened %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()

library('missForest')

considered_cols = missing_counts_d[missing_counts_d$missing_counts<3000,]

considered_cols1 = considered_cols[considered_cols$col_name!='business_id' & considered_cols$col_name!='address'
                & considered_cols$col_name!='postal_code'
                ,]



library(missForest)
iris.imp <- missForest(json_data_flattened[,considered_cols1$col_name])

iris.mis

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

reviews <- stream_in(file("/home/vignesh/Documents/ASM/Review_Toronto_Restaurant.json"))


length(unique(reviews$business_id))

q <- count(json_data_flattened, c('business_id','stars'))


q[q$freq>1,]

library(plyr)
q <- ddply(reviews, .(business_id), nrow)
q$V1 <- as.numeric(q$V1)

q[q$V1==2,]

# compare_m_gibbs <- function(y, ind, maxiter = 10)
# {
# # print("hello")
# ### weakly informative priors
#     print("hello")
# a0 <- 2 ; b0 <- 2*0.67*0.67 ## tau_w hyperparameters
# eta0 <- 2 ; t0 <- 2*0.67*0.67 ## tau_b hyperparameters
# mu0<-3 ; gamma0 <- 1/(0.67*0.67)
# ###

# ### starting values
# m <- nlevels(ind)
# ybar <- theta <- tapply(y, ind, mean)
# temp <- tapply(reviews$stars, reviews$index, var)
# temp[is.na(temp)] <- 0
# tau_w <- 1/mean(temp)
# # tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
# mu <- mean(theta)
# tau_b <-var(theta) ##between group precision
# n_m <- tapply(y, ind, length)
# an <- a0 + (sum(n_m)/2)
# ###

# ### setup MCMC
# theta_mat <- matrix(0, nrow=maxiter, ncol=m)
# mat_store <- matrix(0, nrow=maxiter, ncol=3)
# ###

# ### MCMC algorithm
# for(s in 1:maxiter) 
# {
  
#   # sample new values of the thetas
#   for(j in 1:m) 
#   {
#     taun <- n_m[j] * tau_w + tau_b
#     thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
#     theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
#   }
  
#   #sample new value of tau_w
#   ss <- 0
#   for(j in 1:m){
# #     print(paste0(ind," ",j))
#     ss <- ss + sum((y[ind == j] - theta[j])^2)
#   }
#   bn <- b0 + ss/2
#   tau_w <- rgamma(1, an, bn)
#   print(paste0(an," ",bn," ",ss))
#   print(tau_w)  
  
#   #sample a new value of mu
#   gammam <- m * tau_b + gamma0
#   mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
#   mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
  
#   # sample a new value of tau_b
#   etam <- eta0 + m/2
#   tm <- t0 + sum((theta-mu)^2)/2
#   tau_b <- rgamma(1, etam, tm)
  
#   #store results
#   theta_mat[s,] <- theta
#   mat_store[s, ] <- c(mu, tau_w, tau_b)
# }
# colnames(mat_store) <- c("mu", "tau_w", "tau_b")
# return(list("params" = mat_store, "theta_mat" = theta_mat))
# }

gibbs_normal <- function(y, mu0, tau0, a0, b0, maxiter = 5000){
  ## store samples
  phi.store <- matrix(0, nrow = maxiter, ncol = 2) 
  
  ## use this repeatedly in calculations
  ybar <- mean(y) 
  n <- length(y)
  a.n <- n / 2 + a0
  
  mu.new <- ybar ## initial value
  
    for(s in 1:maxiter){
    
    ## sample tau
    b.n <- b0 + 0.5 * sum( (y - mu.new)^2 )
    tau.new <- rgamma(1, a.n, b.n)
    
    ## sample mu
    tau.n <- n * tau.new + tau0
    mu.n <- (n * tau.new * ybar + tau0 * mu0) / tau.n
    
    mu.new <- rnorm(1,  mu.n, 1 / tau.n)
    
    ## store values
    phi.store[s, ] <- c(mu.new, tau.new)
    }
  colnames(phi.store) <- c("mean", "precision")
  return(params = phi.store)
}

reviews$business_id_1 <- as.numeric( factor(reviews$business_id) )
reviews$index <- as.factor(as.numeric(as.factor(reviews$business_id)))
# fit2 <- compare_m_gibbs(reviews$stars,reviews$business_id_1)

mean(reviews[reviews$business_id=='_3IE9guLHvAr-kJpH4Zd7Q','stars'])

fit.gibbs <- gibbs_normal(reviews[reviews$index==1,'stars'], mu0 = 3, tau0 = 1/(0.67*0.67), a0 = 2, b0 = 2*0.67*0.67)

library(MCMCpack)

gibbs_normal <- function(y, mu0, tau0, a0, b0, maxiter = 5000){
  ## store samples
  phi.store <- matrix(0, nrow = maxiter, ncol = 2) 
  
  ## use this repeatedly in calculations
  ybar <- mean(y) 
  n <- length(y)
  a.n <- n / 2 + a0
  
  mu.new <- ybar ## initial value
  
    for(s in 1:maxiter){
    
    ## sample tau
    b.n <- b0 + 0.5 * sum( (y - mu.new)^2 )
    tau.new <- rgamma(1, a.n, b.n)
    
    ## sample mu
    tau.n <- n * tau.new + tau0
    mu.n <- (n * tau.new * ybar + tau0 * mu0) / tau.n
    
    mu.new <- rnorm(1,  mu.n, 1 / tau.n)
    
    ## store values
    phi.store[s, ] <- c(mu.new, tau.new)
    }
  colnames(phi.store) <- c("mean", "precision")
  return(params = phi.store)
}


plot(as.mcmc(fit.gibbs))

d <- density(rnorm(5000,mean=fit.gibbs[,1],sd=1/sqrt(fit.gibbs[,2])))
plot(d)
mean(rnorm(5000,mean=fit.gibbs[,1],sd=1/sqrt(fit.gibbs[,2])))

reviews$business_id = factor(reviews$business_id)
business_id = levels(reviews$business_id)
sample_means = c()
counts = c()
expected_means = c()
for (bi in business_id){
#     print(bi)
    filter <- reviews[reviews$business_id==bi,'stars']
    fit.gibbs <- gibbs_normal(filter, mu0 = 3, tau0 = 1/(0.67*0.67), a0 = 2, b0 = 2*0.67*0.67)
#     print(paste0(length(reviews[reviews$business_id==bi,'stars'])," ",mean(rnorm(5000,mean=fit.gibbs[,1],sd=1/sqrt(fit.gibbs[,2])))," ",mean(reviews[reviews$business_id==bi,'stars'])))
    sample_means = c(sample_means,mean(filter))
    counts = c(counts,length(filter))
    expected_means = c(expected_means,mean(rnorm(5000,fit.gibbs[,1],1/sqrt(fit.gibbs[,2]))))
}

reviews[reviews$business_id=='__8j8yhsmE98wNWHJNyAgw','stars']

review_data <- data.frame("business_id" = business_id, "expected_means" = expected_means, "sample_means" = sample_means, "counts" = counts)

review_data

write.csv(review_data,file='Review_posterior.csv')

review_data$business_id = factor(review_data$business_id)
m = merge(json_data_flattened, review_data, by="business_id")

m$is_open = as.factor(m$is_open)
ggplot(m) + geom_boxplot(aes(is_open, expected_means, fill = is_open)) 
# + geom_jitter(aes(is_open, stars, shape = json_data_flattened$is_open))

m$expected_means

json_data_flattened$business_id

levels(review_data$business_id)


