
library(mclust)
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

cat_names_sort <- sort(table(cat_total), decreasing = TRUE)
cat_names <- names(cat_names_sort)[2:length(cat_names_sort)] ## 1 is Restaurants - we don't need this

cat_bus_ind_mat <- sapply(json_file$categories, function(y) as.numeric(cat_names %in% y))
cat_bus_ind_mat <- t(cat_bus_ind_mat)
colnames(cat_bus_ind_mat) <- cat_names
df_TO_tidy_cat <- cbind(json_file, cat_bus_ind_mat)
json_data_flattened <- flatten(df_TO_tidy_cat)

c = 0
for (i in names(json_data_flattened)){
    c = c+1
    print(paste0(c," ",i))
}



lat_long = json_file[,c('latitude','longitude')]

fit <- Mclust(lat_long)
plot(fit, what = "BIC")

fit$BIC

fit2 <- Mclust(lat_long, G = 7, modelNames = "VVV")
plot(fit2, what = "classification")

mean(fit2$uncertainty)

fit2 <- Mclust(lat_long, G = 7, modelNames = "VVV")
plot(fit2, what = "uncertainty")

fit2 <- Mclust(lat_long, G = 8, modelNames = "VVV")
plot(fit2, what = "classification")
mean(fit2$uncertainty)



fit2 <- Mclust(lat_long, G = 9, modelNames = "VVV")
plot(fit2, what = "classification")
mean(fit2$uncertainty)

fit2 <- Mclust(lat_long, G = 8, modelNames = "VVV")
plot(fit2, what = "uncertainty")

fit2 <- Mclust(lat_long, G = 9, modelNames = "VVV")
plot(fit2, what = "uncertainty")

fit <- Mclust(lat_long,G=1:20)
plot(fit, what = "BIC")

fit$BIC

fit <- Mclust(lat_long,G=1:40)
plot(fit, what = "BIC")



fit2 <- Mclust(lat_long, G = 27, modelNames = "VVV")
plot(fit2, what = "uncertainty")

fit <- Mclust(lat_long,G=1:50)
plot(fit, what = "BIC")

fit$BIC

fit2 <- Mclust(lat_long, G = 34, modelNames = "VVV")
plot(fit2, what = "uncertainty")

uncerPlot(z = fit2$z)

fit2 <- Mclust(lat_long, G = 33, modelNames = "VVV")
plot(fit2, what = "uncertainty")
uncerPlot(z = fit2$z)

fit2 <- Mclust(lat_long, G = 33, modelNames = "VVV")
plot(fit2, what = "classification")
uncerPlot(z = fit2$z)

fit2 <- Mclust(lat_long, G = 32, modelNames = "VVV")
plot(fit2, what = "uncertainty")
uncerPlot(z = fit2$z)

json_data_flattened$clusters <- fit2$classification

# json_data_flattened <- flatten(json_file)
# json_file$clusters

names(json_data_flattened)

# (json_data_flattened[json_data_flattened$clusters==2,c('attributes.RestaurantsAttire','hours.Friday','hours.Tuesday')])

ggplot(json_data_flattened, aes(y=as.numeric(stars), x=factor(clusters))) +
  geom_boxplot()

temp <- json_data_flattened[,c('stars','clusters','neighborhood')]
temp$clusters <- fit2$classification

summary(factor(temp$clusters))

json_data_flattened$clusters <- fit2$classification
temp1 <- json_data_flattened[json_data_flattened$clusters==1,]

# for (i in names(temp1)[335:337]){
#     print(paste0(i," ",sum(temp1[[i]])))
# }

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

# temp <- json_data_flattened[,c(15:336)]
# temp$clusters <- fit2$classification
# arr = c()
# for (i in names(temp)){
    
# #     temp[[i]] = as.numeric(temp[[i]])
#     arr = c(arr,gsub(" ", "_", gsub('[[:punct:] ]+',' ',i)))
# }
# # arr
# colnames(temp) <- arr
temp$clusters <- factor(temp$clusters)
model = multinom(clusters~neighborhood,data = json_data_flattened,MaxNWts = 845544)


library(caret)
# confusionMatrix(data = predict(model,json_data_flattened), reference = json_data_flattened$clusters)
tab = table(predict(model,json_data_flattened),json_data_flattened$clusters)
tab+

 cm = as.matrix(tab)

 n = sum(cm) # number of instances
 nc = nrow(cm) # number of classes
 diag = diag(cm) # number of correctly classified instances per class 
 rowsums = apply(cm, 1, sum) # number of instances per class
 colsums = apply(cm, 2, sum) # number of predictions per class
 p = rowsums / n # distribution of instances over the actual classes
 q = colsums / n # distribution of instances over the predicted classes

 precision = diag / colsums 
 recall = diag / rowsums 
 f1 = 2 * precision * recall / (precision + recall) 

 data.frame(precision, recall, f1) 

sum(diag) / n 

library(rpart)
json_data_flattened[is.na(json_data_flattened$neighborhood),'neighborhood'] <- "don't know"
json_data_flattened$neighborhood = as.factor(json_data_flattened$neighborhood)

dt <- rpart(factor(clusters)~neighborhood,data=json_data_flattened)

dt

# temp <- json_data_flattened[,c(15:336)]
# temp$clusters <- fit2$classification
# arr = c()
# for (i in names(temp)){
    
# #     temp[[i]] = as.numeric(temp[[i]])
#     arr = c(arr,gsub(" ", "_", gsub('[[:punct:] ]+',' ',i)))
# }
# # arr
# colnames(temp) <- arr
# temp$clusters <- factor(temp$clusters)
# model = multinom(clusters~.,data = temp,MaxNWts = 845544)

json_data_flattened$clusters <- fit2$classification
s <- count(json_data_flattened[,c('Italian','is_open')], c('Italian','is_open'))
s

json_data_flattened$clusters <- fit2$classification
s <- count(json_data_flattened[,c('Indian','is_open')], c('Indian','is_open'))
s

json_data_flattened$clusters <- fit2$classification
s <- count(json_data_flattened[,c('Pizza','clusters')], c('Pizza','clusters'))
s


json_data_flattened[json_data_flattened$clusters==2,c('categories')]

model <- multinom(clusters~factor(neighborhood),data=temp,MaxNWts = 845544)

predict(model,temp)

library(caret)
# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
pdata <- predict(model, newdata = temp, type = "class")

# use caret and compute a confusion matrix
confusionMatrix(data = factor(pdata), reference = factor(temp$clusters))


