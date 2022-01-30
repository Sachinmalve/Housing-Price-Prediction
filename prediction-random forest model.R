melb_data <- read.csv("D:/Data Set/melb_data.csv")
View(melb_data)

summary (melb_data)

names(melb_data)

library(rpart)
library(randomForest)
library(tidyverse)
fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea + YearBuilt + Lattitude + Longtitude,data = melb_data)  

plot(fit,uniform=TRUE)
text(fit,cex=0.6)

head(melb_data)

predict(fit,head(melb_data))

print (head(melb_data $ Price ))

library(modelr)

mae(model=fit , data =melb_data)

splitData <- resample_partition(melb_data,c(test=0.3,train=0.7))

sapply(splitData,dim)

fit2 <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea + YearBuilt + Lattitude + Longtitude, data = splitData$train)

mae(model=fit2 , data=melb_data)
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  predictors <- paste(predictors, collapse="+")
  formula <- as.formula(paste(target,"~",predictors,sep = ""))
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  
  mae <- mae(model, testing_data)
  return(mae)
}
target <- "Price"
predictors <-  c("Rooms","Bathroom","Landsize","BuildingArea",
                 "YearBuilt","Lattitude","Longtitude")  
for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\t MAE: ",mae))
}
fit3 <- randomForest(Price ~ Rooms + Bathroom + Landsize + BuildingArea + YearBuilt + Lattitude
                     + Longtitude, data = splitData$train,na.action = na.exclude)

#Calculating the MAE after fitting a model using Random Forest 
mae(model=fit3,data = splitData$test)

predict(fit3, head(melb_data))
print (head(melb_data $ Price ))
