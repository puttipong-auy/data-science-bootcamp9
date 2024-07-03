library(tidyverse)

library(caret)


##preview data
head(mtcars)


##recap Statistics
model <- lm( mpg ~ hp+wt ,data=mtcars)

##build standard interface for model training
model2 <- train(mpg ~ hp + wt,
      data = mtcars,
      method="lm")


##1. split data

train_test_split<- function(data,size=0.8){
  set.seed(24) 
  n <- nrow(data)
  train_id <- sample(1:n, size*n) ## สุ่มตัวเลข 1ถึงn จำนวน 0.8*nตัว
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ]
  return( list(train_df,test_df) )
}

prep_df <- train_test_split(mtcars,0.8)

##2. train model
#resampling boot LOOCV CV(k-fold)
ctrl <- trainControl(method = "boot",
                     number=25)

model <- train(mpg ~ hp+ wt + am,
               data=prep_df[[1]],
               method="lm",
               trcontrol = ctrl)

##3. score model

p_mpg <- predict(model , newdata=prep_df[[2]] )

##4. evaluate model

actual_mpg <- prep_df[[2]]$mpg 

##
test_mae <- mean(abs(p_mpg-actual_mpg))
test_rmse <- sqrt(mean((p_mpg-actual_mpg)**2))

print(test_mae)
print(test_rmse)

