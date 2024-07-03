library(tidyverse)
library(caret)
library(mlbench)

data()
data("BostonHousing")
data("PimaIndiansDiabetes")


##review train
clean_df <- mtcars %>%
  #select(mpg,hp,wt,am)%>%
  #mean imputation
  mutate(hp=replace_na(hp,146.68),
         wt=replace_na(wt,3.21))%>%
  drop_na()


  
##linear regression
(lm_model <- train(mpg ~ .,
                  data=clean_df,
                  method="lm"))

##KNN
(knn_model <- train(mpg ~ .,
                  data=clean_df,
                  method="knn",
                  metric="RMSE",))

## decision tree
(de_model <- train(mpg ~ .,
                    data=clean_df,
                    method="rpart",
                    metric="RMSE"))

set.seed(42)

##grid search(dataframe)
k_grid <- data.frame(k=c(1:9))


ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE ## progress bar
)
##repeates cv
ctrl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats=5,
  verboseIter = TRUE ## progress bar
)


##KNN
(knn_model <- train(mpg ~ .,
                    data=clean_df,
                    method="knn",
                    metric="RMSE",
                    trControl= ctrl,
                    tuneGrid = k_grid
                    ))
##random K 4 values
(knn_model <- train(mpg ~ .,
                    data=clean_df,
                    method="knn",
                    metric="RMSE",
                    trControl= ctrl,
                    tuneLength = 4
))


##save model
## .RDS extension
saveRDS(knn_model,"knnModel.RDS")


## normalization
## 1. min-max(feature scaling 0-1)
## 2. standardization -3,+3

x <- c(5,10,12,15,20)

minmaxNorm <- function(x){
  (x-min(x)) / (max(x)-min(x))
}

## center and scale
## standardization

zNorm <- function(x){
  (x-mean(x)) / sd(x)
}

## preProcess()
train_df <- mtcars[1:20, ]
test_df <- mtcars[21:32, ]

##compute X_bar, x_sd
tranformer <- preProcess(train_df,
                         method=c("center","scale"))

##compute min max scaling[0,1]
tranformer <- preProcess(train_df,
                         method=c("range"))

train_df_z <- predict(tranformer,train_df)
test_df_z <- predict(tranformer,test_df)













