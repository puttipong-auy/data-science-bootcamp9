## correlation

cor(mtcars$mpg,mtcars$hp)
cor(mtcars$mpg,mtcars$wt)

plot(mtcars$hp,mtcars$mpg,pch=16)
plot(mtcars$wt,mtcars$mpg,pch=16)
plot(mtcars$wt,mtcars$hp,pch=16)

cor(mtcars[,c("mpg","wt","hp")])

##dplyr (tidyverse)
library(dplyr)
cor_mat <-mtcars %>%
  select(mpg,wt,hp) %>%
  cor()

cor_mat

##compute correlation (r) and sig test
cor(mtcars$hp,mtcars$mpg)
cor.test(mtcars$hp,mtcars$mpg)


##Linear Regression
## mpg=f(hp)
lmFit <- lm(mpg ~ hp, data=mtcars)
lmFit

summary(lmFit)

lmFit$coefficients


##prediction
lmFit$coefficients[[1]]+lmFit$coefficients[[2]]*200

newcars <- data.frame(
  hp=c(250,320,300,410,450)
)

##predict()
newcars$mpg_pred<-predict(lmFit,newdata=newcars)
newcars$hp_pred<- NULL
newcars
## hp 450 prdict mpg -> - because data don't have high hp data for train
summary(mtcars$hp)

##Root Mean Squred Error (rmse)
## multiple linear regression
## mpg=f(hp,wt,am)
## mpg= interception + b0*hp+ b1*wt + b2*am

lmFit_v2<- lm(mpg~ hp+wt+am,data=mtcars)
lmFit_v2

coefs<- coef(lmFit_v2)
coefs[[1]] + coefs[[2]]*200 +coefs[[3]]*3.5 + coefs[[4]]*1

## Build Full Model 
lmFit_Full <- lm(mpg ~ .,data=mtcars)

mtcars$predicted <- predict(lmFit_Full)
head(mtcars)

##RMSE
squared_error<- (mtcars$mpg - mtcars$predicted)**2
(rmse<-sqrt(mean(squared_error)))



## Split data
set.seed(42)
n<-nrow(mtcars)
id<-sample(1:n,size=n*0.8)
train_data<- mtcars[id, ]
test_data<- mtcars[-id, ]

## train model
model1 <- lm(mpg ~ hp+wt,data=train_data)
p_train <- predict(model1)
rmse_train<- sqrt(mean( (train_data$mpg-p_train)**2 ))

## test model
p_test<-predict(model1,newdata=test_data)
rmse_test<- sqrt(mean( (test_data$mpg-p_test)**2 ))

## Print Result
cat("RMSE TRAIN:",rmse_train,"\nRMSE TEST:",rmse_test)













