## Logistic Regression
library(dplyr)
data(mtcars)
mtcars %>% head()

str(mtcars)


##convert am to factor
mtcars$am <- factor(mtcars$am,
                    levels=c(0,1),
                    labels=c("Auto","Manual"))

class(mtcars$am)
table(mtcars$am)

## Split data
set.seed(42)
n<-nrow(mtcars)
id<-sample(1:n,size=n*0.8)
train_data<- mtcars[id, ]
test_data<- mtcars[-id, ]

##Train model
logit_model <- glm(am ~ mpg,data=train_data,family="binomial")
p_train <- predict(logit_model,type="response")  ## probability
train_data$pred <- if_else(p_train>=0.5,"Manual","Auto")
mean(train_data$am == train_data$pred)

##test model
p_test <- predict(logit_model,newdata = test_data,type="response")  ## probability
test_data$pred <- if_else(p_test>=0.5,"Manual","Auto")

mean(test_data$am == test_data$pred)

















