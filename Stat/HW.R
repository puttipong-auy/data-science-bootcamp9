library(titanic)

head(titanic_train)

## drop na
titanic_train <- na.omit(titanic_train)
nrow(titanic_train)

## split data
set.seed(24)
n <- nrow(titanic_train) 
id<-sample(1:n,size=n*0.7)
train_data<- titanic_train[id, ]
test_data<- titanic_train[-id, ]

## Train Model
model <- glm(Survived ~ Pclass+Age,data=train_data,family="binomial")
summary(model)
train_data$prob_survival <- predict(model,type="response")
train_data$pred_survival <- ifelse(train_data$prob_survival>= 0.5,1,0)
conM<-table(train_data$pred_survival,train_data$Survived,dnn=c("predicted","actual"))
cat("Accuracy:",(conM[1,1]+conM[2,2])/sum(conM))

## Test Model
test_data$prob_survival <- predict(model,newdata = test_data,type="response")
test_data$pred_survival <- ifelse(test_data$prob_survival>= 0.5,1,0)
conM<-table(test_data$pred_survival,test_data$Survived,dnn=c("predicted","actual"))
cat("Accuracy:",(conM[1,1]+conM[2,2])/sum(conM))


## Accuracy









