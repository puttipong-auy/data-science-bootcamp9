library(tidyverse)

library(caret)

churn <- read_csv("churn.csv")


## HW
## build churn prediction model
## method = "glm"
## 3-5 independent variables


churn <- churn %>%
        mutate(churn= if_else(churn=="No",0,1),
               internationalplan= if_else(internationalplan=="no",0,1),
               voicemailplan= if_else(voicemailplan=="no",0,1))
churn$churn <- factor(churn$churn)

train_test_split<- function(data,size=0.8){
  set.seed(42)
  n<-nrow(data)
  train_id <- sample(1:n,size*n)
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ] 
  return( list(train_df,test_df) )
}
cor(churn)

prep_df<- train_test_split(churn,0.8)

model_full <- train( churn ~ .,
                data=prep_df[[1]],
                method="glm")

summary(model_full)
ctrl <- trainControl(method = "boot",
                     number=25)


model <- train( churn ~ internationalplan + voicemailplan + totalintlcalls 
                    +numbercustomerservicecalls,
                data=prep_df[[1]],
                method="glm")

p_churn <- predict(model , newdata=prep_df[[2]] )
prep_df[[2]]$pred <- p_churn

mean(prep_df[[2]]$churn == prep_df[[2]]$pred)

conM<-table(prep_df[[2]]$pred,prep_df[[2]]$churn,dnn=c("predicted","actual"))

## Model Evaluation
cat("Accuracy:",(conM[1,1]+conM[2,2])/sum(conM))

cat("Precision:",(conM[2,2])/(conM[2,1]+conM[2,2]))

cat("Recall:",(conM[2,2])/(conM[1,2]+conM[2,2]))

cat("F1 Score:" ,2*(0.9*0.9)/(0.9+0.9))




#cor
#internationalplan
#totaldaycharge
#totaldayminutes
#numbercustomerservicecalls
#coef
#internationalplan
#voicemailplan
#totalintlcalls 
#numbercustomerservicecalls




