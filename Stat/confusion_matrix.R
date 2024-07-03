## Logistic Regression Example
## Binary Classification

happiness <- c(10,8,9,7,8,5,9,6,8,7,1,1,3,1,4,5,6,3,2,0)

divorce <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)


df <- data.frame(happiness, divorce)
 
## fit logistic regression Full dataset
model <- glm(divorce ~ happiness,data=df,family="binomial")

summary(model)

## Predict and Evaluate Model
df$prob_disvorce <- predict(model,type="response")

df$pred_divorce <- ifelse(df$prob_disvorce>=0.5,1,0)

## confusion matrix
conM<-table(df$pred_divorce,df$divorce,dnn=c("predicted","actual"))

## Model Evaluation
cat("Accuracy:",(conM[1,1]+conM[2,2])/sum(conM))

cat("Precision:",(conM[2,2])/(conM[2,1]+conM[2,2]))

cat("Recall:",(conM[2,2])/(conM[1,2]+conM[2,2]))

cat("F1 Score:" ,2*(0.9*0.9)/(0.9+0.9))











