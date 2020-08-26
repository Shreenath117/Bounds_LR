# Case Study Solutions : Boundsdata.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('boundsdata.csv')
# View the data loaded
head(data)
# Dropping the first column which is nothing but the Serial number
data=data[2:8]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

data$manip <- as.factor(data$manip)
data$manip

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:

summary(data)

str(data)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 4:

library(corrplot)
library(psych)

data_numeric <- data[,c('out','out.enc','med','med.enc','ttt','enc')]
correlation<-corr.test(data_numeric)
correlation

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 5:

# We observe from the above output of correlation matrix: 
# a) out, med and ttt are highly correlated with enc
# b) out.enc is highly correlated with ttt
# c) med.enc is not correlated with any of the variables

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 6:

pairs(data[,c("out","out.enc","med","med.enc","ttt","enc")], pch=16, las=1)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 7:

ggplot(data, aes(out, ..count..)) + geom_bar(aes(fill = manip), position = "dodge") 

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 8:

ggplot(data, aes(out.enc, ..count..)) + geom_bar(aes(fill = manip), position = "dodge") 

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 9:

# In the plot for Q7) : we observe for out =0 : manip=1 count is higher and for out= 1 ; manip=0 count is higher

# In the plot for Q8) : we observe for out.enc =0 : manip=1 count is slightly higher and for out.enc= 1 ; manip=1 count is higher

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 10:

ggplot(data, aes(med, ..count..)) + geom_bar(aes(fill = manip), position = "dodge") 

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 11:

ggplot(data, aes(med.enc, ..count..)) + geom_bar(aes(fill = manip), position = "dodge") 

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# In the plot for Q10) : we observe for med =0 : manip=0 count is higher
# and for med= 1 ; manip=1 count is significantly higher

# In the plot for Q11) : we observe for med.enc =0 : manip=1 count is higher
# and for med.enc= 1 ; manip=1 count is slightly higher

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 13:

trainrows <- sample(nrow(data), nrow(data) * 0.70)
data.train <- data[trainrows, ]
data.test <- data[-trainrows,]

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 14:

data.train.glm0 <- glm(manip~., family = binomial, data.train)
summary(data.train.glm0)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 15:

#In-Sample Prediction : Response variable split
summary(data.train$manip)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 16:

#Summary of In- Sample predicted values
data.train.pred<-predict(data.train.glm0,type="response")
summary(data.train.pred)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 17:

hist(predict(data.train.glm0,type="response"))

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 18:

pred <- prediction(data.train.pred, data.train$manip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)


#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 19:

#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values"))
# Area under the curve : Accuracy : 65%

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 20:

#Out of Sample Prediction
pred.glm0.test<- predict(data.train.glm0, newdata = data.test, type="response")
summary(pred.glm0.test)

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 21:

library(randomForest)
m3 <- randomForest(manip ~ ., data = data.train)
summary(m3)

m3_fitForest <- predict(m3, newdata = data.test, type="prob")[,2]
m3_pred <- prediction( m3_fitForest, data.test$manip)

m3_perf <- performance(m3_pred, "tpr", "fpr")
m3_pred <- prediction( m3_fitForest, data.test$manip)

#plot variable importance
varImpPlot(m3, main="Random Forest: Variable Importance")

# Model Performance plot
plot(m3_perf,colorize=TRUE, lwd=2, main = "m3 ROC: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)

m3_AUROC <- round(performance(m3_pred, measure = "auc")@y.values[[1]]*100, 2)
m3_AUROC

# Accuracy of Random forest model : 73%



