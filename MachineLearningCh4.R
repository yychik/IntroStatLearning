#Chapter 4 Q10
library(ISLR)
attach(Weekly)

#a)
dim(Weekly)
summary(Weekly)
cor(Weekly[,-9])
pairs(Weekly)

#b)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family="binomial")
summary(glm.fit)

#c)
glm.probs <- predict(glm.fit,Weekly,type="response")
glm.pred <- rep("Down",length(Direction))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred,Direction)
#True Positive = 557/(557+48) = 0.92
#Total accuracy = (54+557)/1089 = 0.56

#d)
train <- Weekly[Year <= 2008,]
test <- Weekly[Year > 2008,]
glm.fit_2008 <- glm(Direction ~ Lag2, data = train, family="binomial")
summary(glm.fit_2008)

glm.probs_2008 <- predict(glm.fit_2008,test,type="response")
glm.pred_2008 <- rep("Down",length(test$Direction))
glm.pred_2008[glm.probs_2008 > 0.5] <- "Up"
table(glm.pred_2008,test$Direction)
#True Positive: 56/(56+5) = 0.918
#Accruracy = (9+56)/104 = 0.625

#e
library(MASS)
lda.fit <- lda(Direction~Lag2, data=Weekly, subset=(Year<=2008))
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, test)
table(lda.pred$class,test$Direction)
#True Positive: 56/(56+5) = 0.918
#Accruracy = (9+34)/104 = 0.413

#f
qda.fit <- qda(Direction~Lag2, data=Weekly, subset=(Year<=2008))
qda.fit
qda.pred <- predict(qda.fit, test)
table(qda.pred$class,test$Direction)
#True Positive: 61/(61) = 1.0
#Accruracy = (61)/104 = 0.586
mean(qda.pred$class == test$Direction)

#g
library(class)
knn.train <- cbind(Lag1,Lag2)[(Year<=2008),]
knn.test <- cbind(Lag1,Lag2)[!(Year<=2008),]
train.Direction <- Direction[(Year<=2008)]
set.seed(1)
knn.pred <- knn(knn.train,knn.test,train.Direction,k=1)
table(knn.pred,test$Direction)
#True Positive: 32/(29+32) = 0.524
#Accuracy: (18+32)/104 = 0.481
mean(knn.pred==test$Direction)

accuracy <- c()
for(i in 1:20)
{
        knn.pred <- knn(knn.train,knn.test,train.Direction,k=i)
        accuracy[i] <- mean(knn.pred==test$Direction)
}

lines((1:20),accuracy)

#more Data for lda?
library(MASS)
accuracy <- c()
for(i in 1991:2009)
{
lda.fit <- lda(Direction~Lag2, data=Weekly, subset=(Year<=i))
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, Weekly[(Year>i),])
table(lda.pred$class,Weekly[(Year>i),]$Direction)
#True Positive: 56/(56+5) = 0.918
#Accruracy = (9+34)/104 = 0.413
accuracy[i-1990] <- mean(lda.pred$class == Weekly[(Year>i),]$Direction)
}
lines((1:19),accuracy)
#===========================================================================

#Q11
attach(Auto)
summary(Auto)

#a)
mpg01 <- rep(0, length(mpg))
mpg01[mpg<median(mpg)] <- 1
auto1 <- cbind(Auto,mpg01)
summary(auto1)

#b)
pairs(auto1)
boxplot(displacement~mpg01)
boxplot(acceleration~mpg01)
boxplot(horsepower~mpg01)
boxplot(weight~mpg01)
boxplot(year~mpg01)
boxplot(origin~mpg01)

#c)
train <- auto1[(year<76),]
test <- auto1[!(year<76),]

#d)
lda.fit <- lda(mpg01 ~ displacement + acceleration + horsepower + weight + year, data=train)
lda.fit
lda.pred <- predict(lda.fit,test)
table(lda.pred$class,test$mpg01)
mean(lda.pred$class == test$mpg01) #0.896

#d)
qda.fit <- qda(mpg01 ~ displacement + acceleration + horsepower + weight + year, data=train)
qda.fit
qda.pred <- predict(qda.fit,test)
table(qda.pred$class,test$mpg01)
mean(qda.pred$class == test$mpg01) #0.877

#e)
glm.fit <- glm(mpg01 ~ displacement + acceleration + horsepower + weight + year, data=train)
glm.pred <- predict(glm.fit, test)
logit_pred <- rep(0,length(glm.pred))
logit_pred[glm.pred > 0.5] <- 1
table(logit_pred,test$mpg01)
mean(logit_pred == test$mpg01) #0.896

#f)
library(class)
knn.train <- cbind(displacement,acceleration, horsepower, weight, year)[(year<76),]
knn.test <- cbind(displacement,acceleration, horsepower, weight, year)[!(year<76),]
acc <- c()
for(i in 1:20)
{
set.seed(1)
knn.pred <- knn(knn.train,knn.test,train$mpg01,k=i)
acc[i] <- mean(knn.pred==test$mpg01)#0.853
}
plot((1:20),acc)