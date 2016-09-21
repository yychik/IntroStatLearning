#Chapter 5
library(ISLR)

#Q5
attach(Default)

#a
set.seed(1)
glm.fit <- glm(default ~ income + balance, family="binomial", data=Default)
glm.fit

#b
idx <- sample(nrow(Default), size=nrow(Default)/2, replace=FALSE)
training <- Default[idx,]
test <- Default[-idx,]
glm.fit <- glm(default ~ income + balance, family="binomial", data=training)
glm.prob <- predict(glm.fit, data=test, type="response")
glm.pred <- rep("No",length(test$default))
glm.pred[glm.prob >= 0.5] <- "Yes"
mean(glm.pred == test$default) #0.9536

#c
error <- c()
for (i in 1:3)
{
        idx <- sample(nrow(Default), size=nrow(Default)/2, replace=FALSE)
        training <- Default[idx,]
        test <- Default[-idx,]
        glm.fit <- glm(default ~ income + balance, family="binomial", data=training)
        glm.prob <- predict(glm.fit, data=test, type="response")
        glm.pred <- rep("No",length(test$default))
        glm.pred[glm.prob >= 0.5] <- "Yes"
        error[i] <- mean(glm.pred == test$default) #0.9536
}
mean(error) #0.9531333

#d
error2 <- c()
for (i in 1:3)
{
        idx <- sample(nrow(Default), size=nrow(Default)/2, replace=FALSE)
        training <- Default[idx,]
        test <- Default[-idx,]
        glm.fit <- glm(default ~ income + balance + student, family="binomial", data=training)
        glm.prob <- predict(glm.fit, data=test, type="response")
        glm.pred <- rep("No",length(test$default))
        glm.pred[glm.prob >= 0.5] <- "Yes"
        error2[i] <- mean(glm.pred == test$default) #0.9536
}
mean(error2) #0.9530667

#6a
set.seed(1)
glm.fit2 <- glm(default ~ income + balance, data=Default, family="binomial")
summary(glm.fit2)
glm.fit2

#6b
boot.fn <- function(data,index)
{
        fit <- glm(default~income+balance, data=data[index,], family="binomial")
        return(fit$coeff[2:3])
}
boot.fn(Default,(1:100))

#6c, d
library(boot)
boot(Default, boot.fn, R=100)

#7a
attach(Weekly)
glm.fit3 <- glm(Direction ~ Lag1 + Lag2, family="binomial", data=Weekly)
glm.fit3

#7b
glm.fit4 <- glm(Direction ~ Lag1 + Lag2, family="binomial", data=Weekly[-1,])
glm.fit4

#7c
glm.prob4 <- predict(glm.fit4, Weekly[1,], type="response")
ifelse(glm.prob4 > 0.5, glm.pred4<-"Up", glm.pred4 <- "Down")
mean(glm.pred4==Weekly[1,]$Direction)

#7d
error <- rep(0,nrow(Weekly))
for(i in 1:nrow(Weekly))
{
        glm.fit5 <- glm(Direction ~ Lag1 + Lag2, family="binomial", data=Weekly[-i,])
        glm.prob5 <- predict(glm.fit5, Weekly[i,], type="response")
        ifelse(glm.prob5 > 0.5, glm.pred5 <- "Up", glm.pred5 <- "Down")
        if(glm.pred5 == Weekly[i,]$Direction){error[i] <- 1}
        
}
1-mean(error) #0.4499

#8a
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

#8b
plot(x,y)

#8c
data <- data.frame(y,x)
err <- rep(0,100)
for(i in 1:100)
{
        lm.fit <- lm(y~x, data=data[-i,])
        lm.err <- predict(lm.fit, data[i,])
        err[i] <- (data$y[i] - lm.err)^2
}
mean(err)

cv.err <- rep(0,4)
for(i in 1:4)
{
glm.fit6 <- glm(y~poly(x,i),data=data)
cv.err[i] <- cv.glm(data,glm.fit6)$delta[1]
}
cv.err