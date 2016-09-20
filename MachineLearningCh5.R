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

