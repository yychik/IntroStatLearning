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
mean(glm.pred == test$default)
