#Chapter 9
library(e1071)

#Q4
set.seed(1)
x <- matrix(runif(100*2), ncol=2)
x[1:50,] <-  sqrt(x[51:100,]^2 + x[1:50,]^2) + 0.01
x[51:100,] <- sqrt(x[1:50,]^2 + x[51:100,]^2) - 0.01
y<-c(rep(1,50), rep(-1,50))
x[y==1,]=x[y==1,]+1
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=(3-y))

svm.linear <- svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svm.linear, dat)
accuracy.linear <- mean(svm.linear$fitted == y)

svm.radial <- svm(y~., data=dat, kernel="radial", gamma=1, cost=0.1, scale=FALSE)
plot(svm.radial, dat)
accuracy.radial <- mean(svm.radial$fitted == y)

svm.poly3 <- svm(y~., data=dat, kernel="polynomial", degree=3 ,gamma=1, cost=0.1, scale=FALSE)
plot(svm.poly3, dat)
accuracy.poly3 <- mean(svm.poly3$fitted == y)

svm.poly4 <- svm(y~., data=dat, kernel="polynomial", degree=4 ,gamma=1, cost=0.1, scale=FALSE)
plot(svm.poly4, dat)
accuracy.poly4 <- mean(svm.poly4$fitted == y)

svm.poly5 <- svm(y~., data=dat, kernel="polynomial", degree=5 ,gamma=1, cost=0.1, scale=FALSE)
plot(svm.poly5, dat)
accuracy.poly5 <- mean(svm.poly5$fitted == y)

#5a
x1 <- runif(500) - 0.5
x2 <- runif(500) -0.5
y <- 1*(x1^2 - x2^2 > 0) 

#5b
plot(x1,x2,col=3-y, main="Actual")

#5c
logistic.fit <- glm(y~x1 + x2, family="binomial")
logistic.pred <- rep(0,length(x1))
logistic.pred[logistic.fit$fitted.values > 0.5] <- 1
plot(x1,x2,col=3-logistic.pred, main="Linear")

#5d,e
logistic2.fit <- glm(y~x1 + x2 + x1^2 + x2^2 +x1*x2 + x1^3 + x2^3 + x1^2*x2 + x2^2*x1, family="binomial")
logistic2.pred <- rep(0,length(x1))
logistic2.pred[logistic2.fit$fitted.values > 0.5] <- 1

plot(x1,x2,col=3-logistic2.pred, main="Degree 3 Polynomial")

#5g
data_q5 <- data.frame(x1=x1,x2=x2,y=as.factor(y))
svm.linear2 <- svm(y~., data=data_q5, kernel="linear", scale=FALSE)
plot(x1,x2, col=4-as.numeric(svm.linear2$fitted), main="Linear SVM")

#5h
svm.nonlinear <- svm(y~., data=data_q5, kernel="radial", scale=FALSE)
plot(x1,x2, col=4-as.numeric(svm.nonlinear$fitted), main="Radial SVM")

#6a
set.seed(1)
x<- matrix(rnorm(20*2), ncol=2)
y<-c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,]+1
plot(x, col=(y+5)/2, pch=19)
dat <- data.frame(x=x, y=as.factor(y))

train.dat <- sample(1:nrow(dat), nrow(dat)/2)
test.dat <- (-train.dat)

#6b
tune.train <- tune(svm, y~., data=dat[train.dat,], kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1.5,10,100)))
summary(tune.train)

#6c
best.mod <- tune.train$best.model
cost=c(0.001,0.01,0.1,1.5,10,100)
accuracy <- c()
for(i in 1:length(cost))
{
mod <- svm(y~., data=dat[test.dat, ], kernel="linear", cost=cost[i])
plot(mod, dat[test.dat, ])
pred <- predict(mod,dat[test.dat,])
accuracy[i] <- mean(pred == y[test.dat])
}
cost[which.max(accuracy)]

#7a
library(ISLR)
attach(Auto)
str(Auto)
Auto$HL <- rep(0,nrow(Auto))
Auto$HL[Auto$mpg > median(Auto$mpg)] <- 1
Auto$HL <- as.factor(Auto$HL)

#7b
cv.linear <- tune(svm, HL~., data=Auto, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1.5,10,100)))
summary(cv.linear)
mean(predict(cv.linear$best.model, Auto)==HL)

#7c
cv.radial <- tune(svm, HL~., data=Auto, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(cv.radial)
mean(predict(cv.radial$best.model, Auto)==HL)

#7d
cv.poly <- tune(svm, HL~., data=Auto, kernel="polynomial", ranges=list(cost=c(0.1,1,10,100,1000), degree=c(2,3), gamma=c(0.5,1,2,3,4)))
summary(cv.poly)
mean(predict(cv.poly$best.model, Auto)==HL)

plot(cv.linear$best.model, data=Auto, mpg~year)
plot(cv.linear$best.model, data=Auto, mpg~cylinders)
plot(cv.linear$best.model, data=Auto, mpg~weight)
plot(cv.linear$best.model, data=Auto, mpg~displacement)
plot(cv.linear$best.model, data=Auto, mpg~acceleration)
plot(cv.linear$best.model, data=Auto, mpg~origin)

plot(cv.radial$best.model, data=Auto, year~origin)
plot(cv.radial$best.model, data=Auto, mpg~acceleration)

plot(cv.poly$best.model, data=Auto, year~acceleration)
plot(cv.poly$best.model, data=Auto, mpg~year)

#8a
attach(OJ)
str(OJ)
train <- sample(1:nrow(OJ), 800)
test <- (-train)

#8b
OJ.svm.linear <- svm(Purchase~., data=OJ[train,], cost=0.01, kernel="linear")
summary(OJ.svm.linear)

#8c
err.training <- mean(predict(OJ.svm.linear, newdata=OJ[train,]) == OJ$Purchase[train])
err.test <- mean(predict(OJ.svm.linear, newdata=OJ[test,]) == OJ$Purchase[test]) 

#8d
OJ.tune <- tune(svm, Purchase~., data=OJ[train,], kernel="linear", ranges=list(cost=c(0.01,0.1,1,5,10)))
summary(OJ.tune)

cverr.train <- mean(predict(OJ.tune$best.model, newdata=OJ[train,]) == OJ$Purchase[train])
cverr.test <- mean(predict(OJ.tune$best.model, newdata=OJ[test,]) == OJ$Purchase[test])

#8e
OJ.svm.radial <- svm(Purchase~., data=OJ[train,], cost=0.01, kernel="radial")
summary(OJ.svm.radial)
err.training2 <- mean(predict(OJ.svm.radial, newdata=OJ[train,]) == OJ$Purchase[train])
err.test2 <- mean(predict(OJ.svm.radial, newdata=OJ[test,]) == OJ$Purchase[test]) 

OJ.tune2 <- tune(svm, Purchase~., data=OJ[train,], kernel="linear", ranges=list(cost=c(0.01,0.1,1,5,10)))
summary(OJ.tune2)
cverr.train2 <- mean(predict(OJ.tune2$best.model, newdata=OJ[train,]) == OJ$Purchase[train])
cverr.test2 <- mean(predict(OJ.tune2$best.model, newdata=OJ[test,]) == OJ$Purchase[test])

#8g
OJ.svm.poly <- svm(Purchase~., data=OJ[train,], cost=0.01, kernel="polynomial", degree=2)
summary(OJ.svm.poly)
err.training3 <- mean(predict(OJ.svm.poly, newdata=OJ[train,]) == OJ$Purchase[train])
err.test3 <- mean(predict(OJ.svm.poly, newdata=OJ[test,]) == OJ$Purchase[test]) 

OJ.tune3 <- tune(svm, Purchase~., data=OJ[train,], kernel="polynomial", ranges=list(cost=c(0.01,0.1,1,5,10), degree=2))
summary(OJ.tune3)
cverr.train3 <- mean(predict(OJ.tune3$best.model, newdata=OJ[train,]) == OJ$Purchase[train])
cverr.test3 <- mean(predict(OJ.tune3$best.model, newdata=OJ[test,]) == OJ$Purchase[test])
