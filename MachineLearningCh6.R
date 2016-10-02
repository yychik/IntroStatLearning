#Chapter 6

#8a
set.seed(1)
x <- rnorm(100)
e <- rnorm(100)

#8b
y <- 1 + 0.5*x - 0.74*x^2 + 1.37*x^3 + e 

#8c
library(leaps)
data <- data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)
regfit.full <- regsubsets(y~.,data, nvmax = 10)
regfit.summary <- summary(regfit.full)
regfit.summary

par(mfrow=c(2,2))
plot(regfit.summary$rsq, xlab="Number of Variables", ylab="R-Squared", type="l")
points(which.max(regfit.summary$rsq),regfit.summary$rsq[which.max(regfit.summary$rsq)], col="red", cex=2, pch=20)

plot(regfit.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R-Squared", type="l")
points(which.max(regfit.summary$adjr2),regfit.summary$adjr2[which.max(regfit.summary$adjr2)], col="red", cex=2, pch=20)

plot(regfit.summary$cp, xlab="Number of Variables", ylab="Mallow's Cp", type="l")
points(which.min(regfit.summary$cp),regfit.summary$cp[which.min(regfit.summary$cp)], col="red", cex=2, pch=20)

plot(regfit.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(regfit.summary$bic),regfit.summary$bic[which.min(regfit.summary$bic)], col="red", cex=2, pch=20)
mtext("Best Subset Selection", side = 3, line = -3, outer = TRUE)


#8d
regfit.fwd <- regsubsets(y~., data, nvmax=10, method="forward")
regfit.fwdsum <- summary(regfit.fwd)
regfit.fwdsum

par(mfrow=c(2,2))
plot(regfit.fwdsum$rsq, xlab="Number of Variables", ylab="R-Squared", type="l")
points(which.max(regfit.fwdsum$rsq),regfit.fwdsum$rsq[which.max(regfit.fwdsum$rsq)], col="red", cex=2, pch=20)

plot(regfit.fwdsum$adjr2, xlab="Number of Variables", ylab="Adjusted R-Squared", type="l")
points(which.max(regfit.fwdsum$adjr2),regfit.fwdsum$adjr2[which.max(regfit.fwdsum$adjr2)], col="red", cex=2, pch=20)

plot(regfit.fwdsum$cp, xlab="Number of Variables", ylab="Mallow's Cp", type="l")
points(which.min(regfit.fwdsum$cp),regfit.fwdsum$cp[which.min(regfit.fwdsum$cp)], col="red", cex=2, pch=20)

plot(regfit.fwdsum$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(regfit.fwdsum$bic),regfit.fwdsum$bic[which.min(regfit.fwdsum$bic)], col="red", cex=2, pch=20)
mtext("Forward Subset Selection", side = 3, line = -3, outer = TRUE)

#====================================================================================

regfit.bwd <- regsubsets(y~., data, nvmax=10, method="backward")
regfit.bwdsum <- summary(regfit.bwd)
regfit.bwdsum

par(mfrow=c(2,2))

plot(regfit.bwdsum$rsq, xlab="Number of Variables", ylab="R-Squared", type="l")
points(which.max(regfit.bwdsum$rsq),regfit.bwdsum$rsq[which.max(regfit.bwdsum$rsq)], col="red", cex=2, pch=20)

plot(regfit.bwdsum$adjr2, xlab="Number of Variables", ylab="Adjusted R-Squared", type="l")
points(which.max(regfit.bwdsum$adjr2),regfit.bwdsum$adjr2[which.max(regfit.bwdsum$adjr2)], col="red", cex=2, pch=20)

plot(regfit.bwdsum$cp, xlab="Number of Variables", ylab="Mallow's Cp", type="l")
points(which.min(regfit.bwdsum$cp),regfit.bwdsum$cp[which.min(regfit.bwdsum$cp)], col="red", cex=2, pch=20)

plot(regfit.bwdsum$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(regfit.bwdsum$bic),regfit.bwdsum$bic[which.min(regfit.bwdsum$bic)], col="red", cex=2, pch=20)
mtext("Backward Subset Selection", side = 3, line = -3, outer = TRUE)

#8e
library(glmnet)
set.seed(1)
xx <- model.matrix(y~., data)[,-1]
train <- sample(1:nrow(xx), nrow(xx)/2)
test<- (-train)
y.test<-y[test]

lasso.cv <- cv.glmnet(xx[train,], y[train], alpha=1)
optlam <- lasso.cv$lambda.min
plot(lasso.cv)

#8f
yy <- -1.5 + 4.23*x^7 + e
data_8f <- data.frame(yy,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)
data_8f2 <- model.matrix(yy~., data_8f)[,-1]

train <- sample(1:nrow(data_8f2), nrow(data_8f2)/2)
test <- (-train)

lasso8f.cv <- cv.glmnet(data_8f2[train,], yy[train], alpha=1)
optlam <- lasso8f.cv$lambda.min
plot(lasso8f.cv)
lasso8f.full <- glmnet(data_8f2,yy,alpha=1,lambda = optlam)
lasso8f.pred <- predict(lasso8f.full, type="coefficients")
lasso8f.pred

bs <- regsubsets(yy~., data_8f, nvmax=10)
bs.summary <- summary(bs)

par(mfrow=c(2,2))
plot(bs.summary$rsq, xlab="Number of Variables", ylab="R-Squared", type="l")
points(which.max(bs.summary$rsq),bs.summary$rsq[which.max(bs.summary$rsq)], col="red", cex=2, pch=20)

plot(bs.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R-Squared", type="l")
points(which.max(bs.summary$adjr2),bs.summary$adjr2[which.max(bs.summary$adjr2)], col="red", cex=2, pch=20)

plot(bs.summary$cp, xlab="Number of Variables", ylab="Mallow's Cp", type="l")
points(which.min(bs.summary$cp),bs.summary$cp[which.min(bs.summary$cp)], col="red", cex=2, pch=20)

plot(bs.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(bs.summary$bic),bs.summary$bic[which.min(bs.summary$bic)], col="red", cex=2, pch=20)
mtext("Best Subset Selection", side = 3, line = -3, outer = TRUE)
#====================================================================
#9a
attach(College)
set.seed(1)
regdata <- model.frame(Apps~., College)
train <- sample(1:nrow(regdata), nrow(regdata)/2)
test <- (-train)

#9b
reg.fit <- lm(Apps~., regdata[train,])
summary(reg.fit)
reg.pred <- predict(reg.fit, regdata[test,])
lm_err <- mean((reg.pred - Apps[test])^2)
lm_err

#9c
ridgedata <- model.matrix(Apps~., College)[,-1]
ridge.fit <- cv.glmnet(ridgedata[train,], Apps[train], alpha=0)
optlam <- ridge.fit$lambda.min
ridge.full <- glmnet(ridgedata[train,], Apps[train], alpha=0, lambda = optlam)
ridge.pred <- predict(ridge.full, newx=ridgedata[test,])
ridge_err <- mean((ridge.pred - Apps[test])^2)
ridge_err

#9d
lasso.fit <- cv.glmnet(ridgedata[train,], Apps[train], alpha=1)
optlam2 <- lasso.fit$lambda.min
lasso.full <- glmnet(ridgedata[train,], Apps[train], alpha=1, lambda = optlam2)
lasso.pred <- predict(lasso.full, newx=ridgedata[test,])
lasso_err <- mean((lasso.pred - Apps[test])^2)
lasso_err

#9e
library(pls)
set.seed(1)
pcr.fit <- pcr(Apps~., data=regdata, subset=train, scale=TRUE, valiation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.fit
summary(pcr.fit)
pcr.pred <- predict(pcr.fit, regdata[test,], ncomp=pcr.fit$ncomp)
pcr_err <- mean((pcr.pred - Apps[test])^2)
pcr_err

#9f
set.seed(1)
pls.fit <- plsr(Apps~., data=regdata, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
pls.pred <- predict(pls.fit, regdata[test,], ncomp=pls.fit$ncomp)
pls_err <- mean((pls.pred - Apps[test])^2)
pls_err

error <- c(lm_err, ridge_err, lasso_err, pcr_err, pls_err)
error
#Lasso is the best fit

coef(lasso.fit)

lasso.full <- glmnet(ridgedata, Apps, alpha=1, lambda = optlam2)
lasso.fullpred <- predict(lasso.full, type="coefficients", s=optlam2)
lasso.fullpred

#====================================================================
#10a
set.seed(1)
X <- matrix(rnorm(1000*20),nrow=1000,ncol=20)
b <- rnorm(20)
b[sample(1:length(b), length(b)/4)] <- 0
e <- rnorm(1000)
y <- X %*% b + e

#10b
train <- sample(1:1000,100)
test <- (-train)

#10c
data_10 <- data.frame(y,X)
bs10.fit <- regsubsets(y~., data=data_10[train,], nvmax=20)
bs10.summary <- summary(bs10.fit)
bs10.summary

train_mse <- rep(NA,20)
train_dat10 <- model.matrix(y~.,data=data_10[train,])

for(i in 1:20)
{
        coeff <- coef(bs10.fit, id=i)
        pred <- train_dat10[,names(coeff)] %*% coeff
        train_mse[i] <- mean((y[train]-pred)^2)
}

par(mfrow=c(1,2))
plot((1:20),train_mse,type="l", xlab="Number of Features", ylab="Mean-Squared Error")
points(which.min(train_mse),train_mse[which.min(train_mse)], col="red", cex=2, pch=20)
title("Training Set MSE")

#10d
mse <- rep(NA,20)
test_dat10 <- model.matrix(y~.,data=data_10[test,])

for(i in 1:20)
{
        coeff <- coef(bs10.fit, id=i)
        pred <- test_dat10[,names(coeff)] %*% coeff
        mse[i] <- mean((y[test]-pred)^2)
}


plot((1:20),mse,type="l", xlab="Number of Features", ylab="Mean-Squared Error")
points(which.min(mse),mse[which.min(mse)], col="red", cex=2, pch=20)
title("Test Set MSE")

#10f
b
coef(bs10.fit,id=which.min(mse)) # more or less, the coefficients that are originally 0 are 0 in the predicted coefficients. 

#10g
names(b) <- names(coef(bs10.fit,id=20)[-1])
distance <- rep(0,20)

for(i in 1:20)
{
        temp <- b
        temp[names(coef(bs10.fit,id=i)[-1])] <- b[names(coef(b0s10.fit,id=i)[-1])] - coef(bs10.fit,id=i)[names(coef(bs10.fit,id=i)[-1])]
        distance[i] <- sqrt(sum(temp^2))
}

par(mfrow=c(1,1))
plot((1:20),distance,xlab="Number of Features",ylab="L-2 Norm", type="l")
points(which.min(distance),distance[which.min(distance)], col="red", cex=2, pch=20)
title("Distance between Predicted and True Coefficients")

