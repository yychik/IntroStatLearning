#Chapter 8
library(ISLR)
#Q3
p1 <- seq(from=0,by=0.05,to=1)
G <- 2*(p1 * (1-p1))
D <- -(p1*log(p1) + (1-p1)*log(1-p1))
plot(p1,G)
plot(p1,D)

#Q7
library(MASS)
library(randomForest)
attach(Boston)
set.seed(1)

train<-sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train,"medv"]

try <- c(1:ncol(Boston))
tree <- seq(from=25, by=10, to=500)
err <- matrix(0, length(try), length(tree))

for(i in c(1:length(try)))
{
        for(j in c(1:length(tree)))
        {
                rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=try[i], ntree=tree[j])
                pred <- predict(rf.boston, newdata=Boston[-train,])
                err[i,j] <- mean((pred - boston.test)^2)
        }
}

#Using more trees, at 480, and at 4 predicts best predict the data

#Q8a
attach(Carseats)
set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
test <- (-train)

#Q8b
reg.tree <- tree(Sales~., data=Carseats[train,])
plot(reg.tree)
text(reg.tree, pretty=0)

tree.pred <- predict(reg.tree, newdata=Carseats[test,])
sales.test <- Carseats[test,"Sales"]
err <- mean((tree.pred - sales.test)^2)
err

#Q8c
cv.regtree<- cv.tree(reg.tree)
plot(cv.regtree$size, cv.regtree$dev, type="b")
cv.regtree$size[which.min(cv.regtree$dev)]

prune.regtree <- prune.tree(reg.tree, best=cv.regtree$size[which.min(cv.regtree$dev)])
plot(prune.regtree)
text(prune.regtree, pretty=0)

prune.pred <- predict(prune.regtree, newdata=Carseats[test,])
err.prune <- mean((prune.pred - sales.test)^2)
err.prune
#higher error rate..

#Q8d
set.seed(1)
bag.tree <- randomForest(Sales~., data=Carseats, subset=train, mtry=ncol(Carseats)-1, importance=TRUE)
bag.pred <- predict(bag.tree, newdata = Carseats[-train,])
bag.err <- mean((bag.pred - sales.test)^2)
#Impressive error rate @ 2.604369 MSE
importance(bag.tree)
#Price, ShelveLoc are most important

#Q8e
rf.tree <- randomForest(Sales~., data=Carseats, subset=train, importance=TRUE)
rf.pred <- predict(rf.tree, newdata=Carseats[-train,])
rf.err <- mean((rf.pred - sales.test)^2)
rf.err #higher than the bagging tree
importance(rf.tree)

#9a
attach(OJ)
train <- sample(1:nrow(OJ), 800)
test <- (-train)

#9b
tree.fit2 <- tree(Purchase~., data=OJ[train,])
summary(tree.fit2)
#Use only 5 variables in tree, with terminal nodes 9. High misclassification rate

#9c
tree.fit2

#9d
plot(tree.fit2)
text(tree.fit2, pretty=0)

#9e
tree.pred2 <- predict(tree.fit2, newdata=OJ[-train,], type="class")
table(tree.pred2, OJ$Purchase[test])
#Test Accuracy 0.8148

#9f
tree.cv <- cv.tree(tree.fit2, FUN=prune.misclass)
par(mfrow=c(1,2))
plot(tree.cv$size, tree.cv$dev, type="b") #number of terminal node vs error
plot(tree.cv$k, tree.cv$dev, type="b") #Prune parameter vs error
#Best tree at terminal node = 6

#9i
tree.prune <- prune.misclass(tree.fit2, best=6)
par(mfrow=c(1,1))
plot(tree.prune)
text(tree.prune, pretty=0)

#9j
err.prune <- predict(tree.prune, newdata=OJ[train,], type="class")
table(err.prune, OJ$Purchase[train])

#Training Error (Pruned) = 0.845
#Training Error (Unpruned) = 0.8488

#9k
test.prune <- predict(tree.prune, newdata=OJ[test,], type="class")
table(test.prune, OJ$Purchase[test])

#Test Error (Pruned) = 0.8074
#Test Error (Unpruned) = 0.8148

#10a
attach(Hitters)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)

#10b
train <- sample(1:nrow(Hitters), 200)
test <- (-train)

#10c
library(gbm)
set.seed(1)
boost.tree <- gbm(Salary~., data=Hitters[train,], distribution="gaussian", n.trees=1000, interaction.depth = 4)
summary(boost.tree)

shrinkage <- seq(from=0.001, by=0.001, to=0.1)
err.boost <- rep(0, length(shrinkage))

for(i in c(1:length(shrinkage)))
{
        boost <- gbm(Salary~., data=Hitters[train,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=shrinkage[i])
        boost.pred <- predict(boost, newdata=Hitters[train,], n.trees=1000)
        err.boost[i] <- mean((boost.pred - Hitters$Salary[train])^2)
}


plot(shrinkage,err.boost, type="b")

err.testboost <- rep(0, length(shrinkage))
for(i in c(1:length(shrinkage)))
{
        boost <- gbm(Salary~., data=Hitters[test,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=shrinkage[i])
        boost.pred <- predict(boost, newdata=Hitters[test,], n.trees=1000)
        err.testboost[i] <- mean((boost.pred - Hitters$Salary[test])^2)
        
}

plot(shrinkage,err.testboost, type="b")

#10f
summary(boost.tree)
#Catbat, CRuns, CHits are most important

#10g
bag.hitters <- randomForest(Salary~., data=Hitters, subset=train, mtry=ncol(Hitters)-1, importance=TRUE)
summary(bag.hitters)
bag.hitterspred <- predict(bag.hitters, newdata=Hitters[test,])
mean((bag.hitterspred - Hitters$Salary[test])^2)

min(err.testboost)

#11a
attach(Caravan)
train <- sample(1:nrow(Caravan), 1000)
test <- (-train)
Caravan$Purchase <- ifelse(Caravan$Purchase=="Yes",1,0)

#11b
caravan.boost <- gbm(Purchase~., data=Caravan[train,], distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01)
summary(caravan.boost)

caravan.pred <- predict(caravan.boost, newdata=Caravan[test,], n.trees = 1000, type="response")
pred.caravan <- rep(0, length(caravan.pred))
pred.caravan[caravan.pred > 0.2] <- 1
table(pred.caravan, Caravan$Purchase[test])
#Accuracy (Boosting) = 0.9201576
#True positive = 0.0843

caravan.logistic <- glm(Purchase~., data=Caravan[train,], family="binomial")
logistic.pred <- predict(caravan.logistic, newdata=Caravan[test,], type="response")
logistic.pred2 <- rep(0, length(logistic.pred))
logistic.pred2[logistic.pred > 0.2] <- 1
table(logistic.pred2, Caravan$Purchase[test])
#Accuracy - 0.884073
#True Positive = 0.1598639
