#Ch10

#7
library(ISLR)

d <- dist(scale(USArrests))
c <- 1 - cor(t(scale(USArrests)))
d <- as.matrix(d)^2
d%/%c

#8a
pca <- prcomp(USArrests, scale=TRUE)
pve <- pca$sdev^2/sum(pca$sdev^2)

#8b
apply((scale(USArrests) %*% pca$rotation)^2,2,sum)/sum(apply(scale(USArrests)^2,2,sum))

#9
hcluster <- hclust(dist(USArrests))
plot(hcluster)
before_std <- cutree(hcluster,3)

hcluster2 <- hclust(dist(scale(USArrests)))
plot(hcluster2)
after_std <- cutree(hcluster2,3)

table(before_std, after_std)

#10a
set.seed(1)
c1 <- matrix(rnorm(20*50),20,50)-3
c2 <- matrix(rnorm(20*50),20,50)+2
c3 <- matrix(rnorm(20*50),20,50)-6
dat <- rbind(c1,c2,c3)
factors <- c(rep(1,20),rep(2,20),rep(3,20))
dat <- data.frame(dat)



#10b
pca2 <- prcomp(dat)
cols <- rainbow(3)
cols[factors]

plot(pca2$x[,1:2], col=cols[factors], pch=10, xlab="Z1", ylab="Z2")

#10c
km <- kmeans(dat, 3, nstart=20)
km.clusters <- km$cluster
table(factors,km$cluster)

#10d
km2 <- kmeans(dat, 2, nstart=20)
km2.clusters <- km2$cluster
table(factors,km2$cluster)

#10e
km3 <- kmeans(dat, 4, nstart=20)
km3.clusters <- km3$cluster
table(factors,km3$cluster)

#10f
kmpc <- kmeans(pca2$x[,1:2],3,nstart=20)
table(factors,kmpc$cluster)

#10g
km_scale <- kmeans(scale(dat), 3, nstart=20)
km_scale.clusters <- km_scale$cluster
table(factors,km_scale$cluster)