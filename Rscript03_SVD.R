rm(list =ls() )
library(foreign) # read.spss
# library(Matrix)
# library(irlba)
# library(ggplot2)
# library(reshape)
library(tidyverse)
source("./ideology_functions.R")
load('allData.Rdata')


maxs <- apply(data1, 2, max) 
mins <- apply(data1, 2, min)

scaled <- as.data.frame(scale(data1, center = mins, scale = maxs - mins))

svd1 <- svd(scaled)
plot(svd1$d[2:17])

X1 <- svd1$u[,1:4]
set.seed(123)
km1 <- kmeans(X1, centers =  4, iter.max = 30, nstart = 100)
km1 <- kmeans(X1, centers = km1$centers[order(km1$centers[,1]),], iter.max = 1)

table(km1$cluster)
Z <- membershipM(km1$cluster)
centers <- diag(colSums(Z)^(-1)) %*% t(Z) %*% as.matrix(x1)
plt.centers(centers, labs = 1:4)

full.cluster <- rep(NA, nrow(originalData))
full.cluster[-missing_ids] <- km1$cluster
result  <- data.frame(1:nrow(originalData), cluster = full.cluster) 
write.csv(result, file ="../result_SVD.csv", row.names = F)




scaled <- as.data.frame(scale(data1))
svd1 <- svd(scaled)
plot(svd1$d[1:17])

X1 <- svd1$u[,1:4]
X1 <- t( apply(X1, 1, function(x) x/sqrt(sum(x*x)/+1e-06)))
set.seed(123)
km1 <- kmeans(X1, centers =  4, iter.max = 30, nstart = 100)
km1 <- kmeans(X1, centers = km1$centers[order(km1$centers[,1]),], iter.max = 1)
table(km1$cluster)
Z <- membershipM(km1$cluster)
centers <- diag(colSums(Z)^(-1)) %*% t(Z) %*% as.matrix(x1)
plt.centers(centers, labs = 1:4)

full.cluster <- rep(NA, nrow(originalData))
full.cluster[-missing_ids] <- km1$cluster
result  <- data.frame(1:nrow(originalData), cluster = full.cluster) 

#write.csv(result, file ="../result_SVD.csv", row.names = F)







