rm(list =ls() )
library(foreign) # read.spss
library(Matrix)
library(irlba)
library(ggplot2)
library(reshape)
source("./ideology_functions.R")
load('data.RData')

# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled; x2 is PCA of x1 with 8 components



nNodes <- dim(x1)[1]
nVar <- dim(x1)[2]

###Exploratory analysis
(headers <- colnames(x1))

# asymmetry

par(mar	= c(3, 4, 4, 2) + 0.1)
par(mfcol = c(2,1))
headers <- colnames(data1)
for(i in 1:ncol(data1)){
  barplot(table(data1[,i]), main = paste0("scores on ", headers[i])) 
}
par(mar= c(5, 4, 4, 2) + 0.1, mfcol = c(1,1))
    

par(mar	= c(3, 8, 4, 2) + 0.1)
boxplot(data1, horizontal = T, las = 1, main = "before scaling")
boxplot(x1, horizontal = T, las = 1, main = "after scaling, mean = 1, sd = 1")
#boxplot(as.matrix(x1), horizontal = T, las = 1, main = "after scaling, mean = 1, sd = 1")
par(mar	= c(5, 4, 2, 3) + 0.1)

#correlation among variables
cov <- cor(data1)
cov[ row(cov) < col(cov)] <- 0; which(abs(cov) > 0.5, arr.ind = T)
balloon.plot(cov, rowlabel = headers, collabel = headers)



## k - means  how many clusters?
k = 1:20
wss <- rep(0,20)
xm <- colMeans(x1)
wss[1] = sum(x1^2)- nrow(x1)*sum(xm^2)

for(i in 2:20){
    model <- kmeans(x1, centers = i, nstart =20, iter.max = 30)
    wss[i] <- sum(model$withinss)
}
plot(wss/wss[1], type ="b"); 
#abline(v=6, col ="red", lty =2); 
abline(v=9, col ="red", lty =2)

k = 9
set.seed(100)
km1 <- kmeans(x1, k, nstart =  100, iter.max = 30)
ord <- order(km1$centers[,1], decreasing = T)
km1 <- kmeans(x1, centers = km1$centers[ord,], nstart =  1, iter.max = 30)
km1$size
Z <-matrix(0, nNodes, k )
for ( i in 1:k){ Z[km1$cluster ==i, i ] <- 1}
NZ <- Z %*% Diagonal(k, km1$size^(-1)) 
Dmat <- as.matrix(dist(x = x1))
Bmat <- t(NZ) %*% Dmat %*% NZ


ggplot(melt(as.matrix(Bmat)), 
       aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
    geom_tile()+ labs(title = "Avg.Distances of Points in/out Cluster(K-means)") + xlab("") + ylab("")

plt.centers(x1, km1$cluster)
plt.mds(mds_fit$points, km1$cluster, xlabel = "MDS Coordinate 1", 
        ylabel = "MDS Coordinate 2", main_title = "clusters in low dimension (Kmeans)")



## hirachical clustering
stree <- hclust(dist(x1),method  ="complete")
plot(stree, labels = rep("",nNodes), ylab ="Distance", 
     main="Complete Linkages")
m <- length(stree$height)
k = 5 # number of clusters
ht <- 0.5*(stree$height[m-k+1]+stree$height[m-k+2]); 
abline(h=ht,lty=2, col ="red")
hlabs <- cutree(stree, h =  ht); table(hlabs)
plt.mds(mds_fit$points, hlabs, xlabel = "MDS Coordinate 1", 
        ylabel = "MDS Coordinate 2", main_title = "clusters in low dimension(Hierachical)")



### Gaussian mixture clustering, up to 9 clusters by default
library(mclust)
mclus <- Mclust(x1, G = k )  
mlabs<- mclus$classification
plot(x =mds_fit$points[,1], y = mds_fit$points[,2], xlab="Coordinate 1", ylab="Coordinate 2",  type = 'n', main = "Hierachical Clustering")
text(x= mds_fit$points[,1], y = mds_fit$points[,2], labels = mlabs, col = mlabs, cex=.7)

plt.centers(x1, labs = mlabs )
plt.mds(mds_fit$points, mlabs, xlabel = "MDS Coordinate 1", 
        ylabel = "MDS Coordinate 2", main_title = "clusters in low dimension (GMM)")



##similarity
# S <- x1%*%t(x1)
distM <-as.matrix(dist(x1))
s = quantile(distM, 0.05)
S <- exp(- distM/s); diag(S) <- 0
D <- rowSums(S)
L <- Diagonal(nNodes, (1+D)^(-1/2)) %*% S%*% Diagonal(nNodes, (1+D)^(-1/2))
svd1 <- irlba::irlba(L, nv = 20)
plot(svd1$d)
k = 6
U <- svd1$u[,1:k]; rowN <- sqrt(rowSums(U*U) +1e-6); 
U1 <- Diagonal(nNodes, rowN^(-1)) %*% U
km2 <- kmeans(U1, k, iter.max = 30, nstart = 30 )

plt.mds(mds_fit$points, km2$cluster, xlabel = "MDS Coordinate 1", 
        ylabel = "MDS Coordinate 2", main_title = "clusters in low dimension")
plt.mds(U1[,2:3], km2$cluster, xlabel = "SC -2 ", 
        ylabel = "SC-3", main_title = "clusters in low dimension(SC-weighted)")

plt.centers(x1, km2$cluster)



#spectral clustering -- construct graph
distM <-as.matrix(dist(x1)); s = quantile(distM, 0.05)
S <- exp(- distM/s); diag(S) <- 0
A <- matrix(0, nNodes, nNodes); 
eps = quantile(S, 0.96) # 95% sparse, 50 edges
for( i in 1:nNodes ){
    #A[i, S[i,]>eps] <- 1
    A[i, order(-S[i,])[1:10]]<-1
}
diag(A) <- 0
A <- 1* (A+t(A)>0)
D <- rowSums(A)
L <- Diagonal(nNodes, (D+1)^(-1/2)) %*% A %*% Diagonal(nNodes, (D+1)^(-1/2))
svd1 <- irlba(L, nv = 20)
plot(svd1$d); abline(v = 6, col ="red", lty=2)
k=6
U <- svd1$u[,1:k]; rowN <- sqrt(rowSums(U*U) +1e-6); U1 <- Diagonal(nNodes, rowN^(-1)) %*% U
km2 <- kmeans(U1, k, iter.max = 30, nstart = 30 )

plt.centers(x1, km2$cluster)
plt.mds(mds_fit$points, km2$cluster, xlabel = "MDS Coordinate 1", 
        ylabel = "MDS Coordinate 2", main_title = "clusters in low dimension(SC-kernel-truncated)")

plt.mds(U1[,2:3], km2$cluster, xlabel = "2nd Eigenvector",
        y= "3rd Eigenvector", main_title = "clusters in low dimension(SC-kernel-truncated)")



## dbscan
# dbscan(x, eps, minPts = 5, weights = NULL,
#        borderPoints = TRUE, search = "kdtree", bucketSize = 10,
#        splitRule = "suggest", approx = 0, ...)
# eps :size of the epsilon neighborhood.
# minPts
library(dbscan)
x.pca <- princomp(x1)
db <- dbscan(x.pca$scores[,1:4], eps = 0.5)
db
db <- dbscan(x.pca$scores[,1:4], eps = 0.4)
db
db <- dbscan(x.pca$scores[,1:4], eps = 0.3)
db
db <- dbscan(x.pca$scores[,1:4], eps = 0.2)
db
db <- dbscan(x.pca$scores[,1:4], eps = 0.1)
db
plt.centers(x1, db$cluster)
plt.mds(mds_fit$points, db$cluster, xlabel = "MDS Coordinate 1", 
        ylabel = "MDS Coordinate 2", main_title = "clusters in low dimension(DB-SCAN)")

