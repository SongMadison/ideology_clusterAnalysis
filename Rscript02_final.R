rm(list =ls() )
library(foreign) # read.spss
library(Matrix)
library(irlba)
library(ggplot2)
library(reshape)
source("./ideology_app/ideology_functions.R")
load('data.RData')
Dat = read.spss("../MCRC1617_CLUSTER ID AND DESCRIPTIVE VARS.sav", 
                to.data.frame=TRUE)


# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled; x2 is PCA of x1 with 8 components



nNodes <- dim(x1)[1]
nVar <- dim(x1)[2]

###Exploratory analysis
(headers <- colnames(x1))

# asymmetry
i=1
hist(x1[,i] ,breaks = 20, main = headers[i])
abline(v=median(x1[,i]), col="red")

par(mar	= c(3, 8, 4, 2) + 0.1)
boxplot(data1, horizontal = T, las = 1, main = "before scaling")
boxplot(x1, horizontal = T, las = 1, main = "after scaling, mean = 1, sd = 1")
#boxplot(as.matrix(x1), horizontal = T, las = 1, main = "after scaling, mean = 1, sd = 1")
par(mar	= c(5, 4, 2, 3) + 0.1)

#correlation among variables
cov <- cor(data1)
cov[ row(cov) < col(cov)] <- 0; which(abs(cov) > 0.5, arr.ind = T)
balloon.plot(cov, rowlabel = headers, collabel = headers)



# k - means  how many clusters?
if (FALSE){
    k = 1:20
    wss <- rep(0,20)
    xm <- colMeans(x1)
    wss[1] = sum(x1^2)- nrow(x1)*sum(xm^2)
    
    for(i in 2:20){
        model <- kmeans(x1, centers = i, nstart =100, iter.max = 30)
        wss[i] <- sum(model$withinss)
    }
    
    setEPS()
    postscript("scree.eps")
    par(mar = c(5,4,1,1))
    plot(wss/wss[1], type ="b", xlab = "number of clusters, k", 
         ylab = "proption of within cluster Sum of Squares")
    text(9.8,0.53,labels = "k=9")
    abline(v=9, col ="red", lty =2)
    dev.off()
}





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




D1 <- as.matrix( round(dist(km1$centers),2)  )
min(D1[col(D1)!= row(D1)])
mean(D1[col(D1)!= row(D1)])
ggplot(melt(as.matrix(D1)), 
       aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
    geom_tile() +title("Distance Among K-means Centers") + xlab("") + ylab("")
#D1[row(D1)<col(D1)] <- 0
write.csv(D1, file = "../D1_mat.csv")
balloon.plot(D1) + ggtitle("distances among the centers by K-means") 

pr <- princomp(km1$centers,cor = F, scores = TRUE) # doesnot work
svd <- svd(km1$centers)
km_mds <- cmdscale(dist(km1$centers), eig =T,  k=2)
plt.cluster(km_mds$points[,1:2], labs = 1:9, xlabel = "MDS comp 1", 
            ylabel = "MDS comp 2") 
plt.cluster(svd$u[,1:2]%*%diag(svd$d[1:2]), labs = 1:9, xlabel = "PCA 1", 
            ylabel = "PCA  2") 
mds_x1 <- cmdscale(dist(x1),k = 2)
plt.data <- diag(colSums(Z)^(-1)) %*% t(Z) %*% mds_fit$points[,1:2]
plt.cluster(plt.data, labs = 1:9)

dist(km_mds$points)
dist(km1$centers)



D2 <- as.matrix(  round(Bmat, 2) )
colnames(D2) <- 1:9; rownames(D2) <- 1:9
(D2.min <- min(D2[col(D2)!= row(D2)]) )#4.59
(D2.mean <- mean(D2[col(D2)!= row(D2)]) )#4.59
mean(diag(D2))
max(diag(D2)); which(D2[col(D2)!= row(D2)] == D2.min, arr.ind = T)
which(diag(D2) > D2.min)
mean(D2[col(D2)!= row(D2)])
mean(diag(D2))
#D2[row(D2)<col(D2)] <- 0
write.csv(D2, file = "../D2_mat.csv")
balloon.plot(D2) + ggtitle( "mean distance among points within/across clusters")

D2_mds <- cmdscale(D2, eig = T, k=2)
plt.cluster(D2_mds$points[,1:2], labs =  1:9)
p1 <- ggplot(melt(as.matrix(Bmat)), 
       aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
       geom_tile()+ labs(title = "Avg.Distances of Points in/out Cluster(K-means)") + xlab("") + ylab("")
p1

image(as.matrix(Bmat))
balloon.plot(A = as.matrix(Bmat))
p2 <- plt.cluster(mds_fit$points[,1:2], km1$cluster, xlabel = "MDS Coordinate 1", 
                  ylabel = "MDS Coordinate 2", 
                  main_title = "clusters in low dimension (Kmeans)")
p2

plt.cluster(mds_fit$points[,c(1,3)], km1$cluster, xlabel = "MDS Coordinate 1", 
                  ylabel = "MDS Coordinate 2", 
                  main_title = "clusters in low dimension (Kmeans)")
plt.cluster(mds_fit$points[,c(2,3)], km1$cluster, xlabel = "MDS Coordinate 1", 
            ylabel = "MDS Coordinate 2", 
            main_title = "clusters in low dimension (Kmeans)")


plt.centers(x1, km1$cluster)
balloon.plot(km1$centers, xlabel  = headers)

centers1 <- t(NZ) %*% as.matrix(data1)

balloon.plot(centers1, xlabel  = headers)


library(grDevices)


Plot_ConvexHull<-function(xcoord, ycoord, lcolor, thick=1){
    hpts <- chull(x = xcoord, y = ycoord)
    hpts <- c(hpts, hpts[1])
    lines(xcoord[hpts], ycoord[hpts], col = lcolor, lwd = thick)
}

Plot_ConvexHull(mds_f)

