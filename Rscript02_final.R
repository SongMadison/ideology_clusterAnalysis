rm(list =ls() )
library(foreign) # read.spss
library(Matrix)
library(irlba)
library(ggplot2)
library(reshape)
source("./ideology_functions.R")
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
for( i in 1:length(headers)){
    hist(x1[,i] ,breaks = 20, main = headers[i])
    abline(v=median(x1[,i]), col="red")
    cat ("Press [enter] to continue or [no|No] key to break")
    line <- readline()
    if (line == 'no' | line == 'No'){
        break
    }
}


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


p1 <- ggplot(melt(as.matrix(Bmat)), 
       aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
    geom_tile()+ labs(title = "Avg.Distances of Points in/out Cluster(K-means)") + xlab("") + ylab("")
p1
p2 <- plt.mds(mds_fit, km1$cluster, xlabel = "MDS Coordinate 1", 
                  ylabel = "MDS Coordinate 2", 
                  main_title = "clusters in low dimension (Kmeans)")
p2

plt.centers(x1, km1$cluster)

