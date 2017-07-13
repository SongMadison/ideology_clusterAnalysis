

rm(list =ls())
#unscaled, full data set
load("./ideology_app/data2.RData")
source("./ideology_app/ideology_functions.R")


full <- memisc::as.data.set(
    memisc::spss.system.file('../MCRC1617_CLUSTER ID AND DESCRIPTIVE VARS.sav'))
full <- as.data.frame(full)
dim(full)


membership <- read.csv("membership.csv")
idx<- which(!is.na(membership$membership))
idx1 <- match(as.character(membership$id[idx]), rownames(x1))
x1 <- x1[idx1,]
dat <- membership[idx,]
dat <- data.frame(id = dat$id, cluster =dat$membership, x1)
dat$polengage <- full$polengage[idx]
dat$ideo_soc <- full$ideo_soc[idx]


k <- length(unique(dat$cluster))
dat$cluster = as.factor(dat$cluster)
dat$cluster <- factor(dat$cluster, levels = 1:k, 
                      labels = paste0('c',1:k, '-',table(dat$cluster)))


ggplot(dat, aes(ideo_soc,polengage)) +
        geom_jitter(width = 0.2, height = 0.2, 
                    aes(shape= cluster, color = cluster))+
      scale_color_manual(breaks = paste0('c',1:k, '-',table(dat$cluster)), 
                       values = c('#800080','#ff0000','#0000ff','#FFC0CB','#000000'))+
    labs(y = "political engagement", x = "")

        
ggplot(dat, aes(polengage)) +
    geom_bar(aes(fill = cluster))+
    facet_grid(.~cluster)+
    scale_color_manual(breaks = paste0('c',1:k, '-',table(dat$cluster)), 
                       values = c('#800080','#ff0000','#0000ff','#FFC0CB','#000000'))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
ggplot(dat, aes(ideo_soc,polengage)) +
    geom_jitter(width = 0.3, height = 0.2, aes(shape = cluster,color = cluster))+
    facet_grid(.~cluster)+
    scale_color_manual(breaks =paste0('c',1:k, '-',table(dat$cluster)) , 
          values = c('#800080','#ff0000','#0000ff','#FFC0CB','#000000'))+
    labs(y = "political engagement", x = "")+ theme(legend.position="none")+
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) 



Z <- membershipM(membership$membership[idx])
NZ <- Z%*% diag(colSums(Z)^-1)
dis <- dist(x1)
t(NZ)%*% as.matrix(dis) %*% NZ
s <- svd(x1)


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
    
    #setEPS()
    #postscript("scree.eps")
    par(mar = c(5,4,1,1))
    plot(wss/wss[1], type ="b", xlab = "number of clusters, k", 
         ylab = "proption of within cluster Sum of Squares")
    text(9.8,0.53,labels = "k=9")
    abline(v=4, col ="red", lty =2)
    #dev.off()
}


