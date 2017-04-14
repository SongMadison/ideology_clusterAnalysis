rm(list =ls())



library(memisc)  #use this one  foreign can work, too

# cleaning data
# read  .sav, removing missing  -- > data1 , standardize the data -- > x1
originalData <- memisc::as.data.set(memisc::spss.system.file('../MCRC1617_CLUSTER VARS.sav'))
originalData <- as.data.frame(originalData)

missing <- lapply( originalData, function(x) which(is.na(x)) ) 
missing_ids <- unique( unlist(missing))
cat(length(missing_ids),  "out of", nrow(originalData), "contains missing values") #21
data1 <- originalData[-missing_ids,]

x1<- scale(data1)
# x1 <- apply(data1, MARGIN = 2, function(x) x/max(x))   # [0,1] standardization
# cbind(mean = colMeans(x1), sd = apply(x1, 2, sd))

d_x1 <- dist(x1)                   # euclidean distances between the rows
mds_fit <- cmdscale(d_x1 ,eig=TRUE, k=3)

pca <- princomp(x1)
plot(pca)
x2 <- pca$scores[,1:8]

save(originalData, missing_ids, data1, x1, mds_fit, x2, file = "data.RData")
## after remove  missing rows and standard the columns
