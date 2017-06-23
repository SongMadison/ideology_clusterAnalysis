rm(list =ls())
# row and column normalization
# 3rd stage cluster analysis

full <- memisc::as.data.set(
    memisc::spss.system.file('../MCRC1617_CLUSTER ID AND DESCRIPTIVE VARS.sav'))
full <- as.data.frame(full)
dim(full)


#unscaled, full data set, self-identified party
originalData <- full[,c(85,94:110)]

# missingness
missing <- lapply( originalData, function(x) which(is.na(x)) ) 
missing_ids <- unique( unlist(missing))
cat(length(missing_ids),  "out of", nrow(originalData), "contains missing values") #21
missed <- originalData[missing_ids,]


#complete cases
data1 <- originalData[-missing_ids,]
party <- data1$party_iden
data1 <- data1[,2:18]
x1 = data1


#should I do change range for each variables ?
rg_c <- t(apply(data1, MARGIN =2 , range))
for (j in 1:ncol(data1)){
    x1[,j]  = (data1[,j] - rg_c[j,1])/(rg_c[j,2] - rg_c[j,1])
}


# row means
rg_r <- t(apply(x1, MARGIN = 1, range))
rmeans <- apply(x1, MARGIN = 1, mean)
rsd <- apply(x1, MARGIN = 1, sd)
for ( i in 1:nrow(data1)){
    x1[i,] <- (x1[i,]- rmeans[i])/(rsd[i]+0.1)
}

#column normalization
x1 = scale(x1)


scaled = x1
d_x1 <- dist(x1)                   # euclidean distances between the rows
mds_fit <- cmdscale(d_x1 ,eig=TRUE, k=3)

pca <- princomp(x1)
plot(pca)
x2 <- pca$scores[,1:8]
dat0 <- list(idx= setdiff(1:nrow(originalData), missed),
             x1 = x1,
             mds_fit = mds_fit,
             x2 = x2
)


partyid <- names(table(party))
allPartisans <- setdiff( 1: nrow(data1), which( (party == 'Independent') 
                                                + (party == 'Other political party') > 0 ) )
strongPartisans <- which((party == 'Strong Democrat') + (party == 'Strong Republican') > 0)
Partisans <- which((party == 'Strong Democrat') + (party == 'Strong Republican') + 
                       (party == 'Democrat') + (party == 'Republican') > 0)
leans <- which((party == 'Lean Republican') + (party == 'Lean Democrat') > 0)
independent <- which(party == 'Independent')
Extened_independent <- c(leans, independent)

#partylabel <- paste0(partyid, '-', table(party))
partylabel <- paste0(c("allPartisans","strongPartisans","Partisans",
                       "leans", "independent","Extened_independent"),'-',
                     c(length(allPartisans),length(strongPartisans), length(Partisans),
                       length(leans), length(independent), length(Extened_independent) ))
idxs <- list(allPartisans,strongPartisans,Partisans ,leans,independent,Extened_independent )
scaled2 <- list()
originalIndex <- setdiff(1:nrow(originalData), missing_ids)  #removing missing
for (i in 1:length(partylabel)){
    #i = 1
    idx = originalIndex [idxs[[i]]]
    x1 <- scaled[idxs[[i]],]
    d_x1 <- dist(x1)   # euclidean distances between the rows
    mds_fit <- cmdscale(d_x1 ,eig=TRUE, k=3)
    pca <- princomp(x1)
    x2 <- pca$scores[,1:8]
    dat <- list( idx = idx, x1 = x1, mds_fit = mds_fit, x2 = x2)
    scaled2[[i]] <- dat
}
scaled2[[9]] <- dat0
x1 <- dat0$x1; mds_fit = dat0$mds_fit; x2 = dat0$x2
save(originalData, data1, missing_ids, mds_fit, x1, x2, scaled2, file = "./ideology_app/data1.RData")
