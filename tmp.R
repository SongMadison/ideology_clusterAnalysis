


centers1 <- t(NZ) %*% as.matrix(originalData[-missing_ids,2:18])
centers1 <- apply(centers1, MARGIN = 2, FUN = round, digits = 3)
centers1 <- data.frame(cluster =1:9, centers1)
write.csv(centers1, file="../data/non-standardized_means_with_cluster.csv", row.names = F)
