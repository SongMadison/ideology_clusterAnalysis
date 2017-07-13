

input = list()
input$clusters = 4
input$'data set' = "Extened_independent-1010"
input$method = 'kmeans'
input$percentDownload  = '50%' 


library(ggplot2); library(Matrix); library(irlba); library(reshape2);
library(mclust)
library(gridExtra)
library(grid)

load("data1.RData")
# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled2; x2 is PCA of x1 with 8 components

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

partClustering <- function(X, clus, percent){
    #part clustering, p_clus
    if (percent < 1-0.01){
        p_clus <- rep(NA, nrow(X))
        for (i in 1:input$clusters){
            idx = which(clus == i)
            if(length(idx) > 1){
                c = colMeans(X[idx,])
            }else{
                c = X[idx,]
            }
            d = apply(X[idx,], MARGIN =1, function(x) sqrt(sum((x-c)^2)))
            p_clus[ idx[ order(d)[1:ceiling(length(idx)*percent)] ] ] <- i
        }
    }else{
        p_clus = clus
    }
    return (p_clus)
}
source("ideology_functions.R")


nNodes <- dim(data1)[1]
nVar <- dim(data1)[2]
partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                "leans-527" ,"independent-483","Extened_independent-1010",
                "fulldata-2549")
