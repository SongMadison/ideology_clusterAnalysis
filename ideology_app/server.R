rm(list =ls())
#setwd("~/Stat/ideology_study/code//ideology_app/")

library(ggplot2); 
library(mclust)
library(gridExtra)
library(grid)
library(poLCA)
#library(Matrix)

load("data1.RData") 
# regenerated on July 6, 2017, added DEM, GOP subsets
# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled2; x2 is PCA of x1 with 8 components

vplayout <- function(x, y) viewport(layout.pos.row = x, 
                                    layout.pos.col = y)

partClustering <- function(X, clus, percent){
    #part clustering, p_clus
    if (percent < 1-0.01){
        p_clus <- rep(NA, nrow(X))
        for (i in 1:length(unique(clus))){
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

calculateCenters <- function(x1, labs){
    x1 <- x1[!is.na(labs),]
    labs <- labs[!is.na(labs)]
    x1 = as.matrix(x1)
    k = length(unique(labs))
    m <- dim(x1)[2]
    nNodes <- length(labs)
    Z <- membershipM(labs)
    NZ <- Z %*% diag(colSums(Z)^(-1))
    centers <- t(NZ)%*%x1
    return (centers)
}



source("../ideology_functions.R")


# originalData, missing_ids, data1, x1, mds_fit, x2
# some glabal variables
nNodes <- dim(data1)[1]
nVar <- dim(data1)[2]
partylabel <- c("Democrats-1024","Republicans-485","allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                "leans-527" ,"independent-483","Extened_independent-1010",
                "fulldata-2549")


shinyServer(function(input, output) {
    
    
    # boxplot for scaling
    createBoxplot1 <- reactive( {
        par(mar	= c(3, 8, 4, 2) + 0.1)
        boxplot(data1, horizontal = T, las = 1, main = "before scaling")
    })
    output$boxplot1 <- renderPlot( createBoxplot1() )
    
    createBoxplot2 <- reactive({
        par(mar	= c(3, 8, 4, 2) + 0.1)
        boxplot(as.matrix(x1), horizontal = T, las = 1, main = "after scaling, mean = 0, sd =1") 
    })
    output$boxplot2 <- renderPlot( createBoxplot2() )
    
    #correlation plot
    createCorr <- reactive({
        cov <- cor(data1)
        cov[ row(cov) < col(cov)] <- 0
        balloon.plot(cov, xlabel = colnames(data1), 
                     ylabel = colnames(data1)
                   ,main ="correlation coefficients among variables")
    })    
    output$correlation <- renderPlot( createCorr() )
    output$comments <- renderText({
        paste0(
"Methods:
Step 1: transform the 17 scores:
1) scale the columns such that all the scores are within range [0,1]
2) scale the rows by substracting the row-means and divied by the row standard  deviation (+0.1)
3) scale the columns of the 17 variables so that each variable has mean = 0 and standard deviation = 1

Step 2: Kmeans algorithms / other algorithms will be applied to slected data sets and the centers of each cluster and grand mean are displayed 

Note:
1, a lot of positive correlation coefficients, like libertarian & authoritarian,govtrust & conspiracy, collectivism & individualism. Due to questions asked.
2, Will PCA make give any better results?
3, Others? \n ")
        
    })
    
  
    
    clustering <- reactive({
        
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            #x2 <- dat$x2
        }
        
        if ( input$method == "kmeans"){
            #if (input$transform == "standardization+PCA"){x1 <- x2}
            set.seed(100)
            km = kmeans(x1, input$clusters, nstart =  100, iter.max = max(30,input$clusters*5))
            # fix the label for fix number of clusters, order by libertarian
            ord <- order(-km$centers[,1])
            km = kmeans(x1, centers = km$centers[ord,],  iter.max = 1) # fixed the order
            clus = km$cluster
        }else if ( input$method == "hierachical"){
            #if (input$transform == "column standardization only") {x1 <- x2}
            stree <- hclust(dist(x1), method  = input$link_method)
            m <- length(stree$height)
            k <- input$clusters # number of clusters
            ht <- 0.5*(stree$height[m-k+1]+stree$height[m-k+2])
            clus <- cutree(stree,h=ht)
           
        }else if( input$method == "GMM"){
            #if (input$transform == "standardization+PCA") x1 <- x2
            set.seed(123) #
            mclus <- Mclust(x1, G = input$clusters ) # fits up to 9 clusters by default 
            clus = mclus$classification
        }else if (input$method == "LCA"){
            set.seed(123)
            x1_lca <- apply(x1, MARGIN = 2, FUN = function(x) numerical_to_factor(x, 5))
            x1_lca <- data.frame(x1_lca)
            f = as.matrix(x1_lca)~1
            lca <- poLCA(formula = f, data = x1_lca, nclass = input$clusters)
            clus = lca$predclass
        }
        
        p_clus = clus
        if ( input$percentDownload  != '100%'){
            #part clustering, p_clus
            percent = sub(pattern = '(\\d+).*','\\1', input$percentDownload)
            percent = as.integer(percent)/100
            p_clus <- partClustering(x1, clus, percent)
        }
        res <- list (clus = clus, p_clus = p_clus)
        return( res )
    })
    
    
    #show the table of cluster size
    output$clust_size <- renderTable( 
            cbind(cluster_id = 1:input$clusters, 
                  size = table(clustering()$clus) )
    )
    
    output$clust_size_p <- renderTable( 
        
        dat <- cbind(cluster_id = 1:input$clusters, 
              size = table(clustering()$p_clus) )
 
    )
    output$membership_csv <- downloadHandler(
        filename = function() {
            "membership.csv"
            #paste(input$member, '.csv', sep='')
        },
        content = function(file){
            write.csv(membership_vec(), file, row.names = F)
        }
    )
    membership_vec <- reactive({
        i <- which(partylabel == input$`data set`)
        #check is fulldata or not
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            idx <- dat$idx
            res <- list()
            res$id<- 1: nrow(originalData)
            res$membership <- rep(NA, nrow(originalData))
            res$membership[idx] <- clustering()$clus
            return(data.frame(res))
        }else{
            res$membership[-missing_ids] <- clustering()$clus
            return(data.frame(res))
        }
        
    })
    
    output$partMembership_csv <- downloadHandler(
        filename = function() {
            paste("partmembership",input$percentDownload, '.csv', sep='')
        },
        content = function(file){
            write.csv(partMembership_vec(), file, row.names = F)
        }
    )
    partMembership_vec <- reactive({
        res <- list()
        res$id<- 1: nrow(originalData)
        res$membership <- rep(NA, nrow(originalData))
        
        i <- which(partylabel == input$`data set`)
        #check is fulldata or not
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            idx <- dat$idx
            res$membership[idx] <- clustering()$p_clus
            return(data.frame(res))
        }else{
            #full data set
            res$membership[-missing_ids] <- clustering()$p_clus
            return(data.frame(res))
        }
    })
    
    
    
    
    createTreePlot <- reactive({
        
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            #x2 <- dat$x2
            nNodes = nrow(x1)
        }
        
        #if (input$transform == "standardization+PCA") x1 <- x2
        stree <- hclust(dist(x1), method  = input$link_method)
        plot(stree, labels = rep("",nNodes), ylab ="Distance", 
             main= "Hierachical Clustering")
        m <- length(stree$height)
        k = input$clusters
        ht <- 0.5*(stree$height[m-k+1]+stree$height[m-k+2]); 
        abline(h=ht,lty=2, col ="red")
    })
    
    output$h_tree_plot <- renderPlot({
        if (input$method =="hierachical"){
            print (createTreePlot())
        }
    })
    
    #line plot
    createCenterPlot1 <- reactive({
        
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            #x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        
        cluster_result <- clustering()
        clus <- cluster_result$clus
        p_clus  <- cluster_result$p_clus
        
        plot1 <-  plt.centers(x1, clus)
        plot2 <-  plt.centers(x1, p_clus)
        grid.arrange(plot1, plot2, ncol = 2)
    })
    
    #ballplot
    createCenterPlot2 <- reactive({
        
        dat_id <- which(partylabel == input$`data set`)
        if (dat_id < length(partylabel) ){
            dat <- scaled2[[dat_id]]
            x1 <- dat$x1
            #x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        cluster_result <- clustering()
        clus <- cluster_result$clus
        p_clus  <- cluster_result$p_clus
        
        centers <- calculateCenters(x1, clus)
        plot1 <- balloon.plot(centers,
            ylabel = 1:length(unique(clus[!is.na(clus)])),
            xlabel = colnames(x1))+
            ggtitle("mean value for each cluster at each variable")
        
        centers2 = calculateCenters(x1, p_clus)
        plot2 <- balloon.plot(centers2, 
            ylabel = 1:length(unique(p_clus[!is.na(p_clus)])),
            xlabel = colnames(x1)) +
            ggtitle("mean value for each cluster at each variable")
        
        grid.arrange(plot1, plot2, ncol = 1)
    })
    #
    createCenterPlot3 <- reactive({
        
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            #x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        
        cluster_result <- clustering()
        clus <- cluster_result$clus
        p_clus  <- cluster_result$p_clus
        Z = membershipM(clus)
        size <- colSums(Z)
         
        if (input$clusters >2){
            centers <- diag(size^(-1))%*%t(Z)%*%x1
            d_c <- dist(centers) # euclidean distances between the rows
            fit <- cmdscale(d_c, eig=TRUE, k=2)
            tmp <- fit$points[,1:2]; colnames(tmp) <- c("z1","z2")
        }else{
            tmp <- diag(size^(-1))%*%t(Z)%*%mds_fit$points[,1:2]; 
            colnames(tmp) <- c("z1","z2")
        }
        tmp <- data.frame(tmp, cluster = 1:input$clusters) 
        tmp$cluster <- as.factor(tmp$cluster)
        
        ggplot(tmp, aes(x = z1, y = z2, colour = cluster) )+
            geom_text(aes(label = cluster),  size =10) + 
            labs( x = "Coordinate 1", y = "Coordinate 2", title = "cluster centers")
        
   })
    
    output$centers_plot1 <- renderPlot({ 
        print (createCenterPlot1())   
    })
    output$centers_plot2 <- renderPlot({
        print (createCenterPlot2())  
    })
    output$centers_plot3 <- renderPlot({
        print (createCenterPlot3())  
    })
    
    createMdsPlot <- reactive({
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            #x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        cluster_result <- clustering()
        clus <- cluster_result$clus
        p_clus  <- cluster_result$p_clus
        p_clus[is.na(p_clus)] <- 0; p_clus <- as.character(p_clus)
        p_clus[p_clus == '0'] = 'o'
        plt.mds(mds_fit, labs = clus, text_label = p_clus)
    })
    
    output$mds_plot1 <- renderPlot({print (createMdsPlot()$p12)})
    output$mds_plot2 <- renderPlot({print (createMdsPlot()$p13)})
    output$mds_plot3 <- renderPlot({print (createMdsPlot()$p23)})
    
    
    

    

    
    
})