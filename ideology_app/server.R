rm(list =ls())
set.seed(100)
source("ideology_functions.R")
load("data3.RData") 
# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled2; x2 is PCA of x1 with 8 components
library(ggplot2); library(Matrix); library(irlba); library(reshape2);
library(mclust)



# originalData, missing_ids, data1, x1, mds_fit, x2

nNodes <- dim(data1)[1]
nVar <- dim(data1)[2]


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
       "1, a lot of positive correlation coefficients, like libertarian & authoritarian,
           govtrust & conspiracy, collectivism & individualism. Due to questions asked.
       2 Will PCA make give any better results?
       3,")
        
    })
    
#partylabel
    partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
    "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
    
    
    clustering <- reactive({
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                         "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
        }
        
        if ( input$method == "kmeans"){
            if (input$transform == "standardization+PCA"){x1 <- x2}
            km = kmeans(x1, input$clusters, nstart =  100, iter.max = 30)
            # fix the label for fix number of clusters, order by libertarian
            ord <- order(-km$centers[,1])
            km = kmeans(x1, centers = km$centers[ord,],  iter.max = 1) # fixed the order
            return( km$cluster)
        }
        if ( input$method == "hierachical"){
            if (input$transform == "standardization+PCA") {x1 <- x2}
            stree <- hclust(dist(x1), method  = input$link_method)
            m <- length(stree$height)
            k <- input$clusters # number of clusters
            ht <- 0.5*(stree$height[m-k+1]+stree$height[m-k+2])
            clus <- cutree(stree,h=ht)
            return(clus)
        }
        if( input$method == "GMM"){
            if (input$transform == "standardization+PCA") x1 <- x2
            mclus <- Mclust(x1, G = input$clusters ) # fits up to 9 clusters by default 
            return(mclus$classification)
        }
    })
    
    
    #show the table of cluster size
    output$clust_size <- renderTable( 
            cbind(cluster_id = 1:input$clusters, 
                  size = table(clustering()) )
    )
    
    output$downloadData <- downloadHandler(
        filename = function() { 
            "membership.csv"
        },
        content = function(file){
            write.csv(membership_vec(), file, row.names = F)
        }
    )
    membership_vec <- reactive({
        
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            idx <- dat$idx
            res <- list()
            res$id<- 1: nrow(originalData)
            res$membership <- rep(NA, nrow(originalData))
            res$membership[idx] <- clustering()
            return(data.frame(res))
        }
        
        res <- list()
        res$id<- 1: nrow(originalData)
        res$membership <- rep(NA, nrow(originalData))
        res$membership[-missing_ids] <- clustering()
        return(data.frame(res))
    })
    
    createTreePlot <- reactive({
        
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
            nNodes = nrow(x1)
        }
        
        if (input$transform == "standardization+PCA") x1 <- x2
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
        
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        labs <- clustering()
        plt.centers(x1, labs)
    })
    
    #ballplot
    createCenterPlot2 <- reactive({
        
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        labs <- clustering(); 
        k <- input$clusters
        Z <- membershipM(labs)
        size <- colSums(Z)
        NZ <- Z %*% Diagonal(k, size^(-1))
        centers <- t(NZ) %*% x1
        balloon.plot(centers, 
                     ylabel = paste0(1:k,"-",size),
                     xlabel = colnames(x1))+
            ggtitle("mean value for each cluster at very variable")
    })
    #
    createCenterPlot3 <- reactive({
        
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        
        labs <- clustering()
        Z = membershipM(labs)
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
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        labs <- clustering()
        plt.mds(mds_fit, labs)
    })
    
    createCenters <- reactive({
        partylabel <- c("allPartisans-2066","strongPartisans-535", "Partisans-1509",          
                        "leans-527" ,"independent-483","Extened_independent-1010","fulldata-2561")
        i <- which(partylabel == input$`data set`)
        if (i < length(partylabel) ){
            dat <- scaled2[[i]]
            x1 <- dat$x1
            x2 <- dat$x2
            mds_fit <- dat$mds_fit
        }
        labs <- clustering()
        plt.centers (x1, labs)
    })
    output$mds_plot1 <- renderPlot({print (createMdsPlot()$p12)})
    output$mds_plot2 <- renderPlot({print (createMdsPlot()$p13)})
    output$mds_plot3 <- renderPlot({print (createMdsPlot()$p23)})
})