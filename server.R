rm(list =ls())
set.seed(100)
source("ideology_functions.R")
load("data.RData") 
# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled; x2 is PCA of x1 with 8 components
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
        boxplot(as.matrix(x1), horizontal = T, las = 1, main = "after scaling, mean =0, sd =1") 
    })
    output$boxplot2 <- renderPlot( createBoxplot2() )
    
    #correlation plot
    createCorr <- reactive({
        cov <- cor(data1)
        cov[ row(cov) < col(cov)] <- 0
        balloon.plot(cov, rowlabel = colnames(data1), collabel = colnames(data1)
                 ,main ="correlation coefficients among variables")
    })    
    output$correlation <- renderPlot( createCorr() )
    output$comments <- renderText({
        "1, a lot of positive correlation coefficients, like libertarian & authoritarian,
        govtrust & conspiracy! what do you think? \n2 Will PCA make give any better results?"
        
    })
    

    my_hclust <- reactive({
        stree <- hclust(dist(x1), method  =input$link_method)
        m <- length(stree$height)
        k <- input$clusters # number of clusters
        ht <- 0.5*(stree$height[m-k+1]+stree$height[m-k+2])
        clus <- cutree(stree,h=ht)
        clus
    })
    
    clustering <- reactive({
        if ( input$method == "kmeans"){
            if (input$transform == "standardization+PCA") x1 <- x2
            km = kmeans(x1, input$clusters, nstart =  30, iter.max = 30)
            # fix the label for fix number of clusters, order by libertarian
            ord <- order(-km$centers[,1])
            km = kmeans(x1, centers = km$centers[ord,], nstart =  30, iter.max = 30)
            return( km$cluster)
        }
        if ( input$method == "hierachical"){
            if (input$transform == "standardization+PCA") x1 <- x2
            return(my_hclust())
        }
        if( input$method == "GMM"){
            if (input$transform == "standardization+PCA") x1 <- x2
            mclus <- Mclust(x1, G = input$clusters ) # fits up to 9 clusters by default 
            return(mclus$classification)
        }
    })
    
    
    
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
        res <- list()
        res$id<- 1:nrow(originalData)
        res$membership <- rep(NA, nrow(originalData))
        res$membership[-missing_ids] <- clustering()
        return(data.frame(res))
    })
    
    createTreePlot <- reactive({
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
        }else{
            NULL
        }
    })
    
    
    createCenterPlot <- reactive({
            labs <- clustering()
            plt.centers(x1, labs)
    })
    output$centers_plot <- renderPlot({
        print (createCenterPlot())  
    })
    
    createCenterPlot2 <- reactive({
        labs <- clustering(); 
        k = length(unique(labs))
        size <- table(labs)
        Z <- membershipM(labs)
        NZ <- Z %*% Diagonal(k, size^(-1))
        centers <- t(NZ) %*% x1
        balloon.plot(t(centers), 
                     collabel = paste0(1:k,"-",size),
                     rowlabel = colnames(x1))+
            ggtitle("mean value for each cluster at very variable")
    })
    output$centers_plot2 <- renderPlot({
        print (createCenterPlot2())  
    })
    
    createMdsPlot <- reactive({
        labs <- clustering()
        plt.mds(mds_fit, labs)
    })
    
    output$mds_plot1 <- renderPlot({print (createMdsPlot()$p1)})
    output$mds_plot2 <- renderPlot({print (createMdsPlot()$p2)})
    output$mds_plot3 <- renderPlot({print (createMdsPlot()$p3)})
})