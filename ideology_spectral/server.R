rm(list =ls())
set.seed(100)
source("ideology_functions.R")

load("result_sc.RData")
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
    
    scree_plot <- reactive({
        plot(svd_L$d, main="singular values")
        abline(v =3, col = 'red', lty = 2)
        abline(v =5, col = 'red', lty = 2)
        abline(v =11, col = 'red', lty = 2)
    })
    output$scree_plot = renderPlot({ 
        scree_plot() })
    
    clustering <- reactive({
        k = input$clusters
        cluster_sc[,k]
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
        res <- list()
        res$id<- 1: nrow(originalData)
        res$membership <- rep(NA, nrow(originalData))
        res$membership[Extened_independent] <- clustering()
        return(data.frame(res))
    })
    
    #line plot
    createCenterPlot1 <- reactive({
        labs <- clustering()
        plt.centers(data1, labs)
    })
    
    #ballplot
    createCenterPlot2 <- reactive({
        labs <- clustering(); 
        k <- input$clusters
        Z <- membershipM(labs)
        size <- colSums(Z)
        base = colMeans(data1)
        centers <- t(Z %*% Diagonal(k, size^(-1))) %*% as.matrix(data1)
        balloon.plot(centers, 
                     ylabel = paste0('c',1:k,"-",size),
                     xlabel = colnames(data1))+
            ggtitle("mean value for each cluster at very variable")
    })
    #
    createCenterPlot3 <- reactive({
        
        labs <- clustering()
        Z = membershipM(labs)
        size <- colSums(Z)
         
        if (input$clusters >2){
            centers <- diag(size^(-1))%*%t(Z)%*%as.matrix(data1)
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
        labs <- clustering()
        plt.mds(mds_fit, labs)
    })
    
    createCenters <- reactive({
        labs <- clustering()
        plt.centers (data1, labs)
    })
    output$mds_plot1 <- renderPlot({print (createMdsPlot()$p12)})
    output$mds_plot2 <- renderPlot({print (createMdsPlot()$p13)})
    output$mds_plot3 <- renderPlot({print (createMdsPlot()$p23)})
})