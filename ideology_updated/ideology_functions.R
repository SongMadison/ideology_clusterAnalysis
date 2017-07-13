membershipM <- function(labs){
    k <- max(labs,na.rm = T); m <- length(labs)
    Z <- matrix(0, m, k)
    for(i in 1:k){
        Z[which(labs == i), i] <- 1 
    }
    return(Z)
}


# line plots of centers
plt.centers <- function(x1, labs){
    
    x1 = as.matrix(x1)
    k = length(unique(labs))
    m <- dim(x1)[2]
    nNodes <- length(labs)
    Z <- membershipM(labs)
    NZ <- Z %*% Diagonal(k, colSums(Z)^(-1)) 
    centers <- t(NZ)%*%x1
    base = colMeans(x1)
    centers = rbind(centers, base)
    data1 <- data.frame(cluster_id = rep(1:(k+1), times =ncol(x1)), 
                        variable = rep(1:ncol(x1), each = (1+k)),
                        values = as.vector(centers))
    data1$variable <- as.factor(data1$variable)
    data1$variable <- factor(data1$variable,levels = 1:ncol(x1), labels = colnames(x1) )
    data1$cluster <- factor(data1$cluster_id)
    data1$cluster <- factor(data1$cluster,1:(k+1), c(paste0('c',1:k,'-',colSums(Z)),"ave"))
    data1$cluster_id[data1$cluster_id ==(k+1)] <- 'x'
    ggplot(data = data1, aes(x = variable, y = values)) + 
        #geom_point(aes(color = cluster)) + 
        geom_line(aes(group = cluster, color = cluster), size = 0.3 )+
        geom_text(aes(label = cluster_id, color = cluster)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        labs(y = "", x ="") + 
        scale_color_manual(breaks = c(paste0('c',1:k,'-',colSums(Z)),"ave"), 
                           values = c(rainbow(k),'#000000'))
        #scale_color_manual(breaks = c(paste0('c',1:k,'-',colSums(Z)),"ave"),
        #                                       values=c(rainbow(k),'#000000'))
    
}

plt.cluster <- function(points, labs, 
                        xlabel = NULL, ylabel = NULL, size= 5, main_title = NULL){
    
    dat <- as.matrix(points[,1:2])
    k <- length(unique(labs))
    colnames(dat) <- c("z1","z2")
    dat <- data.frame(dat, cluster = labs)
    dat$cluster <- as.factor(dat$cluster)
    
    p1 <- ggplot(dat, aes(x = z1, y = z2, colour = cluster) )+
        geom_text(aes(label = cluster),  size= size) + 
        labs( x = xlabel, y = ylabel, title = main_title)
    p1 +theme(plot.title = element_text(hjust = 0.5))+
        scale_color_manual(breaks = 1:k, values=c(rainbow(k)))
}


# plot solution 
plt.mds <- function(fit_mds, labs, 
                    xlabel = NULL, ylabel = NULL, main_title = NULL){
    
    dat <- as.matrix(fit_mds$points[,1:3])
    k <- length(unique(labs))
    colnames(dat) <- c("z1","z2","z3")
    dat <- data.frame(dat, cluster = labs)
    dat$cluster <- as.factor(dat$cluster)

    p1 <- ggplot(dat, aes(x = z1, y = z2, colour = cluster) )+
          geom_text(aes(label = cluster),  size =3) + 
          labs( x = "Coordinate 1", y = "Coordinate 2", title = main_title)+
          theme(plot.title = element_text(hjust = 0.5))+
          scale_color_manual(breaks = 1:k, values=c(rainbow(k)))
    p2 <- ggplot(dat, aes(x = z1, y = z3, colour = cluster) )+
        geom_text(aes(label = cluster),  size =3) + 
        labs( x = "Coordinate 1", y = "Coordinate 3", title = main_title)+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_color_manual(breaks = 1:k, values=c(rainbow(k)))
    p3 <- ggplot(dat, aes(x = z2, y = z3, colour = cluster) )+
        geom_text(aes(label = cluster),  size =3) + 
        labs( x ="Coordinate 2", y = "Coordinate 3", title = main_title) + 
        theme(plot.title = element_text(hjust = 0.5))+
        scale_color_manual(breaks = 1:k, values=c(rainbow(k)))
    
    res = list(p12 = p1, p13=p2, p23 =p3)
    return (res)

}





#flattenMatrix to i,j, value
flattenMatrix <- function(adjM){
    n = dim(adjM)[1]
    m = dim(adjM)[2]
    data<- data.frame(list(
        row = rep(1:n, times = m),
        col = rep(1:m, each = n),
        value = as.vector(adjM)))
    data <- data[,c(2,1,3)]
    return (data)
}

balloon.plot <- function(A, xlabel = NULL, ylabel = NULL , main = NULL){
    
    nr <- nrow(A)
    A <- A[nr:1,]
    dat1 <- flattenMatrix(A)    #col, row, value
    names(dat1) <- c("xx", "yy", "value")
    dat1$xx <- as.factor(dat1$xx)
    dat1$yy <- as.factor(dat1$yy)
    
    
    if (length(which(A<0))>0){
        dat1$sign <- as.factor(1*(dat1$value>0))
        dat1$sign <-factor(dat1$sign, levels =c(1,0), labels = c(">0","<=0"))
        dat1$value <- abs(dat1$value)
    } 
    if (!is.null(xlabel)) dat1$xx <- factor(dat1$xx, levels = 1:ncol(A), xlabel)
    if (!is.null(ylabel)) {dat1$yy <- factor(dat1$yy, levels = 1:nrow(A), ylabel[nr:1])
    }else{
        dat1$yy <- factor(dat1$yy, levels = 1:nrow(A), nr:1)  #label with reverse name
    }
    #dat1$ <-  dat1$value > quantile(dat1$value, 0.9)
    p <- ggplot(dat1,  aes(x = xx, y= yy ))
    
    if (length(which(A<0))>0){
        #p1 <- p + geom_point(aes(size = value, colour = sign)) +xlab('')+ylab('')
        p1 <- p + geom_text(aes(label = myround(value,2), colour = sign, size =value))
    }else{
        #p1 <- p + geom_point(aes(size = value)) +xlab('')+ylab('')
        p1 <- p + geom_text(aes(label = myround(value,2),  size = value))
    }
    
    p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p1 <- p1 + theme(axis.text.y = element_text(angle = 0, hjust = 1))
    p1 + ggtitle(main) +xlab("")+ylab("") + 
        theme(plot.title = element_text(hjust = 0.5))
}

balloon.plot2 <- function(A, xlabel = NULL, ylabel = NULL , main = NULL){
    
    nr <- nrow(A)
    A <- A[nr:1,]
    dat1 <- flattenMatrix(A)    #col, row, value
    dat1 <- cbind(dat1,nr+1 - dat1[,2])
    names(dat1) <- c("xx", "yy", "value","row")
    dat1$xx <- as.factor(dat1$xx)
    dat1$yy <- as.factor(dat1$yy)
    dat1$row <- as.factor(dat1$row)
    
    
    if (!is.null(xlabel)) dat1$xx <- factor(dat1$xx, levels = 1:ncol(A), xlabel)
    if (!is.null(ylabel)) {dat1$yy <- factor(dat1$yy, levels = 1:nrow(A), ylabel[nr:1])
    }else{
        dat1$yy <- factor(dat1$yy, levels = 1:nrow(A), nr:1)  #label with reverse name
    }
    #dat1$ <-  dat1$value > quantile(dat1$value, 0.9)
    p <- ggplot(dat1,  aes(x = xx, y= yy )) +
        geom_text(aes(label = myround(value,2), colour = row, size = value))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    p + ggtitle(main) +xlab("")+ylab("") + theme(plot.title = element_text(hjust = 0.5))
}


# Multiple plot function
# copy and modified from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


#  myround
#'
#' Round a number, preserving extra 0's
#'
#' Round a number, preserving extra 0's.
#'
#' @param x Number to round.
#'
#' @param digits Number of digits past the decimal point to keep.
#'
#' @details
#' Uses \code{\link[base]{sprintf}} to round a number, keeping extra 0's.
#'
#' @export
#' @return
#' A vector of character strings.
#'
#' @examples
#' myround(51.01, 3)
#' myround(0.199, 2)
#'
#' @seealso
#' \code{\link[base]{round}}, \code{\link[base]{sprintf}}
#'
#' @keywords
#' utilities
myround <-
    function(x, digits=1)
    {
        if(digits < 1)
            stop("This is intended for the case digits >= 1.")
        
        if(length(digits) > 1) {
            digits <- digits[1]
            warning("Using only digits[1]")
        }
        
        tmp <- sprintf(paste("%.", digits, "f", sep=""), x)
        
        # deal with "-0.00" case
        zero <- paste0("0.", paste(rep("0", digits), collapse=""))
        tmp[tmp == paste0("-", zero)] <- zero
        
        tmp
    }