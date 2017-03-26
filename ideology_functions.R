

plt.centers <- function(x1, labs){
    
    k = length(unique(labs))
    m <- dim(x1)[2]
    nNodes <- length(labs)
    Z <-matrix(0, nNodes, k )
    for ( i in 1:k){ Z[labs ==i, i ] <- 1}
    NZ <- Z %*% Diagonal(k, table(labs)^(-1)) 
    centers <- t(NZ)%*%x1
    base = colMeans(x1)
    ylim <- range(centers) + c(-0.5, 0.5); 
    ylim[1] <- round(ylim[1],0); ylim[2] <- round(ylim[2],0)
    
    par(mar = c(6.5,3,3,2))
    plot (x= 1:m, y= base, xlim = c(0,m+6), ylim= ylim, 
          ylab="mean values", xlab = "",ann = F,axes = F, 
          type ="l", pch=".", col = "black", lwd =2)
    axis(side = 1, at=1:m, pos= ylim[1],
         labels= colnames(centers)[1:m], las =3)
    unit <- round((ylim[2]-ylim[1])/6, 1)
    axis(side = 2, at=seq(ylim[1],ylim[2], by = unit), 
          labels= as.character(seq(ylim[1],ylim[2], by= unit)))
    for ( i in 1:k){
        points (x = 1:m, y= centers[i,], 
                type ="b", col =i, pch = i)
    }
    legend(x = m+1, y = ylim[2],legend = paste0(1:k,"-", table(labs)), 
           col = 1:k, lty =2, pch = 1:k, cex =  1, box.lwd = 0)
    
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
          labs( x = "Coordinate 1", y = "Coordinate 2", title = main_title)
    p2 <- ggplot(dat, aes(x = z1, y = z3, colour = cluster) )+
        geom_text(aes(label = cluster),  size =3) + 
        labs( x = "Coordinate 1", y = "Coordinate 3", title = main_title)
    p3 <- ggplot(dat, aes(x = z2, y = z3, colour = cluster) )+
        geom_text(aes(label = cluster),  size =3) + 
        labs( x ="Coordinate 2", y = "Coordinate 3", title = main_title)
    
    res = list(p12 = p1, p13=p2, p23 =p3)
    return (res)

}

plt.cluster <- function(points, labs, 
                    xlabel = NULL, ylabel = NULL, main_title = NULL){
    
    dat <- as.matrix(points[,1:2])
    k <- length(unique(labs))
    colnames(dat) <- c("z1","z2")
    dat <- data.frame(dat, cluster = labs)
    dat$cluster <- as.factor(dat$cluster)
    
    p1 <- ggplot(dat, aes(x = z1, y = z2, colour = cluster) )+
        geom_text(aes(label = cluster),  size =3) + 
        labs( x = "Coordinate 1", y = "Coordinate 2", title = main_title)
    p1
}

membershipM <- function(labs){
    k <- max(labs,na.rm = T); m <- length(labs)
    Z <- matrix(0, m, k)
    for(i in 1:k){
        Z[which(labs == i), i] <- 1 
    }
    return(Z)
}

#flattenMatrix to i,j, value
flattenMatrix <- function(adjM){
    n = dim(adjM)[1]
    m = dim(adjM)[2]
    data<- data.frame(list(
        row = rep(1:n, times = m),
        col = rep(1:m, each = n),
        value = as.vector(adjM)))
    return (data)
}

balloon.plot <- function(A, rowlabel = NULL, collabel = NULL , main = NULL){
    nr <- nrow(A)
    A <- A[nr:1,];  
    dat1 <- flattenMatrix(A)
    dat1$row <- as.factor(dat1$row); dat1$col <- as.factor(dat1$col)
    if (length(which(A<0))>0){
        dat1$sign <- as.factor(1*(dat1$value>0))
        dat1$sign <-factor(dat1$sign, levels =c(1,0), labels = c(">0","<=0"))
        dat1$value <- abs(dat1$value)
    } 
    if (!is.null(rowlabel)) dat1$row <- factor(dat1$row, levels = 1:nrow(A), rowlabel[nr:1])
    if (!is.null(collabel)) dat1$col <- factor(dat1$col, levels = 1:ncol(A), collabel)
    #dat1$ <-  dat1$value > quantile(dat1$value, 0.9)
    p <- ggplot(dat1,  aes(x = col, y= row ))
    
    if (length(which(A<0))>0){
        #p1 <- p + geom_point(aes(size = value, colour = sign)) +xlab('')+ylab('')
        p1 <- p + geom_text(aes(label = myround(value,2), colour = sign, size =value))
    }else{
        #p1 <- p + geom_point(aes(size = value)) +xlab('')+ylab('')
        p1 <- p + geom_text(aes(label = myround(value,2), colour = sign, size =value))
    }
    if (! is.null(rowlabel)) {
        p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    p1 + ggtitle(main)
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