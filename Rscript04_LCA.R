
rm(list = ls())
library(poLCA)


load("./code/ideology_app/data1.RData")


numerical_to_factor <- function(x, nlevels){
    
    qt <- quantile(x, 1:(nlevels-1)/nlevels)
    f <- rep(1, length(x))
    for (i in 1:nlevels){
        f[x>qt[i]] <- i+1
    }
    return (f)
}


x1_lca <- apply(x1, MARGIN = 2, FUN = function(x) numerical_to_factor(x, 5))
x1_lca <- data.frame(x1_lca)
f = as.matrix(x1_lca)~1
lca <- poLCA(formula = f, data = x1_lca, nclass = 6)
table(lca$predclass)



