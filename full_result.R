rm(list =ls())
#setwd("~/Stat/DataProjects/ideology_study/code//ideology_app/")


library(ggplot2)
#library(tidyverse) #error on purrr package
#library(dplyr) #%>%
library(mclust)
library(gridExtra)
library(grid)  #grid.arrange
library(poLCA) #LCA 
#library(Matrix)

load("data1.RData") 
# regenerated on July 6, 2017, added DEM, GOP subsets
# originalData ; 
# data1 - removed missing, not scales
# x1 -- scaled2; x2 is PCA of x1 with 8 components

colMeans(data1)
colMeans(x1)
