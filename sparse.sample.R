library(SparseTSCGM)
library(my.sparse.tscgm)
library(longitudinal)

setwd("~/Research Stuff/Summer 2021 Research/TSGCM")
load("drosophila_data_11genes.Rdata")


# dros.int1 <- as.longitudinal(drosophila_data_11genes[1:30,])
#
# my.sparse.tscgm(data=dros.int1, lam1=NULL, lam2=NULL, nlambda=NULL,
#                 model="ar1", penalty="scad", optimality="bic",
#                 control = list(maxit.out = 2, maxit.in = 29))

data("mammary")
mam.tscgm <- my.sparse.tscgm(data=mammary, lam1=c(0.15, 0.93), lam2=c(0.15, 0.93), nlambda=1,
                          model="ar1", penalty="scad", optimality="bic",
                          control=list(maxit.out = 3, maxit.in = 54))

? mammary