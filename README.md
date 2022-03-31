# my_sparse_tscgm
## Sample code

```{r}
library(my.sparse.tscgm)
library(longitudinal)

load("drosophila_data_11genes.Rdata")


dros.int1 <- as.longitudinal(drosophila_data_11genes[1:30,])
my.sparse.tscgm(data=dros.int1, lam1=NULL, lam2=NULL, nlambda=NULL,
                model="ar1", penalty="scad", optimality="bic",
                control = list(maxit.out = 2, maxit.in = 29))

```

```{r}
library(SparseTSCGM)
library(my.sparse.tscgm)

data("mammary")
my.sparse.tscgm(data = mammary, lam1=NULL, lam2=NULL,
                model="ar1", penalty="scad", optimality="bic",
                control = list(maxit.out = 2, maxit.in = 29))
```

