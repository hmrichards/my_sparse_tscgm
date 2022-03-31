\name{my.compute.sparse.tscgm}
\alias{my.compute.sparse.tscgm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{my.compute.sparse.tscgm
%%  ~~function to do ... ~~
}
\description{ compute sparse
}
\usage{
my.compute.sparse.tscgm(penalty = c("scad", "lasso"), T = T, n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty, xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1, lam2 = lam2, optimality = c("NULL", "bic", "bic_ext", "bic_mod", "aic", "gic"), setting = setting)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{penalty}{
%%     ~~Describe \code{penalty} here~~
}
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{q}{
%%     ~~Describe \code{q} here~~
}
  \item{xty}{
%%     ~~Describe \code{xty} here~~
}
  \item{xtx}{
%%     ~~Describe \code{xtx} here~~
}
  \item{yty}{
%%     ~~Describe \code{yty} here~~
}
  \item{xtxt}{
%%     ~~Describe \code{xtxt} here~~
}
  \item{xtx2}{
%%     ~~Describe \code{xtx2} here~~
}
  \item{yty2}{
%%     ~~Describe \code{yty2} here~~
}
  \item{lam1}{
%%     ~~Describe \code{lam1} here~~
}
  \item{lam2}{
%%     ~~Describe \code{lam2} here~~
}
  \item{optimality}{
%%     ~~Describe \code{optimality} here~~
}
  \item{setting}{
%%     ~~Describe \code{setting} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (penalty = c("scad", "lasso"), T = T, n = n, p = p,
    q = q, xty = xty, xtx = xtx, yty = yty, xtxt = xtxt, xtx2 = xtx2,
    yty2 = yty2, lam1 = lam1, lam2 = lam2, optimality = c("NULL",
        "bic", "bic_ext", "bic_mod", "aic", "gic"), setting = setting)
{
    nlam = (n * T) * lam2
    old.B = qr.solve(xtx + nlam * diag(p), xty)
    if (!is.numeric(old.B))
        old.B <- matrix(0, p, q)
    lam11 <- lam1 * (1 - diag(q))
    old.om0 <- diag(q)
    old.om0.i <- diag(q)
    k = 0
    mab = sum(sum(abs(old.B)))
    penalty = match.arg(penalty)
    if (penalty == "scad") {
        while (1) {
            k = k + 1
            samp.cov = (yty - t(xty) \%*\% old.B - t(old.B) \%*\%
                xty + t(old.B) \%*\% xtx \%*\% old.B)/(n * T)
            if (k == 1) {
                g.out = glasso::glasso(s = samp.cov, rho = lam11,
                  thr = 1e-04, maxit = 10000, penalize.diagonal = FALSE)
                old1.om = g.out$wi
                old1.om.i = g.out$w
                if (!is.numeric(old1.om))
                  old1.om <- diag(q)
                wt <- matrix(NA, nrow = q, ncol = q)
                a <- 3.7
                for (i in 1:q) {
                  for (j in 1:q) {
                    if (abs(old1.om[i, j]) <= lam1)
                      wt[i, j] <- lam1
                    else {
                      if ((lam1 <= abs(old1.om[i, j])) & (abs(old1.om[i,
                        j]) < a * lam1)) {
                        wt[i, j] <- ((a * lam1 - abs(old1.om[i,
                          j]))/((a - 1)))
                      }
                      else wt[i, j] <- 0
                    }
                  }
                }
                diag(wt) <- 0
                g.out1 = glasso::glasso(s = samp.cov, rho = wt,
                  thr = 1e-04, maxit = 10000, penalize.diagonal = FALSE,
                  start = "warm", w.init = old1.om.i, wi.init = old1.om)
                old.om = g.out1$wi
                old.om.i = g.out1$w
            }
            if (k > 1) {
                old1.om = old.om
                old1.om.i = old.om.i
                if (!is.numeric(old1.om))
                  old1.om <- diag(q)
                wt <- matrix(NA, nrow = q, ncol = q)
                a <- 3.7
                for (i in 1:q) {
                  for (j in 1:q) {
                    if (abs(old1.om[i, j]) <= lam1)
                      wt[i, j] <- lam1
                    else {
                      if ((lam1 <= abs(old1.om[i, j])) & (abs(old1.om[i,
                        j]) < a * lam1)) {
                        wt[i, j] <- ((a * lam1 - abs(old1.om[i,
                          j]))/((a - 1)))
                      }
                      else wt[i, j] <- 0
                    }
                  }
                }
                diag(wt) <- 0
                g.out1 = glasso::glasso(s = samp.cov, rho = wt,
                  thr = 1e-04, maxit = 10000, penalize.diagonal = FALSE,
                  start = "warm", w.init = old1.om.i, wi.init = old1.om)
                old.om = g.out1$wi
                old.om.i = g.out1$w
            }
            if (!is.numeric(old.om))
                old.om <- diag(q)
            xtyom = (xty \%*\% old.om)
            wt1 <- matrix(NA, nrow = p, ncol = q)
            a <- 3.7
            for (i in 1:p) {
                if (xtx[i, i] == 0)
                  xtx[i, i] <- 1
                for (j in 1:q) {
                  if (abs(old.B[i, j]) <= lam2)
                    wt1[i, j] <- lam2
                  else {
                    if ((lam2 < abs(old.B[i, j])) & (abs(old.B[i,
                      j]) < a * lam2)) {
                      wt1[i, j] <- ((a * lam2 - abs(old.B[i,
                        j]))/((a - 1)))
                    }
                    else wt1[i, j] <- 0
                  }
                }
            }
            rho2 <- wt1 * (n * T)
            warmstart = 1
            if (k == 1)
                warmstart = 0
            B = my.rblasso(s = xtx, m = xtyom, om = old.om, nlam = rho2,
                tol = 1e-05, sbols = mab, maxit = setting$maxit.in,
                warm = warmstart, B0 = old.B)
            bdist = sum(sum(abs(B - old.B)))
            if (!is.numeric(B))
                B <- matrix(0, p, q)
            old.B = B
            if ((bdist < setting$tol.out * mab) | (k > setting$maxit.out))
                break
            message("Outer iterations: ", k, "\n")
            message("lambda1 = ", lam1, "\n")
            message("lambda2 = ", lam2, "\n")
        }
    }
    if (penalty == "lasso") {
        while (1) {
            k = k + 1
            samp.cov = (yty - t(xty) \%*\% old.B - t(old.B) \%*\%
                xty + t(old.B) \%*\% xtx \%*\% old.B)/(n * T)
            if (k == 1) {
                g.out = glasso::glasso(s = samp.cov, rho = lam11,
                  thr = 1e-04, maxit = 10000, penalize.diagonal = FALSE)
                old.om = g.out$wi
                old.om.i = g.out$w
            }
            if (k > 1) {
                old1.om = old.om
                old1.om.i = old.om.i
                g.out1 = glasso::glasso(s = samp.cov, rho = lam11,
                  thr = 1e-04, maxit = 10000, penalize.diagonal = FALSE,
                  start = "warm", w.init = old1.om.i, wi.init = old1.om)
                old.om = g.out1$wi
                old.om.i = g.out1$w
            }
            if (!is.numeric(old.om))
                old.om <- diag(q)
            xtyom = (xty \%*\% old.om)
            wt1 <- matrix(lam2, nrow = p, ncol = q)
            rho2 <- wt1 * (n * T)
            warmstart = 1
            if (k == 1)
                warmstart = 0
            B = my.rblasso(s = xtx, m = xtyom, om = old.om, nlam = rho2,
                tol = 1e-05, sbols = mab, maxit = setting$maxit.in,
                warm = warmstart, B0 = old.B)
            bdist = sum(sum(abs(B - old.B)))
            if (!is.numeric(B))
                B <- matrix(0, p, q)
            old.B = B
            if ((bdist < setting$tol.out * mab) | (k > setting$maxit.out))
                break
            message("Outer iterations: ", k, "\n")
            message("lambda1 = ", lam1, "\n")
            message("lambda2 = ", lam2, "\n")
        }
    }
    if (setting$silent == FALSE)
        cat("Total outer iterations for tscgm : ", k, "\n")
    return(list(gamma = old.B, theta = old.om))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
