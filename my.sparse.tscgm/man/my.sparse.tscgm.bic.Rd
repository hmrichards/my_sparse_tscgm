\name{my.sparse.tscgm.bic}
\alias{my.sparse.tscgm.bic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{my.sparse.tscgm.bic
%%  ~~function to do ... ~~
}
\description{ bic
}
\usage{
my.sparse.tscgm.bic(penalty = c("scad", "lasso"), T = T, n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty, xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1, lam2 = lam2, optimality = c("NULL", "bic", "bic_ext", "bic_mod", "aic", "gic"), setting = setting)
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
    lam.vec.1 = lam1
    lam.vec.2 = lam2
    lamR = length(lam.vec.1) * length(lam.vec.2)
    BICh = matrix(NA, lamR, 1)
    lam1h = matrix(NA, lamR, 1)
    lam2h = matrix(NA, lamR, 1)
    penalty = match.arg(penalty)
    uv <- 0
    for (u in 1:length(lam.vec.1)) {
        for (v in 1:length(lam.vec.2)) {
            uv <- uv + 1
            outscad_s <- my.compute.sparse.tscgm(penalty = penalty,
                T = T, n = n, p = p, q = q, xty = xty, xtx = xtx,
                yty = yty, xtxt = xtxt, xtx2 = xtx2, yty2 = yty2,
                lam1 = lam.vec.1[u], lam2 = lam.vec.2[v], optimality = "NULL",
                setting = setting)
            PO = outscad_s$theta
            PB = outscad_s$gamma
            WS = (yty - t(xty) \%*\% PB - t(PB) \%*\% xty + t(PB) \%*\%
                xtx \%*\% PB)/(n * T)
            lik1 = determinant(PO)$modulus[1]
            lik2 <- sum(diag(PO \%*\% WS))
            diag(PO) = 0
            pdO = sum(sum(PO != 0))
            pdB = sum(sum(PB != 0))
            LLk <- (n * T/2) * (lik1 - lik2)
            LLk0 <- (n * T/2) * (-lik2)
            if (optimality == "bic") {
                BICh[uv, 1] <- -2 * LLk + (log(n * T)) * (pdO/2 +
                  q + pdB)
            }
            else if (optimality == "bic_ext") {
                BICh[uv, 1] <- -2 * LLk + (log(n * T)) * (pdO/2 +
                  q + pdB) + (pdO/2 + q + pdB) * 4 * 0.5 * log(q +
                  p)
            }
            else if (optimality == "bic_mod") {
                BICh[uv, 1] <- -2 * LLk + log(n * T) * (pdO/2 +
                  q + pdB) * log(log(q + p))
            }
            else if (optimality == "aic") {
                BICh[uv, 1] <- -2 * LLk + 2 * (pdO/2 + q + pdB)
            }
            else if (optimality == "gic") {
                BICh[uv, 1] <- -2 * LLk + log(log(n * T)) * (pdO/2 +
                  q + pdB) * log(q + p)
            }
            lam1h[uv, 1] <- lam.vec.1[u]
            lam2h[uv, 1] <- lam.vec.2[v]
        }
    }
    res.scad <- cbind(lam1h, lam2h, BICh)
    bicid <- which.min(res.scad[, 3])
    lam1.opt <- res.scad[bicid, 1]
    lam2.opt <- res.scad[bicid, 2]
    bicm <- res.scad[bicid, 3]
    tmp.out = my.compute.sparse.tscgm(penalty = penalty, T = T,
        n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty,
        xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1.opt,
        lam2 = lam2.opt, optimality = "NULL", setting = setting)
    best.B = tmp.out$gamma
    best.theta = tmp.out$theta
    d.gamma <- tmp.out$gamma
    diag(d.gamma) <- 0
    s.gamma = sum(abs(d.gamma) > 0)/(p^2)
    d.theta <- tmp.out$theta
    diag(d.theta) <- 0
    s.theta = (0.5 * sum(abs(d.theta) > 0))/(0.5 * q * (q - 1))
    tun.ic <- res.scad
    lam1s <- lam.vec.1
    lam2s <- lam.vec.2
    min.ic <- bicm
    colnames(tun.ic) <- c("Lambda1", "Lambda2", "IC")
    return(list(gamma = best.B, theta = best.theta, lam1.opt = lam1.opt,
        lam2.opt = lam2.opt, lam1.seq = lam1s, lam2.seq = lam2s,
        min.ic = min.ic, tun.ic = tun.ic, s.gamma = s.gamma,
        s.theta = s.theta))
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
