\name{my.sparse.tscgm}
\alias{my.sparse.tscgm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{my.sparse.tscgm
%%  ~~function to do ... ~~
}
\description{ main function
}
\usage{
my.sparse.tscgm(data = data, lam1 = NULL, lam2 = NULL, nlambda = NULL, model = c("ar1", "ar2"), penalty = c("scad", "lasso"), optimality = c("NULL", "bic", "bic_ext", "bic_mod", "aic", "gic"), control = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{lam1}{
%%     ~~Describe \code{lam1} here~~
}
  \item{lam2}{
%%     ~~Describe \code{lam2} here~~
}
  \item{nlambda}{
%%     ~~Describe \code{nlambda} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{penalty}{
%%     ~~Describe \code{penalty} here~~
}
  \item{optimality}{
%%     ~~Describe \code{optimality} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
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
function (data = data, lam1 = NULL, lam2 = NULL, nlambda = NULL,
    model = c("ar1", "ar2"), penalty = c("scad", "lasso"), optimality = c("NULL",
        "bic", "bic_ext", "bic_mod", "aic", "gic"), control = list())
{
    if (is.longitudinal(data) == TRUE) {
        n_obs = get.time.repeats(data)$repeats[[1]]
        tps = get.time.repeats(data)$time
        p_num = dim(data)[2]
        time = dim(data)[1]/n_obs
        xy.data <- array(NA, c(time, p_num, n_obs))
        data <- as.matrix(data)
        for (i in 1:n_obs) {
            for (t in 1:time) {
                cc <- 1 + (t - 1) * n_obs + (i - 1)
                xy.data[t, , i] <- data[cc, ]
            }
        }
    }
    else {
        cat("Data format is not longitudinal.", "\n")
    }
    model = match.arg(model)
    penalty = match.arg(penalty)
    data.prep <- my.pre.tscgm(xy.data = xy.data, time = time,
        model = model)
    xty <- data.prep$xty
    xtx <- data.prep$xtx
    yty <- data.prep$yty
    xtxt <- data.prep$xtxt
    xtx2 <- data.prep$xtx2
    yty2 <- data.prep$yty2
    p <- data.prep$p
    T <- data.prep$T
    n <- data.prep$n
    q <- data.prep$q
    if (is.null(lam1) | is.null(lam2)) {
        lam.sequence <- my.lam.generate(model = model, nlambda = NULL,
            xty = xty, xtx = xtx, yty = yty, T = T, n = n)
        lam1 <- lam.sequence$lam1
        lam2 <- lam.sequence$lam2
    }
    optimality = match.arg(optimality)
    nobic = (length(lam1) + length(lam2) == 2)
    doms = (length(lam1) + length(lam2) > 2)
    if (!is.list(control))
        stop("control is not a list")
    setting <- list(maxit.out = 5, maxit.in = 50, tol.out = 1e-04,
        silent = TRUE)
    nmsSetting <- names(setting)
    setting[(nms <- names(control))] <- control
    if (length(noNms <- nms[!nms \%in\% nmsSetting]))
        warning("unknow names in control: ", paste(noNms, collapse = ", "))
    if (nobic != 2 & optimality == "nosel")
        stop("Specify positive scalar values for the tuning parameters")
    if (doms > 2 & optimality != "nosel")
        stop("Specify vector of positive decreasing values for the tuning parameters")
    if (setting$maxit.out < 1)
        stop("read the documentation for 'maxit.out' more carefully")
    if (setting$maxit.in < 1)
        stop("read the documentation for 'maxit.in' more carefully")
    if (setting$tol.out <= 0)
        stop("read the documentation for 'tol.out' more carefully")
    if (!setting$silent \%in\% c("TRUE", "FALSE"))
        stop("read the documentation for 'silent' more carefully")
    doNULL = nobic & (optimality == "NULL")
    dobic = doms & (optimality == "bic")
    dobic1 = doms & (optimality == "bic_ext")
    dobic2 = doms & (optimality == "bic_mod")
    dobic3 = doms & (optimality == "aic")
    dobic4 = doms & (optimality == "gic")
    gamma = NULL
    theta = NULL
    if (doNULL) {
        tmp.out = my.compute.sparse.tscgm(penalty = penalty,
            T = T, n = n, p = p, q = q, xty = xty, xtx = xtx,
            yty = yty, xtxt = xtxt, xtx2 = xtx2, yty2 = yty2,
            lam1 = lam1, lam2 = lam2, optimality = "NULL", setting = setting)
    }
    else if (dobic) {
        tmp.out = my.sparse.tscgm.bic(penalty = penalty, T = T,
            n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty,
            xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1,
            lam2 = lam2, optimality = "bic", setting = setting)
    }
    else if (dobic1) {
        tmp.out = my.sparse.tscgm.bic(penalty = penalty, T = T,
            n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty,
            xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1,
            lam2 = lam2, optimality = "bic_ext", setting = setting)
    }
    else if (dobic2) {
        tmp.out = my.sparse.tscgm.bic(penalty = penalty, T = T,
            n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty,
            xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1,
            lam2 = lam2, optimality = "bic_mod", setting = setting)
    }
    else if (dobic3) {
        tmp.out = my.sparse.tscgm.bic(penalty = penalty, T = T,
            n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty,
            xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1,
            lam2 = lam2, optimality = "aic", setting = setting)
    }
    else if (dobic4) {
        tmp.out = my.sparse.tscgm.bic(penalty = penalty, T = T,
            n = n, p = p, q = q, xty = xty, xtx = xtx, yty = yty,
            xtxt = xtxt, xtx2 = xtx2, yty2 = yty2, lam1 = lam1,
            lam2 = lam2, optimality = "gic", setting = setting)
    }
    gamma = tmp.out$gamma
    gamma = gamma * (1 * (abs(gamma) > 0.01))
    theta = tmp.out$theta
    theta = theta * (1 * (abs(theta) > 0.01))
    lam1.opt = tmp.out$lam1.opt
    lam2.opt = tmp.out$lam2.opt
    lam1.seq = tmp.out$lam1.seq
    lam2.seq = tmp.out$lam2.seq
    s.gamma = tmp.out$s.gamma
    s.theta = tmp.out$s.theta
    tun.ic = tmp.out$tun.ic
    min.ic = tmp.out$min.ic
    if (model == "ar1") {
        colnames(gamma) <- colnames(data)
    }
    else if (model == "ar2") {
        colnames(gamma) <- colnames(data)
        rownames(gamma) <- c(colnames(data), colnames(data))
    }
    colnames(theta) <- rownames(theta) <- colnames(data)
    out = list(gamma = gamma, theta = theta, lam1.opt = lam1.opt,
        lam2.opt = lam2.opt, lam1.seq = lam1.seq, lam2.seq = lam2.seq,
        min.ic = min.ic, tun.ic = tun.ic, s.gamma = s.gamma,
        s.theta = s.theta)
    class(out) = "my.sparse.tscgm"
    return(out)
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
