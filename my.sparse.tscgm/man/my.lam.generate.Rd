\name{my.lam.generate}
\alias{my.lam.generate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{my.lam.generate
%%  ~~function to do ... ~~
}
\description{lam generate

}
\usage{
my.lam.generate(model = c("ar1", "ar2"), nlambda = NULL, xty = xty, xtx = xtx, yty = yty, T = T, n = n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{nlambda}{
%%     ~~Describe \code{nlambda} here~~
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
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (model = c("ar1", "ar2"), nlambda = NULL, xty = xty,
    xtx = xtx, yty = yty, T = T, n = n)
{
    if (model == "ar1") {
        SX <- xtx/(n * T)
        mSX <- glasso::glasso(SX, 0.05, penalize.diagonal = FALSE)
        SXi <- mSX$wi
        SS = (yty)/(n * T)
        SS = cov2cor(SS)
        SAs = xty/(n * T)
        SA = SAs \%*\% SXi
    }
    if (model == "ar2") {
        SX <- xtx/(n * T)
        mSX <- glasso::glasso(SX, 0.05, penalize.diagonal = FALSE)
        SXi <- mSX$wi
        SS = (yty)/(n * T)
        SS = cov2cor(SS)
        SAs = xty/(n * T)
        SA = SXi \%*\% SAs
    }
    lambda <- my.lambda.seq(SS = SS, SA = SA, nlambda = nlambda)
    lam1 <- round(lambda$lambda1, 3)
    lam2 <- round(lambda$lambda2, 3)
    lam2 <- round(lam2/max(lam2), 3)
    nlambda <- lambda$nlambda
    lowr = nlambda - (nlambda - 2)
    upr = nlambda - 2
    lam1 = round(seq(lam1[lowr], lam1[upr], length = nlambda),
        3)
    lam2 = round(seq(lam2[lowr], lam2[upr], length = nlambda),
        3)
    out.lam = list(lam1 = lam1, lam2 = lam2)
    return(out.lam)
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
