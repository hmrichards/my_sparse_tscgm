\name{my.rblasso}
\alias{my.rblasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{my.rblasso
%%  ~~function to do ... ~~
}
\description{rblasso
}
\usage{
my.rblasso(s, m, om, nlam, tol = 1e-05, sbols = 1, maxit, quiet = 0, warm = 0, B0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{om}{
%%     ~~Describe \code{om} here~~
}
  \item{nlam}{
%%     ~~Describe \code{nlam} here~~
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
}
  \item{sbols}{
%%     ~~Describe \code{sbols} here~~
}
  \item{maxit}{
%%     ~~Describe \code{maxit} here~~
}
  \item{quiet}{
%%     ~~Describe \code{quiet} here~~
}
  \item{warm}{
%%     ~~Describe \code{warm} here~~
}
  \item{B0}{
%%     ~~Describe \code{B0} here~~
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
function (s, m, om, nlam, tol = 1e-05, sbols = 1, maxit, quiet = 0,
    warm = 0, B0 = NULL)
{
    p = dim(s)[1]
    q = dim(om)[1]
    s = as.double(s)
    m = as.double(m)
    om = as.double(om)
    nlam = as.double(nlam)
    tmp = matrix(0, nrow = p, ncol = q)
    tol = as.double(tol * sbols)
    mode(p) = "integer"
    mode(q) = "integer"
    mode(maxit) = "integer"
    mode(quiet) = "integer"
    mode(warm) = "integer"
    if (!warm)
        tmp = as.double(tmp)
    else tmp = as.double(B0)
    dotCoutput = .Call("blasso", Sin = s, Min = m, Omin = om,
        pin = p, qin = q, lamin = nlam, tol = tol, maxit = maxit,
        Bout = tmp, warm = warm)
    B = matrix(dotCoutput$Bout, nrow = p, ncol = q)
    return(B)
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
