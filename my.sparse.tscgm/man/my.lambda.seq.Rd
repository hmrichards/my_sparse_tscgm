\name{my.lambda.seq}
\alias{my.lambda.seq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{my.lambda.seq
%%  ~~function to do ... ~~
}
\description{ lam seq
}
\usage{
my.lambda.seq(SS, SA, nlambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS}{
%%     ~~Describe \code{SS} here~~
}
  \item{SA}{
%%     ~~Describe \code{SA} here~~
}
  \item{nlambda}{
%%     ~~Describe \code{nlambda} here~~
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
function (SS, SA, nlambda)
{
    if (is.null(nlambda))
        nlambda = 10
    else if (!is.null(nlambda))
        nlambda = nlambda
    d = dim(SS)[2]
    lambda.min.ratio = 0.1
    lambda.max1 = max(max(SS - diag(d)), -min(SS - diag(d)))
    lambda.min1 = lambda.min.ratio * lambda.max1
    lambda1 = exp(seq(log(lambda.max1), log(lambda.min1), length = nlambda))
    lambda.min.ratio2 = 0.15
    lambda.max2 = max(max(SA), -min(SA))
    lambda.min2 = lambda.min.ratio2 * lambda.max2
    lambda2 = exp(seq(log(lambda.max2), log(lambda.min2), length = nlambda))
    return(list(lambda1 = lambda1, lambda2 = lambda2, nlambda = nlambda))
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
