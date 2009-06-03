\name{ewma.smooth}
\Rdversion{1.1}
\alias{ewma.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Calculates the exponentially smoothed moving average.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ewma.smooth(x, y, lambda = 0.2, start, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, y, lambda = 0.2, start, ...) 
{
    if (length(y) != length(x)) 
        stop("x and y must have the same length!")
    if (abs(lambda) > 1) 
        stop("lambda parameter must be between 0 and 1")
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    n <- length(y)
    if (missing(start)) 
        start <- y[1]
    S1 <- diag(rep(1, n))
    for (i in 1:(n - 1)) {
        for (j in i:n) {
            S1[j, i] <- (1 - lambda)^(j - i)
        }
    }
    S2 <- (1 - lambda)^seq(1, n)
    z <- lambda * (S1 \%*\% y) + S2 * start
    list(x = x, y = z, lambda = lambda, start = start)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
