\name{ewma.formula}
\Rdversion{1.1}
\alias{ewma.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ewma.formula(formula, data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{mu0}{
%%     ~~Describe \code{mu0} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{L}{
%%     ~~Describe \code{L} here~~
}
  \item{group.size}{
%%     ~~Describe \code{group.size} here~~
}
  \item{training}{
%%     ~~Describe \code{training} here~~
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
function(formula, data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 )
{
  result <- ewmaCalc(model.frame(formula, data), mu0, sd, lambda, L, group.size, training)
  result$call <- match.call()
  class(result) <- "ewma"
  result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
