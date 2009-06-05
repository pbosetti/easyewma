\name{ewma}
\Rdversion{1.1}
\alias{ewma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ewma
}
\description{
  Generic function that calculates the EWMA chart. It currently accepts either an array (or a data.frame) with indexes in the first column and values in the second column, or a formula.
}
\usage{
ewma(formula, data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{data} here~~
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
data(burrs)
burrs.ewma <- ewma(burrs, lambda=0.15, training = 3)
plot(burrs.ewma)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
