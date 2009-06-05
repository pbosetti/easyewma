\name{plot.ewma}
\Rdversion{1.1}
\alias{plot.ewma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  plot.ewma
}
\description{
  Plots the ewma object.
}
\usage{
\method{plot}{ewma}(x, typ = "b", xlab = "Index", ylab = "EWMA", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{typ}{
%%     ~~Describe \code{typ} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
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
data(burrs)
burrs.ewma <- ewma(burrs, lambda=0.15, training = 3)
plot(burrs.ewma)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
