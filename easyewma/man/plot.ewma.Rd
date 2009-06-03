\name{plot.ewma}
\Rdversion{1.1}
\alias{plot.ewma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	PLots the ewma object.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.ewma(x, typ = "b", xlab = "Index", ylab = "EWMA", ...)
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, typ="b", xlab="Index", ylab="EWMA",...) {
  plot(x$data$z~x$data$i, 
    ylim=c(min(x$data$lcl),max(x$data$z)),
    xlim=c(0,max(x$data$i)),
    typ = typ, 
    xlab=xlab,
    ylab=ylab,
    ...)
  lines(x$data$lcl~x$data$i, col="red")
  lines(x$data$ucl~x$data$i, col="red")
  abline(h=x$mu0, col="green")
  abline(v=x$fooc$ucl, lty=2, col="red")
  abline(v=x$tr, lty=2, col="darkgray")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
