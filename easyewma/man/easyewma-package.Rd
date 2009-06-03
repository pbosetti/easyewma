\name{easyewma-package}
\Rdversion{1.1}
\alias{easyewma-package}
\alias{easyewma}
\docType{package}
\title{
	Easy EWMA chart builder
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab easyewma\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-06-03\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
~~ An overview of how to use the package, including the most ~~
~~ important functions ~~
}
\author{
Paolo Bosetti

Maintainer: Paolo Bosetti <paolo.bosetti@ing.unitn.it>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(burrs)
burrs.ewma <- ewma(burrs, lambda=0.15, training = 3)
plot(burrs.ewma)
}
