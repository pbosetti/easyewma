\name{easyewma-package}
\Rdversion{1.1}
\alias{easyewma-package}
\alias{easyewma}
\docType{package}
\title{
  Easy EWMA chart builder
}
\description{
  The library allows an easy computation of Exponentially Weighted Moving Average charts.
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
}
\author{
  Paolo Bosetti
  Maintainer: Paolo Bosetti <paolo.bosetti@ing.unitn.it>
}
\references{
 Montgomery, D.~C., 2001.
 Introduction to Statistical Quality Control, 4th~ed.
 Wiley, New York, N. Y.
}
\keyword{ package }
\seealso{
}
\examples{
data(burrs)
burrs.ewma <- ewma(burrs, lambda=0.15, training = 3)
plot(burrs.ewma)
}
