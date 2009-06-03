\name{burrs}
\Rdversion{1.1}
\alias{burrs}
\docType{data}
\title{
	Burr actual sizes in a blade cutting process
}
\description{
	Dataset reports the burr size as measured in a tube cutting process by image analysis. Burr size grows as effect of tool wear.
}
\usage{data(burrs)}
\format{
  A data frame with 36 observations on the following 2 variables.
  \describe{
    \item{\code{i}}{the cut progressive index}
    \item{\code{area}}{the actual burr area in mm^2}
  }
}
\source{
	Measures performed by Paolo Bosetti at the Laboratory of Mechanics and Automatics, University of Trento
}
\examples{
data(burrs)
burrs.ewma <- ewma(burrs, lambda=0.15, training = 3)
plot(burrs.ewma)
}
\keyword{datasets}
