\name{ewma}
\Rdversion{1.1}
\alias{ewma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ewma
}
\description{
  Calculates the chart data
}
\usage{
ewma(data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    A two columns array or a data frame. First column must contain the index number, second column the variable to be controlled.
  }
  \item{mu0}{
    The starting or average value. If omitted, it is calculated as the mean of the first training groups.
  }
  \item{sd}{
    The standard deviation. If omitted, it is calculated as the sd() of the first training groups.
  }
  \item{lambda}{
    The relaxation factor lambda.
  }
  \item{L}{
    The half width of control limits.
  }
  \item{group.size}{
    The size of groups.
  }
  \item{training}{
    The number of groups to be used for training.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{mu0}{the calculated mean value}
  \item{sd}{the calculated standard deviation}
  \item{data}{a data frame containing the chart points}
  \item{fooc}{first out of control points (list of lcl and ucl)}
  \item{training.groups}{number of single values used for training}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Paolo Bosetti
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
\keyword{ manip }
