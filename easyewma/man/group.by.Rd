\name{group.by}
\Rdversion{1.1}
\alias{group.by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  group.by
}
\description{
  Groups the data by group index.
}
\usage{
  group.by(data, sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    An array of values to be grouped
  }
  \item{sample}{
    an array of same size holding the group indices
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
\keyword{ manip }
