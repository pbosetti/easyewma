\name{group.by}
\Rdversion{1.1}
\alias{group.by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Groups the data by group index.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
group.by(data, sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{sample}{
%%     ~~Describe \code{sample} here~~
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
function (data, sample) 
{
    if (length(data) != length(sample)) 
        stop("data and sample must be vectors of equal length")
    x <- lapply(split(data, sample), as.vector)
    lx <- sapply(x, length)
    for (i in which(lx != max(lx))) x[[i]] <- c(x[[i]], rep(NA, 
        max(lx) - lx[i]))
    x <- t(sapply(x, as.vector))
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
