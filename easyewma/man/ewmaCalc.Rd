\name{ewmaCalc}
\Rdversion{1.1}
\alias{ewmaCalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ewmaCalc(data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function(
  data,
  mu0 = NA,
  sd = NA,
  lambda = 0.2,
  L = 3,
  group.size = 3,
  training = 5
  ) {
    n.points <- dim(data)[1]
    n.groups <- floor(n.points/group.size)
    full.df  <- data.frame(
      expand.grid(n=1:group.size, r=1:n.groups), 
      i = data[1:(n.groups*group.size),1],
      x = data[1:(n.groups*group.size),2]
    )
    group.df <- data.frame(
      xbar  = apply(group.by(full.df$x,full.df$r), c(1), mean),
      xsd   = apply(group.by(full.df$x,full.df$r), c(1), sd),
      i     = apply(group.by(full.df$i,full.df$r), c(1), max)
    )
    x.start <- ifelse(is.na(mu0), mean(data[1:training,2]), mu0) 
    x.sd    <- ifelse(is.na(sd), sd(data[1:training,2]), sd)
    range <- L*x.sd*sqrt(lambda/(group.size*(2-lambda))*(1-(1-lambda)^(2*c(1:n.groups))))
    group.df <- data.frame(
      group.df,
      z=ewma.smooth(group.df$i, group.df$xbar,start=x.start,lambda=lambda)$y,
      lcl=x.start-range,
      ucl=x.start+range
    )
    result <-  list(
      mu0 = x.start,
      sd = x.sd,
      data = group.df,
      fooc = list(
        ucl = group.df[group.df$z >= group.df$ucl,]$i[1],
        lcl = group.df[group.df$z <= group.df$lcl,]$i[1]
      ),
      training.groups = group.df$i[training]
    )
    # attr(result, "class")<-"ewma"
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
