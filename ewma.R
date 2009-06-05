ewma.smooth <- function (x, y, lambda = 0.2, start, ...) 
{
    if (length(y) != length(x)) 
        stop("x and y must have the same length!")
    if (abs(lambda) > 1) 
        stop("lambda parameter must be between 0 and 1")
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    n <- length(y)
    if (missing(start)) 
        start <- y[1]
    S1 <- diag(rep(1, n))
    for (i in 1:(n - 1)) {
        for (j in i:n) {
            S1[j, i] <- (1 - lambda)^(j - i)
        }
    }
    S2 <- (1 - lambda)^seq(1, n)
    z <- lambda * (S1 %*% y) + S2 * start
    list(x = x, y = z, lambda = lambda, start = start)
}

group.by <- function (data, sample) 
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

ewma <- function(data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 ) UseMethod("ewma")

ewma.default <- function(data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 )
{
  result <- ewmaCalc(data, mu0, sd, lambda, L, group.size, training)
  result$call <- match.call()
  class(result) <- "ewma"
  result
}

ewma.formula <- function(formula, data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 )
{
  result <- ewmaCalc(model.frame(formula, data), mu0, sd, lambda, L, group.size, training)
  result$call <- match.call()
  class(result) <- "ewma"
  result
}


ewmaCalc <- function(
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

plot.ewma <- function(x, typ="b", xlab="Index", ylab="EWMA",...) {
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

print.ewma <- function(x)
{
  cat("EWMA computation\n")
  cat(paste("Call: ", x$call, "\n"))
  
}

cat("EWMA library loaded")