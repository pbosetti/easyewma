plot.ewma <-
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

