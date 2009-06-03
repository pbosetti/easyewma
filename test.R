source("ewma.R")
burrs <- read.table("data/burrs_1.dat", h=T)
burrs.ewma <- ewma(burrs, lambda=0.15, training = 3)
plot(burrs.ewma$data$z~burrs.ewma$data$i,
  typ="b",
  xlab="Cut index",
  ylab=expression(
    paste("Actual area (mm^2)")
  ),
  ylim=c(min(burrs.ewma$data$lcl),max(burrs.ewma$data$z)),
  xlim=c(0,max(burrs.ewma$data$i))
)
lines(burrs.ewma$data$lcl~burrs.ewma$data$i, col="red")
lines(burrs.ewma$data$ucl~burrs.ewma$data$i, col="red")
abline(h=burrs.ewma$mu0, col="green")
abline(v=burrs.ewma$fooc$ucl, lty=2, col="red")
abline(v=burrs.ewma$tr, lty=2, col="darkgray")
