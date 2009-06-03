ewma <-
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
    attr(result, "class")<-"ewma"
    return(result)
  }

