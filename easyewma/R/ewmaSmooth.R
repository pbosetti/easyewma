ewmaSmooth <-
function (x, y, lambda = 0.2, start, ...) 
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

