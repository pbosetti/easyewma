group.by <-
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

