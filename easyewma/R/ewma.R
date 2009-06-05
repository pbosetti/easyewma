ewma <-
function(data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 ) UseMethod("ewma")

