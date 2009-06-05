ewma.formula <-
function(formula, data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 )
{
  result <- ewmaCalc(model.frame(formula, data), mu0, sd, lambda, L, group.size, training)
  result$call <- match.call()
  class(result) <- "ewma"
  result
}

