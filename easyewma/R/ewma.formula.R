ewma.formula <-
function(formula, data, mu0 = NA, sd = NA, lambda = 0.2, L = 3, group.size = 3, training = 5 )
{
  mat <- cbind(model.frame(formula, data)[2], model.frame(formula, data)[1]) 
  result <- ewmaCalc(mat, mu0, sd, lambda, L, group.size, training)
  result$call <- match.call()
  class(result) <- "ewma"
  result
}

