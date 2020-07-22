wquant <- function (x, weights, probs = c(0.025, 0.975)) {
  which <- !is.na(weights)
  x <- x[which]
  weights <- weights[which]
  
  if (all(is.na(x)) || length(x) == 0) return(rep(NA, length(probs)))
  
  idx <- order(x)
  x <- x[idx]
  weights <- weights[idx]
  w <- cumsum(weights)/sum(weights)
  rval <- approx(w,x,probs,rule=1)
  rval$y
}
