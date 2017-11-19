preda <- function(x, k) {
  n <- length(x)
  print(n)
  pred <- vector(length = n - k)
  current_sum <- sum(x[1:k-1])
  for (i in 1:(n-k)) {
    current_sum <- current_sum + x[i+k-1]
    if (current_sum > k/2) pred[i] <- 1 else pred[i] <- 0
    current_sum <- current_sum - x[i]
  }
  print(pred)
  return(mean(abs(x[(1+k):n] - pred)))
}



preda(c(1, 1, 0, 1, 0, 1, 0, 0), 3)
