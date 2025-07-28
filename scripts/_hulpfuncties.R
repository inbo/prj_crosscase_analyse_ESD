conflicted::conflict_prefer_all(c("dplyr"), quiet = TRUE)

# Functie om kwantielen en gemiddelde te berekenen
quantile_and_m <- function(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE, avg = TRUE) {
  quantiles <- quantile(x, probs = probs, na.rm = na.rm)
  mean_val <- mean(x, na.rm = na.rm)
  return(if(!avg) quantiles else c(quantiles, avg = mean_val))
}
