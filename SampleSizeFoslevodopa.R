library(pwr)

p2_values <- seq(0.01, 0.30, by = 0.01)

sample_sizes <- numeric(length(p2_values))


for (i in seq_along(p2_values)) {

    effect_size <- ES.h(p1 = 0.40, p2 = p2_values[i])
  
  result <- pwr.2p.test(h = effect_size, sig.level = 0.05, power = 0.80)
  
  sample_sizes[i] <- result$n
}



plot(p2_values, sample_sizes, type = "b", pch = 1, lty = 3, lwd = 2, col = "deepskyblue4", 
     xlab = "\n Expected Prevalence \n Among Foslevodopa", 
     ylab = "Required Sample Size ", 
     main = "Two-Proportion Test \n to Detect as Significant \n (Assuming a Reference of 40% for Duodopa)")

abline(h = 100, col = "firebrick", lty = 2)
index_n_100 <- which.min(abs(sample_sizes - 100))
abline(v = p2_values[index_n_100], col = "firebrick", lty = 2)
