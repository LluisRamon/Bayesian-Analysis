# Chapter 1 plots

png(filename = "figure/missing-plot.png")
plot.new()
text(0.5, 0.5, "Missing plot", cex = 1.5)
dev.off()

# Session 2

lbinom <- function(theta, size = 10, succes = 6){
  
  choose(size, succes)*(theta^succes)*(1-theta)^(size - succes)
  
}

x <- seq(0, 1, by = 0.01)

png(filename = "figure/likelihood_binomial.png")
plot(x, lbinom(x, 10, 6), type = 'l', xlab= '', ylab= '')
abline(v = 0.6, col = 'red')
dev.off()

# Session 4



png(filename = "figure/beta_prior_pos_50_50.png")
plot(dbeta(x, 56, 54), col = "red", type = "l")
lines(dbeta(x, 50, 50))
dev.off()

png(filename = "figure/beta_prior_pos_2_2.png")
plot(dbeta(x, 8, 6), col = "red", type = "l")
lines(dbeta(x, 2, 2))
dev.off()
