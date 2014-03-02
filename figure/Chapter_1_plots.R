# Chapter 1 plots

png(filename = "figure/missing-plot.png")
plot.new()
text(0.5, 0.5, "Missing plot", cex = 1.5)
dev.off()

# Session 2

lbinom <- function(theta, size = 10, succes = 6){
  
  choose(size, succes)*(theta^succes)*(1-theta)^(size - succes)
  
}

library(ggplot2)
x <- seq(0, 1, by = 0.01)
likelihod_binomial <- data.frame(x = x, y = lbinom(x, 10, 6))

png(filename = "figure/likelihood_binomial.png")
qplot(x, y, data = likelihod_binomial, geom = "line") + 
  geom_segment(x = 0.6, xend = 0.6, y = 0, yend = lbinom(0.6), colour = I("red")) +
  geom_segment(x = 0.4, xend = 0.4, y = 0, yend = lbinom(0.4)) +
  geom_segment(x = 0.3, xend = 0.3, y = 0, yend = lbinom(0.3))
dev.off()



# Session 4
library(ggplot2)

prior <- data.frame(x = x, y = dbeta(x, 50, 50), dist = "prior (50, 50)", stringsAsFactors= FALSE)
posterior <- data.frame(x = x, y = dbeta(x, 56, 54), dist = "posterior (56, 54)", stringsAsFactors= FALSE)
beta_prior_pos <- rbind(prior, posterior)

png(filename = "figure/beta_prior_pos_50_50.png")
qplot(x, y, colour = dist, data = beta_prior_pos, geom = "line", 
      main = "Prior and Posterior Beta distribution")
dev.off()

prior <- data.frame(x = x, y = dbeta(x, 2, 2), dist = "prior (2, 2)", stringsAsFactors= FALSE)
posterior <- data.frame(x = x, y = dbeta(x, 8, 6), dist = "posterior (8, 6)", stringsAsFactors= FALSE)
beta_prior_pos <- rbind(prior, posterior)

png(filename = "figure/beta_prior_pos_2_2.png")
qplot(x, y, colour = dist, data = beta_prior_pos, geom = "line", 
      main = "Prior and Posterior Beta distribution")
dev.off()

# Session 5

beta1 <- data.frame(x = x, y = dbeta(x, 0.5, 0.5), dist = "(0.5, 0.5)", stringsAsFactors= FALSE)
beta2 <- data.frame(x = x, y = dbeta(x, 5, 1), dist = "(5, 1)", stringsAsFactors= FALSE)
beta3 <- data.frame(x = x, y = dbeta(x, 1, 3), dist = "(1, 3)", stringsAsFactors= FALSE)
beta4 <- data.frame(x = x, y = dbeta(x, 2, 2), dist = "(2, 2)", stringsAsFactors= FALSE)
beta5 <- data.frame(x = x, y = dbeta(x, 2, 5), dist = "(2, 5)", stringsAsFactors= FALSE)
beta6 <- data.frame(x = x, y = dbeta(x, 5, 2), dist = "(5, 2)", stringsAsFactors= FALSE)

betas <- rbind(beta1, beta2, beta3, beta4, beta5, beta6)

png(filename = "figure/Beta_distribution.png")
qplot(x, y, colour = dist, data = betas, geom = "line", 
      main = "Beta distribution examples")
dev.off()


