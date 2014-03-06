# Chapter 1 plots

png(filename = "figure/missing-plot.png")
plot.new()
text(0.5, 0.5, "Missing plot", cex = 1.5)
dev.off()

# Session 2

x <- 1:10
y1 <- c(8.5, 8, 7, 6, 5.3, 5, 5.2, 6.1, 6.6, 7)
y2 <- c(3.5, 5, 6, 7, 8, 8.2, 7, 6, 5, 3.5)
y3 <- exp(seq(0.94, 2.2, by = 0.14))

dades <- data.frame(x = c(1:10, 1:10, 1:10), y = c(y1, y2, y3), 
                    MSE = c(rep("R", 10), rep("LS", 10), rep("ML", 10)))

theme_xkcd <- theme(
  panel.background = element_rect(fill="white"),
  axis.ticks = element_line(colour=NA),
  panel.grid = element_line(colour="white"),
  axis.text.y = element_text(colour=NA),
  axis.text.x = element_text(colour=NA)
  
)

# http://stackoverflow.com/questions/12675147/how-can-we-make-xkcd-style-graphs-in-r
# http://drunks-and-lampposts.com/2012/10/02/clegg-vs-pleb-an-xkcd-esque-chart/

png(filename = "figure/MSE_estiamtors.png")
ggplot(aes(x = x, y = y, colour = MSE), data = dades) +
  geom_smooth(size=1, position="jitter", fill=NA, method = "loess") +
  ylim(2, 11) + labs(x = "", y = "") + theme_xkcd
dev.off()


lbinom <- function(theta, size = 10, succes = 6){
  
  choose(size, succes)*(theta^succes)*(1-theta)^(size - succes)
  
}

library(ggplot2)
x <- seq(0, 1, by = 0.01)
likelihod_binomial <- data.frame(x = x, y = lbinom(x, 10, 6))
area_menor_0.3 <- likelihod_binomial[likelihod_binomial$x <= 0.3,]
area_menor_0.4 <- likelihod_binomial[likelihod_binomial$x <= 0.4,]

png(filename = "figure/likelihood_binomial.png")
qplot(x, y, data = likelihod_binomial, geom = "line") + 
  geom_segment(x = 0.6, xend = 0.6, y = 0, yend = lbinom(0.6), colour = I("darkgreen")) +
  geom_segment(x = 0.4, xend = 0.4, y = 0, yend = lbinom(0.4), colour = I("darkgreen")) +
  geom_segment(x = 0.3, xend = 0.3, y = 0, yend = lbinom(0.3)) + 
  geom_area(data = area_menor_0.3 , fill = I("blue"), alpha = 0.3) + 
  geom_area(data = area_menor_0.4, fill = I("green"), alpha = 0.3) +
  annotate("text", x=0.35, y=0.01, parse=TRUE, size=6, label="2*zeta") +
  annotate("text", x=0.27, y=0.01, parse=TRUE, size=6, label="zeta")
dev.off()

# Comparing intervals doesn't make sense
cond <- likelihod_binomial$x >= 0.22 & likelihod_binomial$x <= 0.33
area_0.1_0.3 <- likelihod_binomial[cond,]

cond <- likelihod_binomial$x >= 0.36 & likelihod_binomial$x <= 0.46
area_0.4_0.5 <- likelihod_binomial[cond,]

png(filename = "figure/likelihood_binomial_intervals.png")
qplot(x, y, data = likelihod_binomial, geom = "line") + 
  geom_area(data = area_0.1_0.3, fill = I("darkgreen"), alpha = 0.3)  + 
  geom_area(data = area_0.4_0.5, fill = I("green"), alpha = 0.3) +
  geom_segment(x = 0.22, xend = 0.22, y = 0, yend = lbinom(0.22), colour = "darkgreen") +
  geom_segment(x = 0.33, xend = 0.33, y = 0, yend = lbinom(0.33), colour = "darkgreen") +
  geom_segment(x = 0.36, xend = 0.36, y = 0, yend = lbinom(0.36), colour = "green") +
  geom_segment(x = 0.46, xend = 0.46, y = 0, yend = lbinom(0.46), colour = "green") 
dev.off()

# Comparing interval in a reparametrized likelihood

x <- seq(0, 1, by = 0.01)
likelihod_binomial <- data.frame(x = x, y = lbinom(x, 10, 6))

cond <- likelihod_binomial$x >= 0.35 & likelihod_binomial$x <= 0.8
area_0.3_0.7 <- likelihod_binomial[cond,]

q1 <- qplot(x, y, data = likelihod_binomial, geom = "line") + 
  geom_segment(x = 0.35, xend = 0.35, y = 0, yend = lbinom(0.35), colour = "blue") +
  geom_segment(x = 0.8, xend = 0.8, y = 0, yend = lbinom(0.8), colour = "blue") + 
  geom_area(data = area_0.3_0.7, fill = I("blue"), alpha = 0.3) 

# Reparametrizing binomial likelihood

lbinom_rep <- function(theta, size = 10, succes = 6){
  
  choose(size, succes)*((1/theta)^succes)*(1-(1/theta))^(size - succes)
  
}

x <- seq(1, 6, by = 0.01)
likelihod_binomial <- data.frame(x = x, y = lbinom_rep(x, 10, 6))

cond <- likelihod_binomial$x >= 1/0.8 & likelihod_binomial$x <= 1/0.35
area_0.1_0.3 <- likelihod_binomial[cond,]

q2 <- qplot(x, y, data = likelihod_binomial, geom = "line") +
  geom_segment(x = 1/0.35, xend = 1/0.35, y = 0, yend = lbinom_rep(1/0.35), colour = "blue") +
  geom_segment(x = 1/0.8, xend = 1/0.8, y = 0, yend = lbinom_rep(1/0.8), colour = "blue") + 
  geom_area(data = area_0.1_0.3, fill = I("blue"), alpha = 0.3)

library(gridExtra)

png(filename = "figure/likelihood_binomial_reparametrization.png")
grid.arrange(q1, q2, ncol = 1)
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


