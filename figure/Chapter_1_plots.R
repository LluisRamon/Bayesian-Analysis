# Chapter 1 plots

png(filename = "figure/missing-plot.png")
plot.new()
text(0.5, 0.5, "Missing plot", cex = 1.5)
dev.off()

# Session 4

x <- seq(0, 1, by = 0.01)

png(filename = "figure/beta_prior_pos_50_50.png")
plot(dbeta(x, 56, 54), col = "red", type = "l")
lines(dbeta(x, 50, 50))
dev.off()

png(filename = "figure/beta_prior_pos_2_2.png")
plot(dbeta(x, 8, 6), col = "red", type = "l")
lines(dbeta(x, 2, 2))
dev.off()
