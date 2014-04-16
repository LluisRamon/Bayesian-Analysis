library("grid")
library("ggplot2")
library("gridExtra")

nothing_theme <- theme(title = element_text(size = 20, face = "bold.italic"), 
                       panel.background = element_blank(), panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(), axis.ticks = element_blank(),
                       axis.text.x = element_blank(), axis.text.y = element_blank(),
                       axis.title.x = element_blank(), axis.title.y = element_blank())

# Diagram 1 statistical model ------------------------------------------------

x <- c(1, 2, 2, 1, 1)
y <- c(1, 1, 2, 2, 1)

diagram <- data.frame(x, y)

dibuix <- ggplot(data = diagram, aes(x = x, y = y)) + ylim(0, 2) + xlim(0.75, 2.5) +
  geom_path() + 
  geom_segment(aes(x = 1.5, y = 1.25, xend = 1.5, yend = 0.5), arrow = arrow(length = unit(0.2, "cm"))) 

eq <- "'{P(y| ' * theta * '):' * theta %in% Omega * '}'"
def <- "'Inference game: guesing what ' * theta^bold('*') %in% Omega"
def2 <- "'is the one responsible for ' * Y == y"

(diagram1 <- dibuix + 
   annotate("text", x = 1.5, y = 1.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 2.25, y = 1.6, size = 6, label = "List of \n probability models") +
   annotate("text", x = 1.6, y = 0.75, parse = TRUE, size = 6, label = "Y == y") +
   annotate("text", x = 1.5, y = 0.25, parse = TRUE, size = 6, label = def) + 
   annotate("text", x = 1.5, y = 0.125, parse = TRUE, size = 6, label = def2))

png(filename = "figure/Statistical_model.png", width = 680)
diagram1 +  ggtitle("Statistical model") + nothing_theme
dev.off()


# Diagram 2 binomial model ------------------------------------------------

eq <- "'{Binomial | (10,' * theta * ')}'"
def1 <- "'a) Point estimation   '"
def2 <- "'b) Interval estimation'"
def3 <- "'c) Hipothesis testing '"

dibuix <- ggplot(data = diagram, aes(x = x, y = y)) + ylim(- 0.25, 2) + xlim(0.75, 2.25) +
  geom_path() + 
  geom_segment(aes(x = 1.5, y = 1.25, xend = 1.5, yend = 0.5), arrow = arrow(length = unit(0.2, "cm"))) 


(diagram2 <- dibuix + 
   annotate("text", x = 1.5, y = 1.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.6, y = 0.75, parse = TRUE, size = 6, label = "Y == 4") +
   annotate("text", x = 1.5, y = 0.25, parse = TRUE, size = 6, label = def1) + 
   annotate("text", x = 1.5, y = 0.125, parse = TRUE, size = 6, label = def2) + 
   annotate("text", x = 1.5, y = 0, parse = TRUE, size = 6, label = def3))

png(filename = "figure/exemple_binomial.png")
diagram2 +  ggtitle("Binomial model") + nothing_theme
dev.off()


# Diagram 3 stat_binom model ------------------------------------------------

x <- c(1, 2, 2, 1, 1)
y <- c(1, 1, 2, 2, 1)

diagram <- data.frame(x, y)

dibuix <- ggplot(data = diagram, aes(x = x, y = y)) + ylim(- 0.25, 2) + xlim(0.75, 2.25) +
  geom_path() + 
  geom_segment(aes(x = 1.5, y = 1.25, xend = 1.5, yend = 0.5), arrow = arrow(length = unit(0.2, "cm"))) 

eq <- "'{P(y| ' * theta * '):' * theta %in% Omega * '}'"


(diagram1 <- dibuix + 
   annotate("text", x = 1.5, y = 1.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.7, y = 0.75, parse = TRUE, size = 6, label = "Y == y") +
   annotate("text", x = 1.5, y = 0.35, parse = TRUE, size = 6, label = "'???'"))

eq <- "'{Binomial | (10,' * theta * ')}'"

expl1 <- "'And we want to guess which'"
expl2 <- "theta^bold('*') %in% Omega * ' generated ' * Y == 4"

dibuix <- ggplot(data = diagram, aes(x = x, y = y)) + ylim(- 0.25, 2) + xlim(0.75, 2.25) +
  geom_path() + 
  geom_segment(aes(x = 1.5, y = 1.25, xend = 1.5, yend = 0.5), arrow = arrow(length = unit(0.2, "cm"))) 

(diagram2 <- dibuix + 
   annotate("text", x = 1.5, y = 1.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.7, y = 0.75, parse = TRUE, size = 6, label = "Y == 4") + 
   annotate("text", x = 1.5, y = 0.25, parse = TRUE, size = 6, label = expl1) +
   annotate("text", x = 1.5, y = 0.125, parse = TRUE, size = 6, label = expl2))


p1 <- diagram1 +  ggtitle("Statistical model") + nothing_theme
p2 <- diagram2 +  ggtitle("Binomial model") + nothing_theme

png(filename = "figure/stat_mod_binom.png", width = 680)
grid.arrange(p1, p2, ncol = 2)
dev.off()

# Diagram 4 bayesian model ------------------------------------------------

x <- c(1, 2, 2, 1, 1) + 0.15*c(1, -1, -1, 1, 1)
y <- c(1, 1, 2, 2, 1) + 0.15*c(1, 1, -1, -1, 1)

diagram <- data.frame(x, y)

dibuix <- ggplot(data = diagram, aes(x = x, y = y)) + ylim(0.4, 2) + xlim(0.75, 2.25) +
  geom_path() + 
  geom_segment(aes(x = 1.5, y = 1.3, xend = 1.5, yend = 0.75), arrow = arrow(length = unit(0.2, "cm"))) 

eq <- "'{P(y| ' * theta * '):' * theta %in% Omega * '}'"
pi <- "pi * '(' * theta * ')'"
def <- "'???'"


(diagram4 <- dibuix + 
   annotate("text", x = 1.5, y = 1.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.5, y = 1.4, parse = TRUE, size = 7, label = pi) + 
   annotate("text", x = 1.5, y = 0.65, parse = TRUE, size = 6, label = def))

png(filename = "figure/bayesian_model.png")
diagram4 +  ggtitle("Bayesian model") + nothing_theme
dev.off()



# Diagram 5 bayesian model ------------------------------------------------

data <- "'Data'"
eq <- "y == 6"
lik <- "italic(l)[y] * '(' * theta * ')'"

(diagram5 <- diagram4 + 
  annotate("text", x = 1.65, y = 1.05, parse = TRUE, size = 6, label = data) +
  annotate("text", x = 1.65, y = 0.95, parse = TRUE, size = 6, label = eq) + 
  annotate("text", x = 1.65, y = 0.85, parse = TRUE, size = 6, label = lik))

png(filename = "figure/bayesian_model_amp.png")
diagram5 +  ggtitle("Bayesian model") + nothing_theme
dev.off()


# Diagram 6 All models compared -------------------------------------------

eq <- "'{P(y| ' * theta * '):' * theta %in% Omega * '}'"
def1 <- "'a) Point estimation   '"
def2 <- "'b) Interval estimation'"
def3 <- "'c) Hipothesis testing '"

diagram1 <- diagram
diagram1$y <- diagram1$y + 1

dibuix <- ggplot(data = diagram1, aes(x = x, y = y)) + ylim(- 0.25, 3) + xlim(0.75, 2.25) +
  geom_path() + 
  geom_segment(aes(x = 1.5, y = 2.25, xend = 1.5, yend = 1.125), arrow = arrow(length = unit(0.2, "cm"))) 

text1 <- "italic('You get data')"
text2 <- "Y == y"

(diagram2 <- dibuix + 
   annotate("text", x = 1.5, y = 2.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.775, y = 1.6, parse = TRUE, size = 6, label = text1) +
   annotate("text", x = 1.65, y = 1.4, parse = TRUE, size = 6, label = text2) +
   annotate("text", x = 1.5, y = 0.75, parse = TRUE, size = 6, label = def1) + 
   annotate("text", x = 1.5, y = 0.625, parse = TRUE, size = 6, label = def2) + 
   annotate("text", x = 1.5, y = 0.5, parse = TRUE, size = 6, label = def3))

fre_mod <- diagram2 +  ggtitle("Frequentist model") + nothing_theme

diagram2 <- diagram
diagram2$y <- diagram2$y -1

dibuix2 <- dibuix + geom_path(data = diagram2)

lik <- "italic(l)[y] * '(' * theta * ')'"

(diagram2 <- dibuix2 + 
   annotate("text", x = 1.5, y = 2.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.775, y = 1.6, parse = TRUE, size = 6, label = text1) +
   annotate("text", x = 1.65, y = 1.4, parse = TRUE, size = 6, label = text2) +
   annotate("text", x = 1.5, y = 0.7, parse = TRUE, size = 6, label = eq) + 
   annotate("text", x = 1.5, y = 0.4, parse = TRUE, size = 6, label = lik))

lik_mod <- diagram2 +  ggtitle("Likelihood based model") + nothing_theme

prior <- "pi * '(' * theta * ')'"
posterior <- "pi * '(' * theta * '| y)'"

(diagram2 <- dibuix2 + 
   annotate("text", x = 1.5, y = 2.6, parse = TRUE, size = 7, label = eq) +
   annotate("text", x = 1.5, y = 2.4, parse = TRUE, size = 7, label = prior) +
   annotate("text", x = 1.775, y = 1.6, parse = TRUE, size = 6, label = text1) +
   annotate("text", x = 1.65, y = 1.4, parse = TRUE, size = 6, label = text2) +
   annotate("text", x = 1.5, y = 0.7, parse = TRUE, size = 6, label = eq) + 
   annotate("text", x = 1.5, y = 0.5, parse = TRUE, size = 6, label = posterior))

bay_mod <- diagram2 +  ggtitle("Bayesian model") + nothing_theme

png(filename = "figure/all_models.png", width = 980)
grid.arrange(fre_mod, lik_mod, bay_mod, ncol = 3)
dev.off()



