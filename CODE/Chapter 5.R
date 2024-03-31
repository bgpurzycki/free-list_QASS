#############################
### Ethnographic Free-List Data
### Chapter 5: Models, Prediction, and Uncertainty
### Benjamin Grant Purzycki
### Last Updated: March 26, 2024
################################

##############################################
### Preliminaries
rm(list = ls())
setwd()

mycol1 <- rgb(224, 224, 224, max = 255, alpha = 100, names = "lightgray") 
mycol2 <- rgb(255, 255, 255, max = 255, alpha = 100, names = "white")
mycol3 <- rgb(0, 0, 0, max = 255, alpha = 100, names = "darkgray")

library(AnthroTools)
library(xtable)
library(rethinking)

# Installing rethinking (and other packages that require stan) is a little
# more involved. Go here: https://github.com/rmcelreath/rethinking for
# instructions.

##############################################
### Table 5.1: Picking apart the mean

## The Mean as Model
obs <- c(2, 5, 5, 6, 10, 10, 12, 18, 29, 29)
barplot(table(obs))
count <- c(1, 2, NA, 1, 2, NA, 1, 1, 2, NA)
d <- data.frame(cbind(obs, count))
n <- length(d$obs)
d$cntdn <- count/n
d$timesobs <- d$cntdn*d$obs
d$dev <- d$obs - mean(d$obs)
d$dev.sq <- d$dev^2
d

## standard deviation
(variance <- (sum(d$dev.sq))/(n - 1)) # var with Bessel-Gauss' correction
(sqrt(variance)) # standard deviation
sd(d$obs)

## z-scores
d$z <- (d$obs - mean(d$obs))/sd(d$obs) # (observation - mean)/sd(variable)

##############################################
### Figure 5.1: Accounting for deviance from the mean

par(mar = c(1, 4, 1, 1))
plot(obs, ylim = c(0, 30),
     ylab = "value", xlab = NA, axes = F, frame.plot = T, pch = 16)
Axis(side = 2, labels = T)
abline(h = mean(obs), lty = 2)
arrows(1, 2, 1, mean(obs), length = 0) # x0, y0, x1, y1
arrows(2, 5, 2, mean(obs), length = 0) # x0, y0, x1, y1
arrows(3, 5, 3, mean(obs), length = 0) # x0, y0, x1, y1
arrows(4, 6, 4, mean(obs), length = 0) # x0, y0, x1, y1
arrows(5, 10, 5, mean(obs), length = 0) # x0, y0, x1, y1
arrows(6, 10, 6, mean(obs), length = 0) # x0, y0, x1, y1
arrows(7, 12, 7, mean(obs), length = 0) # x0, y0, x1, y1
arrows(8, 18, 8, mean(obs), length = 0) # x0, y0, x1, y1
arrows(9, 29, 9, mean(obs), length = 0) # x0, y0, x1, y1
arrows(10, 29, 10, mean(obs), length = 0) # x0, y0, x1, y1

##############################################
### Figure 5.2: z-distribution curve

par(mar = c(4, 4, 1, 1))
curve(dnorm(x, 0, 1), xlim = c(-4, 4), 
      main = NULL, xlab = expression(italic(z)-score), ylab = "Probability")
cord.x <- c(-1.96, seq(-1.96, 1.96, 0.01), 1.96) # +/-1.96 on a z-distribution
cord.y <- c(0, dnorm(seq(-1.96, 1.96, 0.01)), 0) 
polygon(cord.x, cord.y, col = 'gray') 
lines(c(0, 0), c(0, dnorm(0, 0, 1)), lty = 3) # the mean!

# shaded area is 95% of the curve which is +/- 1.96 standard
# errors above and below the mean

## Standard error and confidence intervals
(se <- sd(d$obs)/sqrt(n)) # standard error
(upper <- mean(d$obs) + se * 1.96) # upper bound of CI
(lower <- mean(d$obs) - se * 1.96) # lower bound of CI

# M = 12.6, 95% CI = [6.57, 18.63] 

## Confidence interval of the mean function

CI.mean <- function(variable){
  mean <- mean(variable, na.rm = T)
  stdev <- sd(variable, na.rm = T)
  n <- length(which(!is.na(variable)))
  se <- stdev/sqrt(n)
  lower <- mean - se * 1.96
  upper <- mean + se * 1.96
  df1 <- data.frame(mean, 
                    round(stdev, digits = 2), 
                    round(lower, digits = 2), 
                    round(upper, digits = 2))
  colnames(df1) <- c("mean", "sd", "lower", "upper")
  rownames(df1) <- c("normal") # calculated assuming normality
  return(df1)
}

mean(d$obs)
sd(d$obs)
CI.mean(d$obs)

CI.mean(rnorm(10, mean(d$obs), sd(d$obs))) # run a bunch of times!
CI.mean(rnorm(100, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(1000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(10000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(100000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(1000000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals

# 95% of any repeated observations will yield intervals
# that contain the population or "true" mean. So, if we replicated this study
# 100 times, 95 of the intervals will contain the population mean. The wider
# the interval then, the less precise the mean is as an estimate--
# or model of the true mean. Very narrow intervals represent very
# precise estimates of the mean. 

simdat <- rnorm(1000, mean = 12.6, sd = 9.73)
CI.mean(simdat) # compare the confidence intervals

simdat2 <- rnorm(1000, mean = 12.6, sd = 1) # SD!
CI.mean(simdat2) # compare the confidence intervals

## Graph it!

par(mfrow = c(2, 1)) # splits the graph panels

hist(simdat, xlab = NA, ylab = NA, prob = TRUE,
     main = "Simulated Data I (M = 12.6, SD = 9.73)", 
     xlim = c(-20, 50), breaks = 15)
curve(dnorm(x, 12.6, 9.73), xlim = c(-20, 50), 
      main = NULL, add = TRUE) 

hist(simdat2, xlab = NA, ylab = NA, prob = TRUE,
     main = "Simulated Data II (M = 12.6, SD = 1)", 
     xlim = c(-20, 50), breaks = 15)
curve(dnorm(x, 12.6, 1), xlim = c(-20, 50), 
      main = NULL, add = TRUE) 

##############################################
### Footnote 2 Reference: Poisson Model

n <- 500
plot(density(rpois(n, lambda = 0.5)))
plot(density(rpois(n, lambda = 5)))
plot(density(rpois(n, lambda = 10)))

n <- 500
alpha <- 5
beta <- 0.7
x <- runif(n, min = 0, max = 1)
lambda <- exp(alpha + beta * x)
y <- rpois(n, lambda = lambda) 
plot(log(y) ~ x, log = "y", pch = 16)
mpoisson <- glm(y ~ x, family = "poisson")
confint(mpoisson)

##############################################
### R Code Box 5.1: Simple OLS regression
### Table 5.2: Fictitious data of participant age and number of items listed

PARTID <- paste0("ID", seq(1:20))
x <- c(1, 28, 16, 40, 42, 30, 4, 25, 7, 19, 33, 35, 40, 10, 43, 10, 45, 19, 18, 28)
y <- c(9, 20, 17, 26, 33, 24, 10, 23, 10, 15, 23, 25, 21, 12, 35, 15, 32, 15, 18, 23)
d <- data.frame(PARTID, y, x)

(m1 <- lm(y ~ x, data = d))
summary(m1)
confint(m1)

## Calculating these step by step
# Regression
d$xd <- x - mean(x)
d$yd <- y - mean(y)
d$xdsq <- d$xd^2
d$xy <- d$xd*d$yd

beta <- (sum(d$xy))/(sum(d$xdsq))
alpha <- mean(y) - beta*mean(x)

# Residual standard error
d$ypred <- alpha + beta * x
d$preddiff <- y - d$ypred
d$preddiffsq <- d$preddiff^2
SSE <- sum(d$preddiffsq)
var.e <- SSE/(nrow(d) - 2)
resstderr <- sqrt(var.e)

# Std. error of intercept
alpha.se <- resstderr * sqrt((1/nrow(d)) + ((mean(x)^2) / sum(d$xdsq)))

# Std. error of slope
beta.se <- sqrt(var.e / sum(d$xdsq))

# t-statistics
t.alpha <- alpha/alpha.se
t.beta <- beta/beta.se

# 95% CIs
beta.up <- beta + qt(0.975, nrow(d) - 2)*beta.se
beta.lo <- beta - qt(0.975, nrow(d) - 2)*beta.se
alpha.up <- alpha + qt(0.975, nrow(d) - 2)*alpha.se
alpha.lo <- alpha - qt(0.975, nrow(d) - 2)*alpha.se

# Table 5.3: Regression output
outtab <- round(data.frame(est = c(alpha, beta), se = c(alpha.se, beta.se), 
                           tstat = c(t.alpha, t.beta), lower = c(alpha.lo, beta.lo),
           upper = c(alpha.up, beta.up), row.names = c("alpha", "beta_age")), 2)
xtable(outtab) # for LaTeX users

##############################################
### Figure 5.3: Regression of fake data from Table 5.1

par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
plot(y ~ x, data = d,
     ylab = "# of items listed", xlab = "age",
     xlim = c(0, 50), ylim = c(0, 40),
     pch = 16)
abline(m1)

newxs <- seq(min(x), max(x), length.out = 100)
preds <- predict(m1, newdata = data.frame(x = newxs), 
                 interval = "confidence")

polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency

points(0, alpha)

##############################################
### Figure 5.4: Uncertainty interval demo

mycol <- rgb(128, 128, 128, max = 255, alpha = 25, names = "vapor")

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
n <- 100 # sample size
x <- rnorm(n, 0, 10) # predictor
y <- .5*x + rnorm(n, 0, 10) # outcome

plot(y ~ x, xlim = c(-30, 30), ylim = c(-30, 30),
     xlab = expression(italic("x")), 
     ylab = expression(italic("y")), pch = 16)
abline(lm(y ~ x))
newx <- seq(min(x), max(x), length.out = 100)
preds <- predict(lm(y ~ x), newdata = data.frame(x = newx), 
                 interval = "confidence")
polygon(c(rev(newx), newx), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency
text(-25, 25, "(a)")

regsim <- function(n, xbar, sd, beta){ # function to resample
  x <- rnorm(n, xbar, sd)
  y <- beta * x + rnorm(n, xbar, sd)
  abline(lm(y ~ x), col = mycol1)
}

plot(NA, xlim = c(-30, 30), ylim = c(-30, 30),
     xlab = expression(italic("x")), ylab = expression(italic("y")))
replicate(500, regsim(100, 0, 10, 0.5))
text(-25, 25, "(b)")

##############################################
### Figure 5.5: Anscombe's Quartet

d <- datasets::anscombe

m1 <- lm(y1 ~ x1, data = d)
m2 <- lm(y2 ~ x2, data = d)
m3 <- lm(y3 ~ x3, data = d)
m4 <- lm(y4 ~ x4, data = d)

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
plot(y1 ~ x1, data = d, pch = 16, xlim = c(4, 18), ylim = c(4, 12))
newxs <- seq(min(d$x1), max(d$x1), length.out = 100)
preds <- predict(m1, newdata = data.frame(x1 = newxs), 
                 interval = "confidence")
polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency
abline(m1)

plot(y2 ~ x2, data = d, pch = 16, xlim = c(4, 18), ylim = c(4, 12))
newxs <- seq(min(d$x2), max(d$x2), length.out = 100)
preds <- predict(m2, newdata = data.frame(x2 = newxs), 
                 interval = "confidence")
polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency
abline(m2)

plot(y3 ~ x3, data = d, pch = 16, xlim = c(4, 18), ylim = c(4, 12))
newxs <- seq(min(d$x3), max(d$x3), length.out = 100)
preds <- predict(m3, newdata = data.frame(x3 = newxs), 
                 interval = "confidence")
polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency
abline(m3)

plot(y4 ~ x4, data = d, pch = 16, xlim = c(4, 18), ylim = c(4, 12))
newxs <- seq(min(d$x4), max(d$x4), length.out = 100)
preds <- predict(m4, newdata = data.frame(x4 = newxs), 
                 interval = "confidence")
polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency
abline(m4)

##############################################
### Figure 5.6. Examples of Priors

par(mfrow = c(2, 2), mar = c(4.5, 2, 1, 1))
curve(dnorm(x, 10, 1), from = -5, to = 20, 
      ylab = NA, lwd = 1.5, xlab = "")
curve(dnorm(x, 10, 5), from = -5, to = 20, 
      ylab = NA, lwd = 1.5, xlab = "", add = T, lty = 2)
legend(-4.5, 0.38, lty = c(1, 2), legend = c("= 1", "= 5"),
       title = expression(sigma), cex = 1.2, lwd = 1.5)
mtext(expression(paste("Normal(10, ", sigma, ")")), 1, padj = 2.5)
curve(dnorm(x, -5, 1), from = -10, to = 0, 
      ylab = NA, lwd = 1.5, xlab = "")
mtext("Normal(-5, 1)", 1, padj = 3)
curve(dunif(x, 0, 5), from = -5, to = 10, 
      ylab = NA, lwd = 1.5, xlab = "")
mtext("Uniform(0, 5)", 1, padj = 3)
plot(NA, xlim = c(0, 10), ylim = c(0, 1), xlab = NA)
curve(dexp(x, rate = .25), 
      from = 0, to = 10, col = 'black', add = T, lty = 1, lwd = 1.5)
curve(dexp(x, rate = .5), 
      from = 0, to = 10, col = 'black', add = T, lty = 2, lwd = 1.5)
curve(dexp(x, rate = 1), 
      from = 0, to = 10, col = 'black', add = T, lty = 3, lwd = 1.5)
legend(3.8, .9, lty = c(1, 2, 3), legend = c("= 0.25", "= 0.5", "= 1"), 
       title = expression(lambda), cex = 1.2, lwd = 1.5)
mtext(expression(paste("Exponential", (lambda))), 1, padj = 2.2)

##############################################
### Figure 5.7. Simulated posteriors

set.seed(777)

n <- 150
x <- rnorm(n, 5, 1)
alpha <- rnorm(n, 10, 0.5)
b <- 0.7
y <- alpha + b*x + rnorm(n, 0, 1)

dat <- data.frame(y, x)

# betas
bll <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(10, 1),
    bx ~ dnorm(0, 1),
    sigma ~ exp(1)
  ), data = dat)
precis(bll, prob = .95)

bhl <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(5, 5),
    bx ~ dnorm(10, 1),
    sigma ~ exp(1)
  ), data = dat)
precis(bhl, prob = .95)

bhh <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(10, 1),
    bx ~ dnorm(10, 10),
    sigma ~ exp(1)
  ), data = dat)
precis(bhh, prob = .95)

postbll <- extract.samples(bll)
postbhl <- extract.samples(bhl)
postbhh <- extract.samples(bhh)

# alphas
all <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(1, 1),
    bx ~ dnorm(0.5, 1),
    sigma ~ exp(1)
  ), data = dat)
precis(all, prob = .95)

alh <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(1, 10),
    bx ~ dnorm(0.5, 1),
    sigma ~ exp(1)
  ), data = dat)
precis(alh, prob = .95)

ahl <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(10, 1),
    bx ~ dnorm(0.5, 1),
    sigma ~ exp(1)
  ), data = dat)
precis(ahl, prob = .95)

ahh <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(10, 10),
    bx ~ dnorm(0.5, 1),
    sigma ~ exp(1)
  ), data = dat)
precis(ahh, prob = .95)

# Middling
bmm <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bx*x,
    a ~ dnorm(10, 10),
    bx ~ dnorm(10, 10),
    sigma ~ exp(1)
  ), data = dat)
precis(bmm, prob = .95)

postall <- extract.samples(all)
postalh <- extract.samples(alh)
postahl <- extract.samples(ahl)
postahh <- extract.samples(ahh)

postbmm <- extract.samples(bmm)

# Graph
par(mfrow = c(4, 2), mar = c(4, 2.5, 1, 1))

plot(density(postbll$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postbll$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (0, 1), ", alpha, "= (10, 1)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(a)")

plot(density(postbhl$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postbhl$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (5, 5), ", alpha, "= (10, 1)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(b)")

plot(density(postbhh$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postbhh$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (10, 10), ", alpha, "= (10, 1)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(c)")

plot(density(postall$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postall$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (0.5, 1), ", alpha, "= (1, 1)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(d)")

plot(density(postalh$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postalh$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (0.5, 1), ", alpha, "= (1, 10)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(e)")

plot(density(postahl$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postahl$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (0.5, 1), ", alpha, "= (10, 1)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(f)")

plot(density(postahh$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postahh$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (0.5, 1), ", alpha, "= (10, 10)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(g)")

plot(density(postbmm$bx), xlim = c(-.5, 2), ylim = c(0, 3), 
     main = NA, xlab = NA)
polygon(density(postbmm$bx), col = mycol1)
abline(v = b, lty = 2)
mtext(expression(paste(beta, "= (10, 10), ", alpha, "= (10, 10)")), side = 1, padj = 2.5, cex = .8)
text(-0.4, 2.7, "(h)")
