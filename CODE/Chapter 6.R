####################################################
### Ethnographic Free-List Data: Chapter 6
### SAGE
### Benjamin Grant Purzycki
### Last Updated: October 6, 2023
####################################################

### Preliminaries
setwd()

### Packages used in this chapter
library(AnthroTools)
library(xtable)
library(rethinking)
library(plyr)
library(brms)
library(ggplot2)
library(ggExtra)
library(papaja)
library(tidyr)
library(dplyr)
library(tibble)
library(rstanarm)

####################################################
### R Code Box 6.1: Simulating Data with a Gaussian Outcome
set.seed(777)
n <- 100 # sample size
alpha <- 7 # intercept
b_age <- 0.7 # bage
b_sex <- -2 # bsex

age <- rnorm(n, 30, 7) # age
age.c <- age - mean(age)
sex <- rbinom(n, 1, 0.5) # sex
noise <- rnorm(n, 3, 5) # extra realism
fl <- alpha + b_age * age.c + b_sex * sex + noise # linear model
d <- data.frame(fl, age.c, sex)

### Figure 6.1: Plots from simulated data predicting list lengths
m1 <- quap(
  alist(
    fl ~ dnorm(mu, sigma),
    mu <- a + bage * age.c + bsex * sex,
    a ~ dnorm(10, 4),
    bage ~ dnorm(0, 1),
    bsex ~ dnorm(0, 1),
    sigma ~ exp(1)
  ), data = d)

precis(m1, prob = 0.95)
post <- extract.samples(m1)

age.seq <- seq(-20, 20, by = 1)
flsim <- sim(m1, data = list(age.c = age.seq, sex = 0.5))
flPI <-  apply(flsim, 2, PI, prob = 0.95)

mu <- link(m1, data = data.frame(age.c = age.seq, sex = 0.5))
mu <- mu$mu
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# To examine the differences between males and non-males
#age.seq <- seq(-20, 20, by = 1)
#mu.female <- link(m1, data = data.frame(sex = 0, age.c = age.seq), n = 100)
#mu.female <- mu.female$mu
#mu.male <- link(m1, data = data.frame(sex = 1, age.c = age.seq), n = 100)
#mu.male <- mu.male$mu

#mu.female.mean <- apply(mu.female, 2, mean)
#mu.female.PI <- apply(mu.female, 2, PI, prob = 0.95)

#mu.male.mean <- apply(mu.male, 2, mean)
#mu.male.PI <- apply(mu.male, 2, PI, prob = 0.95)

#plot(fl ~ age.c, data = d)
#lines(age.seq, mu.female.mean)
#lines(age.seq, mu.male.mean)
#shade(mu.male.PI, age.seq)
#shade(mu.female.PI, age.seq)

# Plot
plot.mat = matrix(c(1, 1, 1, 2, 2, 2,
                    3, 3, 3, 3, 3, 3),
                  nrow = 2, byrow = T)
layout(plot.mat)
#layout.show(n = 3)

par(mar = c(3, 4, 1, 1))
plot(fl ~ age.c, pch = 16, 
     xlab = NA, ylab = NA, data = d,
     xlim = c(-20, 20), ylim = c(-5, max(fl)))
mtext("age (centered)", 1, padj = 3.5, cex = .8)
mtext("# listed", 2, padj = -3.5, cex = .8)
lines(age.seq, mu.mean)
#shade(mu.HPDI, age.seq)
shade(flPI, age.seq)
text(-17, 20, "(a)")

boxplot(fl ~ sex,
        xlab = NA, ylab = NA)
mtext("sex (1 = male)", 1, padj = 3.5, cex = .8)
mtext("# listed", 2, padj = -3.5, cex = .8)
text(.9, 20, "(b)", adj = 2.5)

labs <- c("intercept", "age.c", "sex")
x <- seq(1, length(labs), by = 1)

OR <- c(mean(post$a), mean(post$bage), mean(post$bsex))
LL <- precis(m1, prob = 0.95)[,3]
UL <- precis(m1, prob = 0.95)[,4]
LS <- OR-LL
US <- UL-OR

par(mar = c(4, 5, 1, 1))
plot(OR, x, pch = 16, xlim = c(-5, 12), ylim = c(0.5, 4), 
     xlab = NA, 
     ylab = NA, yaxt = "n", frame.plot = F)
arrows(x0 = OR-LS, y0 = x, x1 = US + OR, y1 = x, code = 3, angle = 90, length = 0.05)
abline(v = 0, lty = 2)
axis(2, at = x, labels = labs, las = 2, cex = 0.8)
mtext("estimate", 1, padj = 2.5, cex = 0.8)
text(-3.7, 3.5, "(c)", adj = 2.5)

#################################################
# rstanarm version (not in text)
betas <- normal(location = c(.5, -1), scale = c(1, 1), autoscale = F)
alphas <- normal(location = 10, scale = 4, autoscale = F)
sigmapri <- exponential(1, autoscale = FALSE)  

m2 <- stan_glm(fl ~ age.c + sex, data = d,
               prior = betas,
               prior_intercept = alphas,
               prior_aux = sigmapri)
prior_summary(m2)

print(m2)
post2 <- as.matrix(m2)
posterior_interval(m2, prob = 0.95)
mean(post2[,1]) # intercept
mean(post2[,2]) # age.c
mean(post2[,3]) # sex
mean(post2[,4]) # sigma

# Simulating regression with Binomial Outcome (not in text)
# Logistic regression simulation

logistic <- function(x){
  out <- 1/(1 + exp(-x))
  return(out)
}

logit <- function(p){
  out <- log(p/(1 - p))
  return(out)
}

n <- 1000 # sample size
a <- .7 # intercept (base prob. of 1)

# with no predictor
anthro <- rbinom(n, 1, a)
table(anthro)
(m0 <- glm(anthro ~ 1, family = binomial))
logistic(coef(m0))

# with binary predictor
sex <- sample(c(0, 1), size = n, replace = TRUE)
mod <- logit(a) + logit(.2)*sex
probs <- 1/(1 + exp(-mod))
anthro <- rbinom(n, 1, probs)
(m1 <- glm(anthro ~ sex, family = "binomial"))
logistic(coef(m1))

# with continuous predictor
age <- rnorm(n, 30, 5)
agec <- age - mean(age)
mod <- logit(a) + logit(.5)*agec # linear model
probs <- 1/(1 + exp(-mod)) # pass through an inverse-logit function
listed <- rbinom(n, 1, probs) # binomially distributed presence
(m2 <- glm(listed ~ agec, family = "binomial"))
logistic(coef(m2))

## Logistic with continuous predictor with rethinking (not in text)
n <- 100
age <- rnorm(n, 30, 5)
agec <- age - mean(age)
mod <- 10 + 5*agec # linear model
probs <- 1/(1 + exp(-mod)) # pass through an inverse-logit function
listed <- rbinom(100, 1, probs) # binomially distributed presence

df <- data.frame(listed = listed, agec = agec)
(m3f <- glm(listed ~ agec, data = df, family = "binomial"))

m3 <- quap(
  alist(
    listed ~ dbinom(1, p),
    logit(p) <- a + bagec*agec,
    a ~ dnorm(10 , 1),
    bagec ~ dnorm(0, 1)
  ), data = df)

precis(m3, prob = 0.95)
plot(precis(m3, prob = 0.95))
xtable(precis(m3, prob = 0.95))

newdat <- data.frame(agec = seq(min(agec), max(agec), length = 1000))
newdat$listed <- predict(m3f, newdata = newdat, type = "response")

par(mfrow = c(1, 2))
par(mar = c(3, 3, 1, 1))
plot(probs ~ mod, pch = 16)
text(86, 0.01, "(a)")
plot(listed ~ agec, data = df, 
     pch = 16, ylab = "Pr(Listing)")
lines(listed ~ agec, newdat, 
      col = "darkgray", lwd = 1,
      xlab = "age (centered at mean)")
text(15.5, 0.01, "(b)")

### Table 6.1 and Figure 6.2: Predicting listing "greed"
library(rethinking)
d <- read.delim("Greed_Experiment1.txt") # predicting "greed"

b1 <- quap(
  alist(
    listed ~ dbinom(1, p),
    logit(p) <- a + btreatment*treatment,
    a ~ dnorm(0 , 1),
    btreatment ~ dnorm(0, 1)
  ), data = d)
precis(b1, prob = .95)
plot(precis(b1, prob = .95))

# Ordered categorical
### Figure 6.3: Reanalysis of Irish data
Data <- read.delim("Pooled_Ireland.txt")
labs <- c("Subj", "Order", "Code", "God_Always_Never")
cathsub <- Data[labs]
cathcomp <- subset(cathsub, God_Always_Never <=2 & Code %in% c("Abuse/Paedophilia"))

pr_k <- table(cathcomp$Order)/nrow(cathcomp)
cum_pr_k <- cumsum(pr_k)
plot(1:18, cum_pr_k, type = "b", xlab = "Order", 
     pch = 16, ylab = "Cum. Prop.", ylim = c(0, 1))

barplot(t(prop.table(table(cathcomp$Order, cathcomp$God_Always_Never))), beside = T, ylim = c(0, .2))
box()

dat <- list(
  order = cathcomp$Order,
  apostate = cathcomp$God_Always_Never - 1)

mc2 <- ulam(
  alist(
    order ~ dordlogit(phi, kappas),
    phi <- bapostate * apostate,
    bapostate ~ dnorm(0, 1.5),
    kappas ~ dnorm(0, 1.5)
  ), data = dat, chains = 4, cores = 4)

precis(mc2, prob = 0.95, depth = 2)
round(inv_logit(coef(mc2)), 2)

plot(NULL, type = "n", xlab = "apostate", ylab = "probability",
     xlim = c(0, 1), ylim = c(0, 1), xaxp = c(0, 1, 1), yaxp = c(0, 1, 2))
nA <- 0:1
pdat <- data.frame(apostate = nA)
mc2sim <- sim(mc2, data = pdat)

xxx <- data.frame(table(mc2sim[,1]), table(mc2sim[,2]))
xxx$Var1.1 <- NULL
row.names(xxx) <- xxx$Var1
xxx$Var1 <- NULL

par(mar = c(3, 3, 1, 1), fig = c(0, 1, 0, 1))
barplot(t(xxx), ylim = c(0, 500), 
        beside = T, xlab = "Order of Simulated Items", ylab = "Frequency")
box()
u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc"))
v <- c((v[1] + v[2])/2.5, v[2], (v[3] + v[4])/2.5, v[4])
par(fig = v, new = TRUE, mar = c(4, 4, 1, 1))
plot(1:18, cum_pr_k, type = "b", xlab = "Order of Actual Items", 
     pch = 16, ylab = "Cum. Prop.", ylim = c(0, 1), cex = .7)

# Beta distribution
mycol1 <- rgb(224, 224, 224, max=255, alpha = 100, names = "lightgray") 
mycol2 <- rgb(255, 255, 255, max = 255, alpha = 100, names = "white")
mycol3 <- rgb(0, 0, 0, max = 255, alpha = 100, names = "darkgray")

n <- 1000
p <- seq(0, 1, length = n)

# This plots probability curves (not in text)
par(mar = c(4, 4, 1, 1))
plot(NA, xlim = c(-.1, 1.1), ylim = c(0, 4.5),
     xlab = "Salience", ylab = "Density")
polygon(p, dbeta(p, 2, 10), ylab=NA, col = mycol1)
polygon(p, dbeta(p, 2, 2), col = mycol2) 
polygon(p, dbeta(p, 5, 2), col= mycol3)
legend(.4, 4, c('Beta(2, 10)','Beta(2, 2)','Beta(5, 2)'),
       fill = c(mycol1, mycol2, mycol3),
       cex = .7)

### R Code Box 6.2: Simulating and plotting beta-distribution
### Figure 6.4: Simulated Beta data (ha!)
par(mar = c(4, 4, 1, 1))
plot(NA, xlim = c(-.1, 1.1), ylim = c(0, 4.5), 
     xlab = "Salience",
     ylab = "Density")
polygon(density(rbeta(p, 2, 10, 1)), col = mycol1)
polygon(density(rbeta(p, 5, 5, 1)), col = mycol2)
polygon(density(rbeta(p, 10, 2, 1)), col = mycol3)
legend(.25, 4.2, legend = c("Beta(2, 10)", "Beta(5, 5)", "Beta(10, 2)"),
       fill = c(mycol1, mycol2, mycol3),
       cex = .75)

# Even more (not in text)
plot(NA, xlim = c(-0.1, 1.1), ylim = c(0, 8),
     xlab = "Salience", 
     ylab = "Density")
lines(density(rbeta(p, 1, 1, 1)), lty = 1)
lines(density(rbeta(p, 5, 1, 1)), lty = 2)
lines(density(rbeta(p, 10, 1, 1)), lty = 3)
lines(density(rbeta(p, 1, 5, 1)), lty = 4)
lines(density(rbeta(p, 1, 10, 1)), lty = 5)
lines(density(rbeta(p, 10, 10, 1)), lty = 6)
legend(.4, 8, legend = c("(1, 1)", "(5, 1)", "(10, 1)",
                         "(1, 5)", "(1, 10)", "(10, 10)"),
       lty = c(1, 2, 3, 4, 5, 6),
       cex = .5)

### R Code Box 6.3: Simulating ZOIB-distributed data
# Function for simulating zoib
rzoib <- function(n, theta, gmma, mu, phi) { 
  a <- mu * phi
  b <- (1 - mu) * phi
  y <- vector("numeric", n)
  y <- ifelse(
    rbinom(n, 1, theta), 
    rbinom(n, 1, gmma), 
    rbeta(n, a, b)
  )
  y
}

set.seed(666)

n <- 500
b <- 0.20 # set effect being in group 1
x <- rbinom(n, 1, .5) # 50% of n in two groups
theta <-  0.5 # zero-one
gmma <- 0.65 + b * x # conditional-one
mu <- 0.25 + b * x # 
phi <- 5
dat <- data.frame(x = x, theta = theta, gmma = gmma, mu = mu, phi = phi)
dat$y <- rzoib(n, dat$theta, dat$gmma, dat$mu, dat$phi)
sim_dat <- dat[c("x", "y")]
sim_dat$x <- ifelse(sim_dat$x==0, "control", "treatment")
sim_dat$x <- as.factor(sim_dat$x)

sim_mod <- brm(formula = bf(
  y ~ 1 + x,
  phi ~ 1 + x,
  theta ~ 1 + x,
  gmma ~ 1 + x),
  data = sim_dat,
  seed = 666,
  family = zero_one_inflated_beta())

### Table 6.2: Output for analysis of simulated ZOIB-distributed data
sim_mod_summary <- as_draws_df(sim_mod, pars = c("b_")) %>%
  mutate_at(c("b_phi_Intercept", "b_phi_xtreatment"), exp) %>% 
  mutate_at(vars(-"b_phi_Intercept", "b_phi_xtreatment"), plogis) %>% 
  posterior_summary() %>% 
  as.data.frame() %>% 
  rownames_to_column("Parameter")
sim_mod_summary <- cbind(Parameter = sim_mod_summary[,1], round(sim_mod_summary[,2:5], 2))
sim_mod_summary

### Figure 6.5: Simulated raw data for ZOIB
set.seed(666)
sim_ce <- conditional_effects(sim_mod)
sim_est <- data.frame(x = c(1, 2), 
                      median = c(sim_ce$x[["estimate__"]][1], sim_ce$x[["estimate__"]][2]),
                      lower = c(sim_ce$x[["lower__"]][1], sim_ce$x[["lower__"]][2]),
                      upper = c(sim_ce$x[["upper__"]][1], sim_ce$x[["upper__"]][2]))

sim_p <- ggplot(sim_dat, aes(x = x, y = y, color = as.factor(x))) +
  geom_jitter(alpha = 0.3, position = position_jitter(height = 0.01, width = 0.1)) +
  theme_classic() +
  theme(legend.position = "none") + 
  geom_pointrange(data = sim_est, 
                  aes(x = x, y = median, ymin = lower, ymax = upper), color = "black", size = 0.25) +
  geom_errorbar(data = sim_est, 
                aes(x = x, y = median, ymin = lower, ymax = upper, width = 0.25), color = "black", size = 0.5) +
  scale_color_manual(values = c("black", "darkgray")) + # col for control
  scale_fill_manual(values = c("black", "darkgray")) + # col for treat
  xlab("Condition") + ylab("Salience")
sim_p

ggMarginal(sim_p, groupColour = TRUE, groupFill = TRUE, type = "density", margins = "y")

### Table 6.3 and Figure 6.6: Varying intercepts model
d2 <- read.delim("Greed_Experiment2.txt") # predicting "greed set"
dat <- list(
  listed = d2$listed,
  treatment = d2$treatment,
  partid = d2$partid
)

b2 <- ulam(
  alist(
    listed ~ dbinom(1, p),
    logit(p) <- a[partid] + btreatment*treatment,
    btreatment ~ dnorm(0, 1),
    a[partid] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), 
  data = dat, chains = 4, cores = 4, log_lik = TRUE)

precis(b2, prob = .95)
plot(precis(b2, prob = .95))
logistic(-1.18)
logistic(-1.18 + 0.12)

## Figure 6.8: Free-list length as a predictor of nominations
rm(list = ls())
load("FishingAbilityWorkspace.RData")

mycol3 <- rgb(0, 0, 0, max = 255, alpha = 100, names = "darkgray")
par(mfrow = c(1, 3), mar = c(4, 3.5, 1, 1))

# masks
plot(mask.summary$free.listed.fish, mask.summary$y, 
     ylim = c(0, 1), xlim = c (8, 27), pch = 16,
     ylab = NA, xlab = NA)
xaxseq <- seq(min(mask.summary$free.listed.fish), max(mask.summary$free.listed.fish), length.out = 1000)
polygon(c(xaxseq, rev(xaxseq)), 
        c(pred.endorse.mask.list$y$mu.ci[1,], rev(pred.endorse.mask.list$y$mu.ci[2,])),
        col = mycol3, border = NA)
lines(mask.list.pred$free.listed.fish, pred.endorse.mask.list$y$mu)
mtext("Free-list length", side = 1, padj = 3.3, cex = .8)
mtext("Mask endorsements", side = 2, padj = -3.3, cex = .8)
text(10, 0.9, "(a)")

# bows
plot(bows.summary$y ~ bows.summary$free.listed.fish, 
     ylim = c(0, 1), xlim = c (8, 27), pch = 16,
     ylab = NA, xlab = NA,
     cex = .8)
xaxseq <- seq(min(bows.summary$free.listed.fish), max(bows.summary$free.listed.fish), length.out = 1000)
polygon(c(xaxseq, rev(xaxseq)), 
        c(pred.endorse.bows.list$y$mu.ci[1,], rev(pred.endorse.bows.list$y$mu.ci[2,])),
        col = mycol3, border = NA)
lines(bows.list.pred$free.listed.fish, pred.endorse.bows.list$y$mu)
mtext("Free-list length", side = 1, padj = 3.3, cex = .8)
mtext("Bow endorsements", side = 2, padj = -3.3, cex = .8)
text(10, 0.9, "(b)")

# hooks
plot(hooks.summary$y ~ hooks.summary$free.listed.fish, 
     ylim = c(0, 1), xlim = c (8, 27), pch = 16,
     ylab = NA, xlab = NA)
xaxseq <- seq(min(hooks.summary$free.listed.fish), max(hooks.summary$free.listed.fish), length.out = 1000)
polygon(c(xaxseq, rev(xaxseq)), 
        c(pred.endorse.hook.list$y$mu.ci[1,], rev(pred.endorse.hook.list$y$mu.ci[2,])),
        col = mycol3, border = NA)
lines(hook.list.pred$free.listed.fish, pred.endorse.hook.list$y$mu)
mtext("Free-list length", side = 1, padj = 3.3, cex = .8)
mtext("Hook endorsements", side = 2, padj = -3.3, cex = .8)
text(10, 0.9, "(c)")

## Moral models
rm(list = ls())
set.seed(7)
library(rethinking)
load("MoralModelsWorkspace.RData")

## Model code is here for inspection, but see 
## Purzycki, Pisor, et al. "The cognitive and cultural foundations of moral behavior"
## for deeper look.

m1 <- map2stan(
  alist(
    ## coin model
    y ~ dbinom(30, p),
    logit(p) <- a + zi[id] * sigma_id + aj[group] + # z standardizes adaptive prior for varying effects on individuals
      (bH+bHj[group]) * h + bHavg * inv_logit(Havg[group]) +
      (bpun+bpunj[group]) * pun + bpunavg * Pavg[group] + 
      (bomni+bomnij[group]) * omni + bomniavg * Oavg[group] +
      bkids * kids + btreat * treat + 
      border * order + bgmcheck * gmcheck + bgame * game,
    ## honesty model
    h ~ dbinom(2, ph),
    logit(ph) <- Havg[group],
    Havg[group] ~ normal(Mu_Havg, sigmaHavg),
    Mu_Havg ~ normal(0, 5),
    sigmaHavg ~ exponential(1),
    ## pun model
    pun ~ normal(pun_mu, pun_sd),
    pun_mu <- Pavg[group],
    Pavg[group] ~ normal(Mu_Pavg, sigmaPavg),
    Mu_Pavg ~ normal(0.5, 1) & T[0, 1],
    sigmaPavg ~ exponential(1),
    ## omni model
    omni ~ normal(omni_mu, omni_sd),
    omni_mu <- Oavg[group],
    Oavg[group] ~ normal(Mu_Oavg, sigmaOavg),
    Mu_Oavg ~ normal(0.5, 1) & T[0, 1],
    sigmaOavg ~ exponential(1),
    ## priors
    a ~ normal(0, 10),
    bH ~ normal(0, 1),
    c(bpun, bomni, bkids, btreat, border, bgmcheck, bgame, bpunavg, bomniavg) ~ normal(0, 1),
    bHavg ~ normal(0, 1),
    ## varying intercepts and slopes for groups and h (individual response) on y
    c(aj, bHj, bpunj, bomnij)[group] ~ dmvnormNC(Sigmaj, Rhoj),
    Sigmaj ~ dexp(1),
    Rhoj ~ dlkjcorr(4),
    ## individual varying intercepts (residuals for binomial overdispersion)
    zi[id] ~ normal(0, 1),
    sigma_id ~ exponential(1),
    ## imputation distributions below
    kids ~ normal(kids_mu, kids_sd), # >=0 constraint imposed later
    order ~ bernoulli(0.5),
    gmcheck ~ bernoulli(phi_gmcheck),
    phi_gmcheck ~ beta(1, 1),
    kids_mu ~ normal(1, 1) & T[0, ],
    kids_sd ~ exponential(10),
    pun_sd ~ exponential(1),
    omni_sd ~ exponential(1)
  ),
  start = list(Havg=rep(0, 8)),
  constraints = list(
    sigma_id = "lower = 0",
    phi_gmcheck = "lower = 0, upper = 1",
    kids_impute = "lower = 0",
    pun_impute = "lower = 0, upper = 1",
    omni_impute="lower = 0, upper = 1"
  ),
  data = dat_list , 
  sample = TRUE, control = list(adapt_delta = 0.99, max_treedepth = 13), 
  chains = 3, cores = 3, iter = 1000, warmup = 500, WAIC = FALSE, DIC = TRUE)

post <- extract.samples(m1)

## Can run this to examine specific results
table1 <- precis(m1, prob = 0.95, pars = c("a", "bH", "bpun", "bomni", "bkids", 
                                           "btreat", "border", "bgmcheck", "bgame",
                                           "bpunavg", "bomniavg", "bHavg"), digits = 2)
#table1 <- precis(m1, prob = 0.95)
lemontwigs <- data.frame(cbind(table1$mean, table1$`2.5%`, table1$`97.5%`))
collabels <- c("OR", "Lower", "Upper")
colnames(lemontwigs) <- collabels
rowlabels <- row.names(table1)
rownames(lemontwigs) <- rowlabels
exp(lemontwigs)

### Figure 6.9: Individual and group-level prevalence of free-listing (dis)honesty
punavgseq <- seq(from = 0, to = 1, length.out = 10)

p_pred <- sapply(punavgseq, function(x) 
  with(post, {
    inv_logit(a + bkids * 0 + 
                bH * 0.5 + bHavg * x +  # set all other effects at mid-scale
                bpun * 0.5 + bpunavg * 0.5 + 
                bomni * 0.5 + bomniavg * 0.5)
  }))

p_hon_avg <- apply(p_pred, 2, mean)
p_hon_PI <- apply(p_pred, 2, PI, prob = 0.95)

par(mfrow = c(1, 2), mar = (c(4.5, 4, 1, 1)))
plot(precis(m1, prob = .95, pars = c("a", "bkids", "bH", "bpun", "bomni")), xlab = "estimates (individual)")
plot(punavgseq, p_hon_avg, type = "l", ylim = c(0, 1), 
     ylab = "Prob. of Coin to Distant", 
     xlab = "prevalence of 'honesty'", col = "black", cex.lab = 1)
shade(p_hon_PI, punavgseq, col = col.alpha("black", 0.2))
abline(h = 0.5, lty = 2, lwd = 0.5)

                 
########################################################################

# If using the data, please read, refer, and cite the following:

# Bendixen, T., & Purzycki, B. G. (2023). Cognitive and cultural models in psychological science: A tutorial on modeling free-list data as a dependent variable in Bayesian regression. Psychological Methods.
# Koster, J., Bruno, O., & Burns, J. L. (2016). Wisdom of the elders? Ethnobiological knowledge across the lifespan. Current Anthropology, 57(1), 113-121.
# Purzycki, B. G., Pisor, A. C., Apicella, C., Atkinson, Q., Cohen, E., Henrich, J., ... & Xygalatas, D. (2018). The cognitive and cultural foundations of moral behavior. Evolution and Human Behavior, 39(5), 490-501.
# Purzycki, B. G., Stagnaro, M. N., & Sasaki, J. (2020). Breaches of trust change the content and structure of religious appeals. Journal for the Study of Religion, Nature and Culture, 14(1), 71-94.
# Turpin, H. (2022). Unholy Catholic Ireland: Religious hypocrisy, secular morality, and Irish irreligion. Stanford University Press.
