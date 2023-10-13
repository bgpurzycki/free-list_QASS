##############################################
### Ethnographic Free-List Data
### Chapter 2: Content Analysis
### Benjamin Grant Purzycki
### Last Updated: September 29, 2023
##############################################

### Preliminaries
setwd()

library(AnthroTools)
library(xtable)

### R Code Box 2.1: Frequency distribution plot (Figure 2.1)
FL10 <- read.delim("Tyva Republic_Virtues_2010.txt") # Pull up Tyvan Virtue/Morality data

TV.bin <- FreeListTable(FL10, CODE = "GC", Order = "Order", # create presence set
                        Subj = "Subj", tableType = "PRESENCE")
n <- length(unique(TV.bin$Subject)) # sample size
SUM <- colSums(TV.bin[,2:52]) # sum of each item listed; adjust the 52 accordingly! ncols would give a more general option, but then graphs might wind up being VERY wide...like this particular comment...but worse...
PROP <- SUM/n
FREQ <- data.frame(SUM, PROP) # turn vector into data.frame
newdata <- FREQ[order(-FREQ$SUM),, drop = F] # sort

par(mar = c(8, 2, 1, 0)) # margins for plot
barplot(newdata$SUM, names.arg = rownames(newdata), las = 2)
#barplot(newdata$SUM/n, names.arg = rownames(newdata), las = 2,
#        ylim = c(0, 0.5)) 

#############################################################
### Plots of list length distributions

lengths <- rowSums(TV.bin[,2:52])
lengthset <- data.frame(lengths = lengths, PARTID = TV.bin$Subject)
lengthset <- lengthset[order(-lengthset$lengths),, drop = F]
par(mar = c(4, 2, 1, 0)) # margins for plot
barplot(lengthset$lengths, names.arg = lengthset$PARTID, las = 2,
        cex.names = .7) # find the most knowledgeable

plot(NA, xlim = c(-5, 15), ylim = c(0, .4))
polygon(density(lengthset$lengths), col = "gray") # shape of distribution

#abline(v = mean(lengthset$lengths))
#############################################################

### Figure 2.2: Correlation between # listed and avg. s_i
# Set up
FL.G <- CalculateSalience(FL10, Subj = "Subj", # item salience
                          Order = "Order", 
                          CODE = "GC",
                          Salience = "GC.S")

labs <- c("Subj", "Order", "GC", "GC.S")
FL.G <- FL.G[labs]
FL.G <- FL.G[complete.cases(FL.G), ]
TV.max <- FreeListTable(FL.G, CODE = "GC", Order = "Order",
                        Subj = "Subj", Salience = "GC.S", tableType = "MAX_SALIENCE")
TV.max$Subject <- NULL
TV.avg <- colMeans(TV.max)

# Plot
par(mar = c(4.2, 4.2, 1, 1))
plot(TV.avg ~ SUM, xlab = "Individuals listing items", 
     ylab = expression("Average item salience ("*italic(s[i])*")"), #expression(italic('x'['i'])*' =')
     pch = 16)
abline(lm(TV.avg ~ SUM))

### R Code Box 2.2: Item Salience
FL.G <- CalculateSalience(FL10, Subj = "Subj", # item salience
                          Order = "Order", 
                          CODE = "GC",
                          Salience = "GC.S")

### R Code Box 2.3: Cultural Salience
GFL.S <- SalienceByCode(FL.G, Subj = "Subj", # cultural salience
                        CODE = "GC", Salience = "GC.S",
                        dealWithDoubles = "MAX")
FlowerPlot(GFL.S, "Good") # make a flower plot

### Figure 2.3: Hard-coding flower plots
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))

#CODE <- c("worms", "fever", "cold", "inflam.", "stomach", "cough", "pain", "gas")
#SmithsS <- c(.52, .48, .46, .43, .40, .33, .32, .31)
#bwamawego <- data.frame(CODE, SmithsS)
#bwamawego$SmithsS <- as.numeric(bwamawego$SmithsS)

circle <- function(xorig, yorig, radius, add, ...){ 
  x <- seq(-radius, radius, length.out = 1000)
  y <- sapply(x, function(z) sqrt(radius^2 - z^2))
  if(add == TRUE){
    lines(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
          type = "l", ...)
  } else {
    plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
         type = "l", ...)
  }
}

## Panel 2.3a
plot(c(-110, 110), c(-110, 110), type = "n", 
     xlab = "", ylab = "", axes = FALSE, asp = 1, 
     family = "Calibri") 

rad <- 30 # predefined radius

circle(0, 0, rad, add = TRUE, col = "black", lwd = 5.23) # domain circle

circle(0, 80, rad, add = TRUE, col = "black", lwd = 5.23) # top circle
circle(60, 60, rad, add = TRUE, col = "black", lwd = 4.76) # top-right
circle(80, 0, rad, add = TRUE, col = "black", lwd = 4.57) # right circle
circle(60, -60, rad, add = TRUE, col = "black", lwd = 4.28) # lower right
circle(0, -80, rad, add = TRUE, col = "black", lwd = 3.97) # bottom circle
circle(-60, -60, rad, add = TRUE, col = "black", lwd = 3.26) # lower left
circle(-80, 0, rad, add = TRUE, col = "black", lwd = 3.24) # left circle
circle(-60, 60, rad, add = TRUE, col = "black",  lwd = 3.09) # top-left

## Connections!
notch <- 50 # length for vertical and horizontal lines
nitch <- 21.5 # length for diagonals
natch <- 38.5 # length for diagonals

segments(0, rad, 0, notch, lwd = 5.23) # top
segments(nitch, nitch, natch, natch, lwd = 4.76) # top-right
segments(rad, 0, notch, 0, lwd = 4.57) # right 
segments(nitch, -nitch, natch, -natch, lwd = 4.28) # lower right
segments(0, -rad, 0, -notch, lwd = 3.97) # bottom
segments(-nitch, -nitch, -natch, -natch, lwd = 3.26) # lower left
segments(-rad, 0, -notch, 0, lwd = 3.24) # left
segments(-nitch, nitch, -natch, natch, lwd = 3.09) # top-left

## Labels!
text(0, 0, labels = "Illness", font = 2) # center
text(0, 80, labels = "worms", font = 2) # top
text(60, 60, labels = "fever", font = 2) #top-right
text(80, 0, labels = "colds", font = 2) # right
text(60, -60, labels = "inflam.", font = 2) # lower right
text(0, -80, labels = "stomach", font = 2) # bottom
text(-60, -60, labels = "cough", font = 2) # lower left
text(-80, 0, labels = "pain", font = 2) # left
text(-60, 60, labels = "gas", font = 2) # top-left

text(10, rad+10, labels = "0.52", font = 2) #top
text(37, 25, labels = "0.48", font = 2) # top-right
text(rad+10, -5, labels = "0.46", font = 2) # right
text(25, -35, labels = "0.43", font = 2) # lower right
text(-10, -rad-10, labels = "0.40", font = 2) # bottom
text(-35, -25, labels = "0.33", font = 2) # lower left
text(-rad-10, 5, labels = "0.46", font = 2) # left
text(-24, 35, labels = "0.31", font = 2) # top-left
text(-100, 100, "(a)")

## Panel 2.3b
plot(c(-110, 110), c(-110, 110), type = "n", 
     xlab = "", ylab = "", axes = FALSE, asp = 1, 
     family = "Calibri") 

rad <- 30 # predefined radius

circle(0, 0, rad, add = TRUE, col = "black", lwd = 2.3) # domain circle

circle(0, 80, rad, add = TRUE, col = "black", lwd = 2.3) # top circle
circle(60, 60, rad, add = TRUE, col = "black", lwd = 2.3) # top-right
circle(80, 0, rad, add = TRUE, col = "black", lwd = 2.2) # right circle
circle(60, -60, rad, add = TRUE, col = "black", lwd = 1.2) # lower right
circle(0, -80, rad, add = TRUE, col = "black", lwd = 1.1) # bottom circle
circle(-60, -60, rad, add = TRUE, col = "black", lty = 2) # lower left
circle(-80, 0, rad, add = TRUE, col = "black", lty = 2) # left circle
#circle(-60, 60, rad, add = TRUE, col = "black",  lwd = 3.09) # top-left

## Connections!
notch <- 50 # length for vertical and horizontal lines
nitch <- 21.5 # length for diagonals
natch <- 38.5 # length for diagonals

segments(0, rad, 0, notch, lwd = 2.3) # top
segments(nitch, nitch, natch, natch, lwd = 2.3) # top-right
segments(rad, 0, notch, 0, lwd = 2.2) # right 
segments(nitch, -nitch, natch, -natch, lwd = 1.2) # lower right
segments(0, -rad, 0, -notch, lwd = 1.1) # bottom
segments(-nitch, -nitch, -natch, -natch, lty = 2) # lower left
segments(-rad, 0, -notch, 0, lty = 2) # left
#segments(-nitch, nitch, -natch, natch, lwd = 3.09) # top-left

## Labels!
text(0, 0, labels = "Reincarnate", font = 2) # center
text(0, 80, labels = "murder", font = 2) # top
text(60, 60, labels = "harm", font = 2) #top-right
text(80, 0, labels = "bad", font = 2) # right
text(60, -60, labels = "illegal", font = 2) # lower right
text(0, -80, labels = "theft", font = 2) # bottom
text(-60, -60, labels = "bad thoughts", font = 2) # lower left
text(-80, 0, labels = "<0.10", font = 2) # left
#text(-60, 60, labels = "gas", font = 2) # top-left

text(10, rad+10, labels = "0.23", font = 2) #top
text(37, 25, labels = "0.23", font = 2) # top-right
text(rad + 10, -5, labels = "0.22", font = 2) # right
text(25, -35, labels = "0.12", font = 2) # lower right
text(-10, -rad - 10, labels = "0.11", font = 2) # bottom
text(-35, -25, labels = "0.10", font = 2) # lower left
text(-100, 100, "(b)")

### Figure 2.4 Coffee example
rowlabs <- c("color intensity", "brown color", "sweet flavor", "caramel flavor", "watery")
collabs <- c("P1", "P2", "P3", "P4", "P5", "P6")
colint <- c(.22, .11, .17, .15, .1, .12)
brncol <- c(.18, .20, .2, .1, .15, .12)
swtfla <- c(.32, .30, .30, .34, .53, .36)
carfla <- c(.03, .03, .03, .03, .12, .02)
watery <- c(.23, .22, .22, .01, .03, .05)
d <- data.frame(t(data.frame(colint, brncol, swtfla, carfla, watery, row.names = collabs)))

par(mar = c(5.5, 4, 1, 1))
plot(d$P1, ylim = c(0, .6), xaxt = "n",
     xlab = NA, ylab = expression("Smith's "*italic(S)), lwd = 2, type = "b", pch = 16)
lines(d$P2, lty = 2, lwd = 2, type = "b", pch = 16)
lines(d$P3, lty = 3, lwd = 2, type = "b", pch = 16)
lines(d$P4, lty = 4, lwd = 2, type = "b", pch = 16)
lines(d$P5, lty = 5, lwd = 2, type = "b", pch = 16)
lines(d$P6, lty = 6, lwd = 2, type = "b", pch = 16)
axis(1, at = 1:5, labels = FALSE)
text(1:5-.3, par("usr")[3] - 0.05, labels = rowlabs, 
     srt = 45, xpd = NA, adj = c(.55, 2))
legend(4, .6, legend = c("S1", "S2", "S3", "S4", "S5", "S6"), lty = c(1:6), lwd = 2,
       title = "Sample")

### Figure 2.5: Item and cultural salience across list lengths and sample sizes
si <- function(n, k){
  si <- (n + 1 - k)/n
  return(si)
}

si(n = c(1:100), k = c(1:100))

SmithsS <- function(sis, N){
  S <- sum(sis)/N
  return(S)
} 

si1 <- si(n = c(1:100), k = c(1:100))
S1 <- SmithsS(1, c(1:100))

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(si1, type = "l",
     ylab = expression("item salience ("*italic(s[i])*")"*" of last item listed"), 
     xlab = expression("list length ("*italic(n)*")"),
     lwd = 1.5)
plot(S1, type = "l",
     ylab = expression("Smith's "*italic(S)*" of last item listed"), 
     xlab = expression("sample size ("*italic(N)*")"),
     lwd = 1.5)
lines(SmithsS(5, c(5:100)), lty = 2, lwd = 1.5)
lines(SmithsS(10, c(10:100)), lty = 3, lwd = 1.5)
lines(SmithsS(20, c(20:100)), lty = 4, lwd = 1.5)
lines(SmithsS(50, c(50:100)), lty = 5, lwd = 1.5)
legend(60, 1, c("= 1", "= 5", "= 10", "= 20", "= 50"), lty = c(1:5),
       title = expression("Sum of "*italic(s[i])), cex = .8, lwd = 1.5)

## Salience in Sutrop (2001)
#L: # times item in all lists; N: # participants: mp: mean position
sutrop <- function(L, N, mP){ 
  S = L/(N * mP)
  return(S)
}

sutrop(1, 100, 1)
sutrop(2, 100, 4)
sutrop(25, 100, 18)

## Salience in Robbins, Nolan, & Chen (2017)
data(FruitList) # from AnthroTools
test <- FruitList
test <- CalculateSalience(test, Rescale = F)
SBC <- SalienceByCode(test, dealWithDoubles = "MAX")

ntab <- as.data.frame(SBC$SumSalience/SBC$SmithsS)
n <- ntab[1,1] # sample size

SBC$L <- SBC$SumSalience/SBC$MeanSalience # times someone lists something
SBC$B <- (SBC$SumSalience + SBC$L - 1)/(2*n - 1) # Robbins, Nolan, Chen
SBC <- SBC[order(-SBC$SmithsS),, drop = F] # sort

plot(SBC$B ~ SBC$SmithsS, pch = 16,
     xlim = c(0, 1), ylim = c(0, 1))
flm <- lm(SBC$B ~ SBC$SmithsS)
abline(lm(flm))

## Real Example
FL10 <- read.delim("Tyva Republic_Virtues_2010.txt") # Pull up Tyvan Virtue/Morality data
FL.G <- CalculateSalience(FL10, Subj = "Subj", # item salience
                          Order = "Order", 
                          CODE = "GC",
                          Salience = "GC.S")
GFL.S <- SalienceByCode(FL.G, Subj = "Subj", # cultural salience
                        CODE = "GC", Salience = "GC.S",
                        dealWithDoubles = "MAX")
ntab <- as.data.frame(GFL.S$SumSalience/GFL.S$SmithsS)
n <- ntab[1,1] # sample size

GFL.S$L <- GFL.S$SumSalience/GFL.S$MeanSalience # times someone lists something
GFL.S$B <- (GFL.S$SumSalience + GFL.S$L - 1)/(2*n - 1) # Robbins, Nolan, Chen
GFL <- GFL.S[order(-GFL.S$SmithsS),, drop = F] # sort
colorder <- c("CODE", "L", "MeanSalience", "SumSalience",
              "SmithsS", "B")
GFL <- GFL[, colorder]

xtable(GFL) ### Table 2.2 (Full Version) 
par(mar = c(4, 4, 1, 1))
plot(B ~ SmithsS, data = GFL, pch = 16, xlab = "Smith's S", ylab = "S'")
mB <- lm(B ~ SmithsS, data = GFL)
newx <- seq(min(GFL$SmithsS), max(GFL$SmithsS), length.out = 100)
preds <- predict(mB, newdata = data.frame(SmithsS = newx), 
                 interval = 'confidence')
polygon(c(rev(newx), newx), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) 
abline(mB)

## Categorical bias from Robbins and Nolan (1997)
# L: # of referenced category of listed items (A)
# D: # of referenced category of listed items (B) 
# sumL: sum of order values of L
# sumD: sum of order values of D

DCB <- function(L, D, sumL){ # simple direct function
  B <- (L*(L + 2*D + 1) - 2*sumL)/(2*L*D)
  return(B)
}

DCB(4, 2, 14) # from article
DCB(10, 0, 10)
DCB(2, 1, 3) # participant EX001
DCB(2, 2, 6) # participant EX002
DCB(1, 2, 3) # participant EX003
DCB(2, 4, 7) # participant EX004

# Simulate B values for use in simple regression
n <- 100
bvalues <- rnorm(n, .1, 0.5)
hist(bvalues, xlab = "B", main = NA)
m1 <- lm(bvalues ~ 1)
confint(m1)

# Example
data(FruitList)
FLbin <- FreeListTable(FruitList, CODE = "CODE", Order = "Order", 
                       Subj = "Subj", tableType = "PRESENCE")
tFLbin <- t(FLbin) # transpose
tFLbin <- data.frame(tFLbin[-1,])
FLrank <- FreeListTable(FruitList, CODE = "CODE", Order = "Order", 
                       Subj = "Subj", tableType = "HIGHEST_RANK")
tFLrank <- t(FLrank) # transpose
tFLrank <- data.frame(tFLrank[-1,])

### R Code Box 2.4: Cronbach's alpha and intervals
alpha.fun <- function(data, interval){
  k <- ncol(data)
  n <- nrow(data)
  p <- 1 - interval
  cormat <- cor(data)[lower.tri(cor(data))]
  rhat <- mean(cormat)
  alpha <- (k*rhat)/(1 + (k - 1)*rhat)
  df1 <- n - 1
  df2 <- df1*(k - 1)
  ffl <- qf(p/2, df1 = df1, df2 = df2)
  ffu <- qf(1 - (p/2), df1 = df1, df2 = df2)
  upper <- 1 - (1 - alpha)*ffl
  lower <- 1 - (1 - alpha)*ffu
  return(data.frame(alpha = alpha, lower = lower, upper = upper))
}

listed1 <- c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0)
listed2 <- c(0, 0, 1, 1, 0, 1, 1, 1, 0, 0)
listed3 <- c(0, 1, 1, 1, 0, 0, 1, 1, 0, 0)
listed4 <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0)
listed5 <- c(0, 0, 1, 1, 0, 1, 1, 1, 0, 0)

d <- data.frame(listed1, listed2, listed3, listed4, listed5)

alpha.fun(d, 0.95)

random1 <- rbinom(10, 1, 0.5)
random2 <- rbinom(10, 1, 0.5)
random3 <- rbinom(10, 1, 0.5)
random4 <- rbinom(10, 1, 0.5)
random5 <- rbinom(10, 1, 0.5)
dran <- data.frame(random1, random2, random3, random4, random5)

alpha.fun(dran, 0.95)

# make table for LaTeX
library(xtable)
crontab <- data.frame(d, dran)
xtable(crontab, digits = 0)
