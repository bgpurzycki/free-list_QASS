##############################################
### Ethnographic Free-List Data
### Chapter 2: Content Analysis
### Benjamin Grant Purzycki
### Last Updated: May 18, 2023
##############################################

##############################################
### Preliminaries

setwd()
setwd("C:/Users/au624473/Dropbox/2. In Progress/Articles and Books/Books/Methods with Free-List Data/3. Workflow/Data")

library(AnthroTools)
library(xtable)

##############################################
### R Code Box 2.1: Frequency distribution plot

FL10 <- read.delim("Tyva Republic_Virtues_2010.txt") # Pull up Tyvan Virtue/Morality data

TV.bin <- FreeListTable(FL10, CODE = "GC", Order = "Order", # create presence set
                        Subj = "Subj", tableType = "PRESENCE")
n <- length(unique(TV.bin$Subject)) # sample size
SUM <- colSums(TV.bin[,2:52]) # sum of each item listed; adjust the 52 accordingly! ncols would give a more general option, but then graphs might wind up being VERY wide...like this particular comment...but worse...
PROP <- SUM/n
FREQ <- data.frame(SUM, PROP) # turn vector into data.frame
newdata <- FREQ[order(-FREQ$SUM),, drop = F] # sort

par(mar = c(8, 2, 1, 0)) # margins for plot
barplot(newdata$SUM, names.arg = rownames(newdata), las = 3)

par(mar = c(9, 3, 1, 0))
barplot(newdata$SUM/n, names.arg = rownames(newdata), las = 2, # for proportion of sample
        ylim = c(0, 0.5)) 

### Other plot types

lengths <- rowSums(TV.bin[,2:52])
lengthset <- data.frame(lengths = lengths, PARTID = TV.bin$Subject)
lengthset <- lengthset[order(-lengthset$lengths),, drop = F]

par(mar = c(4, 4, 1, 0)) # margins for plot
barplot(lengthset$lengths, names.arg = lengthset$PARTID, las = 2,
        cex.names = .7, xlab = "Participant", ylab = "list length") # find the most knowledgeable

plot(lengthset$lengths, pch = 16, xlab = NA, ylab = "list length", xaxt = "n")
plot(lengthset$lengths, type = "l", xlab = NA, ylab = "list length", xaxt = "n")

plot(lengthset$lengths, pch = 16, xlab = NA, ylab = "list length", xaxt = "n")
lines(lengthset$lengths)

plot(NA, xlim = c(-5, 15), ylim = c(0, .4), xlab = "list length", ylab = "density")
polygon(density(lengthset$lengths), col = "gray") # shape of distribution
abline(v = mean(lengthset$lengths), lty = 2)

##############################################
## Figure 2.1: Frequency distribution plot

par(mar = c(2, 10.5, 0, 1)) # make it vertically oriented
barplot(rev(newdata$SUM), names.arg = rev(rownames(newdata)), 
        las = 1, horiz = T, cex.names = 1.3)
# chances are, this won't fit on your monitor. If on RStudio,
# in the graphics pane, try Export --> Save as pdf --> Pdf size (A4) --> Portrait

##############################################
### R Code Box 2.2: Item Salience

FL.G <- CalculateSalience(FL10, Subj = "Subj", # item salience
                          Order = "Order", 
                          CODE = "GC",
                          Salience = "GC.S")
# You'll get some warnings. Read them! It's OK!
View(FL.G)

##############################################
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

##############################################
### R Code Box 2.4: Cultural Salience

GFL.S <- SalienceByCode(FL.G, Subj = "Subj", # cultural salience
                        CODE = "GC", Salience = "GC.S",
                        dealWithDoubles = "MAX")
par(mar = c(0, 0, 0, 0))
FlowerPlot(GFL.S, "Good") # make a flower plot

##############################################
### Figure 2.3: Hard-coding flower plots

# par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0)) # for vertical
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0)) # for horizontal

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

##############################################
### Figure 2.4 Coffee example

rowlabs <- c("color intensity", "brown color", "sweet flavor", "caramel flavor", "watery")
collabs <- c("P1", "P2", "P3", "P4", "P5", "P6")
colint <- c(.22, .11, .17, .15, .1, .12)
brncol <- c(.18, .20, .2, .1, .15, .12)
swtfla <- c(.32, .30, .30, .34, .53, .36)
carfla <- c(.03, .03, .03, .03, .12, .02)
watery <- c(.23, .22, .22, .01, .03, .05)
d <- data.frame(t(data.frame(colint, brncol, swtfla, carfla, watery, row.names = collabs)))

par(mar = c(5.5, 4, 1, 1), mfrow = c(1, 1))
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

##############################################
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

##############################################
## Salience in Sutrop (2001)

#L: # times item in all lists; N: # participants: mp: mean position
sutrop <- function(L, N, mP){ 
  S = L/(N * mP)
  return(S)
}

sutrop(1, 100, 1)
sutrop(2, 100, 4)
sutrop(25, 100, 18)

## Sutrop workflow
data("FruitList")
d <- FruitList
d.c <- CleanFreeList(d, Order = "Order", Subj = "Subj",
                     CODE = "CODE", ejectBadSubj = F, 
                     deleteDoubleCode = T,
                     ConsolidateOrder = T, RemoveMissingData = T)
d.s <- CalculateSalience(d.c, Order = "Order", Subj = "Subj",
                         CODE = "CODE", Salience = "Salience")
dsal <- SalienceByCode(d.s, Subj = "Subj",
                       CODE = "CODE", Salience = "Salience",
                       dealWithDoubles = "MAX")

L <- table(d.c$CODE) # times items listed
dmp <- aggregate(d.c$Order, list(d.c$CODE), FUN = mean) # mP
N <- length(unique(d.c$Subj)) # sample size
newdat <- data.frame(L, dmp)
newdat$Group.1 <- NULL
names(newdat)[names(newdat) == "Var1"] <- "CODE"
names(newdat)[names(newdat) == "Freq"] <- "L"
names(newdat)[names(newdat) == "x"] <- "mP"

newdat$Sutrop <- newdat$L/(N * newdat$mP)
newdat

newtab <- merge(newdat, dsal, by = "CODE") # merge
newtab <- newtab[order(-newtab$SmithsS),] # sort
newtab <- newtab[, c(1, 2, 3, 5, 6, 7, 4)] # reorder columns
newtab[,3:7] <- round(newtab[,3:7], 2) # round
newtab # clean table!

## Real Example
FL10 <- read.delim("Tyva Republic_Virtues_2010.txt") # Pull up Tyvan Virtue/Morality data
FL.G <- CalculateSalience(FL10, Subj = "Subj", # item salience
                          Order = "Order", 
                          CODE = "GC",
                          Salience = "GC.S")
GFL.S <- SalienceByCode(FL.G, Subj = "Subj", # cultural salience
                        CODE = "GC", Salience = "GC.S",
                        dealWithDoubles = "MAX")
n <- as.data.frame(GFL.S$SumSalience/GFL.S$SmithsS)[1, 1]

GFL.S$L <- GFL.S$SumSalience/GFL.S$MeanSalience # times someone lists something
GFL.S$B <- (GFL.S$SumSalience + GFL.S$L - 1)/(2 * n - 1) # Robbins, Nolan, Chen
GFL <- GFL.S[order(-GFL.S$SmithsS),, drop = F] # sort
colorder <- c("CODE", "L", "MeanSalience", "SumSalience",
              "SmithsS", "B")
GFL <- GFL[, colorder]

# now get mean order (k_i in Sutrop's)
labs <- c("Subj", "Order", "GC")
subdat <- FL10[labs]
d.c <- CleanFreeList(subdat, Order = "Order", Subj = "Subj",
                     CODE = "GC", ejectBadSubj = F, 
                     deleteDoubleCode = F,
                     ConsolidateOrder = T, RemoveMissingData = F)
d.c <- d.c[complete.cases(d.c),]
length(unique(d.c$Subj)) # check we've got the same n as above
newdat <- aggregate(d.c$Order, list(d.c$GC), FUN = mean) # mp
names(newdat)[names(newdat) == "Group.1"] <- "CODE"
names(newdat)[names(newdat) == "x"] <- "mP"

newtab <- merge(newdat, GFL.S, by = "CODE")
newtab$Sutrop <- newtab$L/(N * newdat$mP)
newtab <- newtab[order(-newtab$SmithsS),]

newtab <- newtab[newtab$SmithsS >= 0.09,] # by most salient

newtab <- newtab[, c(1, 6, 2, 3, 4, 5, 7, 8)]
newtab[, 3:8] <- round(newtab[, 3:8], 2)
newtab # compare the salience scores!

##############################################
## Salience in Robbins, Nolan, & Chen (2017)

data(FruitList) # from AnthroTools
test <- FruitList
test <- CalculateSalience(test, Rescale = F)
SBC <- SalienceByCode(test, dealWithDoubles = "MAX")

ntab <- as.data.frame(SBC$SumSalience/SBC$SmithsS)
n <- ntab[1, 1] # sample size

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
ntab <- as.data.frame(GFL.S$SumSalience/GFL.S$SmithsS)[1, 1] # resulting sample size

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

##############################################
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
n <- 100 # sample size
bvalues <- rnorm(n, .1, 0.5) # vector of individuals $B$ scores
hist(bvalues, xlab = "B", main = NA)
m1 <- lm(bvalues ~ 1) 
confint(m1) # 95\% confidence intervals; if these intervals are symmetrical around 0.5, 
# there is no bias, but the intervals will lean one direction or another 
# depending on the direction of bias.

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

##############################################
### Pearson's r Correlation Coefficient function with 95% CI

corfun <- function(x, y){
       devx <- x - mean(x)
       devy <- y - mean(y)
       devprod <- devx * devy
       sumdev <- sum(devprod)
       devxs <- devx^2
       devys <- devy^2
       devxssum <- sum(devxs)
       devyssum <- sum(devys)
       denom <- sqrt(devxssum) * sqrt(devyssum)
       r <- sumdev / denom
       df <- cbind(x, y)
       df <- df[complete.cases(df),]
       n <- nrow(df)
       z <- 0.5 * (log((1 + r) / (1 - r)))
       stderr <- 1/(sqrt(n - 3))
       upper <- z + 1.96 * stderr
       lower <- z - 1.96 * stderr
       rupper <- (exp(2 * upper) - 1) /
             (exp(2 * upper) + 1)
       rlower <- (exp(2 * lower) - 1) /
             (exp(2 * lower) + 1)
       return(data.frame(n = n, r = r, z = z, 
            lower = rlower, upper = rupper))
  }

y <- rnorm(100, 5, 1)
x <- y * 0.5 + rnorm(100, 0, 1)
corfun(y, x)
cor.test(y, x)

##############################################
### Note 10: F-distribution
###
n <- 1000
f1 <- rf(n, 1, 5)
f2 <- rf(n, 1, 10)
f3 <- rf(n, 1, 50)
f4 <- rf(n, 1, 100)
f5 <- rf(n, 50, 1)
f6 <- rf(n, 50, 10)
f7 <- rf(n, 50, 50)
f8 <- rf(n, 50, 100)
f9 <- rf(n, 100, 1)
f10 <- rf(n, 100, 50)

par(mfrow = c(2, 5), mar = c(4, 4, 1, 1))
hist(f1, main = NA, probability = T)
hist(f2, main = NA, probability = T)
hist(f3, main = NA, probability = T)
hist(f4, main = NA, probability = T)
hist(f5, main = NA, probability = T)
hist(f6, main = NA, probability = T)
hist(f7, main = NA, probability = T)
hist(f8, main = NA, probability = T)
hist(f9, main = NA, probability = T)
hist(f10, main = NA, probability = T)

##############################################
### R Code Box 2.5: Cronbach's alpha and intervals

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
crontab <- data.frame(d, dran)
xtable(crontab, digits = 0)

##############################################
# If using the data, please be sure to read, refer, and cite the following:

# Tyva Republic_Virtues_2010.txt: Purzycki, B. G., & Bendixen, T. (2020). Examining Values, Virtues, and Tradition in the Republic of Tuva with Free-List and Demographic Data. New Research of Tuva, (4), 6-18.
