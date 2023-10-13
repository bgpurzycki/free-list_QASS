##############################################
### Ethnographic Free-List Data
### Chapter 4: Sharedness and Overlap
### Benjamin Grant Purzycki
### Last Updated: September 29, 2023
##############################################

### Preliminaries
setwd()
setwd("C:/Users/au624473/Dropbox/2. In Progress/Articles and Books/Books/Methods with Free-List Data/3. Workflow/Data")

### Packages used in this chapter
library(AnthroTools)
library(euler)

#######################################################
### Footnote 1 Code Reference: Pearson's r function
corfun <- function(x, y){
  devx <- x - mean(x)
  devy <- y - mean(y)
  devprod <- devx*devy
  sumdev <- sum(devprod)
  devxs <- devx^2
  devys <- devy^2
  devxssum <- sum(devxs)
  devyssum <- sum(devys)
  denom <- sqrt(devxssum)*sqrt(devyssum)
  r <- sumdev/denom
  df <- cbind(x, y)
  df <- df[complete.cases(df),]
  n <- nrow(df)
  z <- .5*log((1 + r)/(1 - r))
  stderr <- 1/(sqrt(n - 3))
  upper <- z + 1.96*stderr
  lower <- z - 1.96*stderr
  rupper <- (exp(2*upper) - 1)/(exp(2*upper) + 1)
  rlower <- (exp(2*lower) - 1)/(exp(2*lower) + 1)
  return(data.frame(n = n, r = r, z = z, lower = rlower, upper = rupper))
}
#######################################################

# Tyva Example of Domain Overlap
tyva <- read.delim("Beliefs_Tyva_2013.txt", sep = "\t", header = T)
str(tyva) # BGD: buddha dislikes; LGD: spirits' dislikes; POD: cops' dislikes

BGDl <- c("CERCID", "BGD")
BGD <- tyva[BGDl]
BGD <- BGD[complete.cases(BGD),]
buddha <- FreeListTable(BGD, CODE = "BGD", tableType = "FREQUENCY", Subj = "CERCID")
colnames(buddha) <- paste(colnames(buddha), "bu", sep = ".")
buddha$Subject <- buddha$Subject.bu
#colname(buddha$Subject.sp <- NULL

LGDl <- c("CERCID", "LGD")
LGD <- tyva[LGDl]
LGD <- LGD[complete.cases(LGD),]
spirits <- FreeListTable(LGD, CODE = "LGD", tableType = "FREQUENCY", Subj = "CERCID")
colnames(spirits) <- paste(colnames(spirits), "sp", sep = ".")
spirits$Subject <- spirits$Subject.sp
#spirits$Subject <- NULL

PODl <- c("CERCID", "POD")
POD <- tyva[PODl]
POD <- POD[complete.cases(POD),]
cops <- FreeListTable(POD, CODE = "POD", tableType = "FREQUENCY", Subj = "CERCID")
colnames(cops) <- paste(colnames(cops), "po", sep = ".")
cops$Subject <- cops$Subject.po
#cops$Subject <- NULL

uber0 <- merge(buddha, cops, by = "Subject")
uber <- merge(spirits, uber0, by = "Subject") #

corfun(uber$Morality.bu, uber$Morality.po)
corfun(uber$Morality.sp, uber$Morality.po)
corfun(uber$Morality.bu, uber$Morality.sp)

### R Code Box 4.1: Jaccard's similarity index
# a/(a + b + c)
# a: number of rows where both columns are 1
# b: number of rows where this and not the other column is 1
# c: number of rows where the other and not this column is 1

jaccard <- function(var1, var2) {
  sums <- rowSums(cbind(var1, var2), na.rm = T)
  similarity <- length(sums[sums==2])
  total <- length(sums[sums==1]) + similarity
  similarity/total
}

M <- data.frame(cbind(c(1,0,0,0,1,1), # individual x category
                      c(0,1,0,1,0,1),
                      c(0,1,1,1,0,1)))
colnames(M) <- c("A", "B", "C")

jaccard(M$A, M$B)
jaccard(M$A, M$C)
jaccard(M$B, M$C)

jaccmat <- matrix(NA, ncol = ncol(M), nrow = ncol(M)) 

for(i in seq_along(M)) {
  for(j in seq_along(M)) {
    jaccmat[i, j] <- jaccard(M[, i], M[, j]) 
    colnames(jaccmat) <- colnames(M)
    rownames(jaccmat) <- colnames(M)
  }
}

jaccmat

### Figure. 4.1. Comparing agents
## Prelims
library(AnthroTools)

jaccard <- function(var1, var2) {
  sums <- rowSums(cbind(var1, var2), na.rm = T)
  similarity <- length(sums[sums==2])
  total <- length(sums[sums==1]) + similarity
  similarity/total
}

dd <- read.csv("Cross-cultural_ERM1.csv", sep = ";")
tyvasub <- subset(dd, dd$Culture == "Tyva Republic")
tyvalabs <- c("CERCID", "Order", "BGD", "LGD", "POD")
tyva <- tyvasub[tyvalabs]

# Presence
BGDbin <- FreeListTable(tyva, CODE = "BGD", Order = "Order", Subj = "CERCID", tableType = "PRESENCE")
LGDbin <- FreeListTable(tyva, CODE = "LGD", Order = "Order", Subj = "CERCID", tableType = "PRESENCE")
PODbin <- FreeListTable(tyva, CODE = "POD", Order = "Order", Subj = "CERCID", tableType = "PRESENCE")

BGDbin$`Food` <- 0 # BGD has no 'Food'
LGDbin$`D/K` <- 0  # LGD doesn't have any "I don't know's"
PODbin$`Ecology` <- 0  # LGD doesn't have any "I don't know's"
PODbin$`Religion` <- 0  # LGD doesn't have any "I don't know's"

BGDbin$BGDmor <- BGDbin$Morality # presence to merge later
LGDbin$LGDmor <- LGDbin$Morality # presence to merge later
PODbin$PODmor <- PODbin$Morality # presence to merge later

binmerge1 <- merge(BGDbin, LGDbin, by = "Subject", all = TRUE)
binmerge2 <- merge(binmerge1, PODbin, by = "Subject", all = TRUE)
lab0 <- c("Subject", "BGDmor", "LGDmor", "PODmor")
binmerge <- binmerge2[lab0]

# Frequency
labs1 <- c("CERCID", "Order", "BGD")
BGD <- tyva[labs1]
BGD <- BGD[complete.cases(BGD), ]

labs2 <- c("CERCID", "Order", "LGD")
LGD <- tyva[labs2]
LGD <- LGD[complete.cases(LGD), ]

labs3 <- c("CERCID", "Order", "POD")
POD <- tyva[labs3]
POD <- POD[complete.cases(POD), ]

BGDfreq <- FreeListTable(BGD, CODE = "BGD", Order = "Order", Subj = "CERCID", tableType = "FREQUENCY")
LGDfreq <- FreeListTable(LGD, CODE = "LGD", Order = "Order", Subj = "CERCID", tableType = "FREQUENCY")
PODfreq <- FreeListTable(POD, CODE = "POD", Order = "Order", Subj = "CERCID", tableType = "FREQUENCY")

BGDfreq$`Food` <- 0 # BGD has no "food"
LGDfreq$`D/K` <- 0 # LGD doesn't have any "I don't know's"
PODfreq$`Food` <- 0 # Cops don't eat
PODfreq$`Ecology` <- 0 # Cops aren't stewards

BGDfreq$BGDsum <- rowSums(BGDfreq[,2:11])
LGDfreq$LGDsum <- rowSums(LGDfreq[,2:11])
PODfreq$PODsum <- rowSums(PODfreq[,2:11])

colnames(BGDfreq)[which(names(BGDfreq) == "Morality")] <- "BGDmorf"
colnames(LGDfreq)[which(names(LGDfreq) == "Morality")] <- "LGDmorf"
colnames(PODfreq)[which(names(PODfreq) == "Morality")] <- "PODmorf"

freqmerge1 <- merge(BGDfreq, LGDfreq, by = "Subject", all = TRUE)
freqmerge2 <- merge(freqmerge1, PODfreq, by = "Subject", all = TRUE)

lab0 <- c("Subject", "BGDmorf", "BGDsum", "LGDmorf", "LGDsum", "PODmorf", "PODsum")
freqmerge <- freqmerge2[lab0]

moralmerge <- merge(binmerge, freqmerge, by = "Subject", all = TRUE)

jaccard(moralmerge$BGDmor, moralmerge$LGDmor)
jaccard(moralmerge$BGDmor, moralmerge$PODmor)
jaccard(moralmerge$LGDmor, moralmerge$PODmor)

# Figure 4.1. Overlap of "moral" content
library(eulerr)
fit <- euler(c(Buddha = 1, Spirits = 1, Police = 1, 
               "Buddha&Police" = .69, "Spirits&Police" = .13, "Buddha&Spirits" = .15))
fit$original.values # need to remove #'s we don't want in plot
valuestoplot <- c(NA, NA, NA, .15, .69, .13, NA)
plot(fit, fills = c("white", "darkgray", "lightgray"), quantities = valuestoplot)

### R Code Box 4.2: Cognitive Sharing and Diversity
# M: # total # of all possible listed items (i.e., wordbank)
# T: # total # of items listed by sample (NOTE: REPEATS)
# N: # sample size
# C: # ratio of distinct items by sample M/N

CSD <- function(MM, TT, N){ 
  numerator <- MM - TT
  CC <- MM/N
  CSD <- numerator/(MM - MM * N) ## Cognitive Sharing of the Domain
  Q <- CSD*CC ## Quantity of Organization
  return(data.frame(CSD = CSD, Q = Q, C = CC))
}

CSD(8, 15, 5)

MM <- 1:50
N <- 50
TT <- 1000

plot(CSD(MM, TT, 200)$CSD ~ MM, 
     type = "l", xlab = "M", ylab = "CSD",
     xlim = c(0, 50), ylim = c(0, 1), lty = 1)
lines(CSD(MM, TT, 100)$CSD ~ MM, lty = 2)
lines(CSD(MM, TT, 50)$CSD ~ MM, lty = 3)
lines(CSD(MM, TT, 25)$CSD ~ MM, lty = 4)

## Real examples
FL10 <- read.delim("Tyva Republic_Virtues_2010.txt") # Pull up Tyvan Virtue/Morality data
MM <- length(table(FL10$BC)) # total # of all possible listed items (i.e., word-bank)
TT <- sum(table(FL10$BC)) # total # of items listed by sample (NOTE: REPEATS)
N <- length(unique(FL10$Subj))# sample size
CSD(MM, TT, N)

lkin <- read.delim("Kinship_Lithuania.txt")
lkin.s <- CalculateSalience(lkin, Order = "order", Subj = "PARTID", CODE = "TRANS")
lkinsal <- SalienceByCode(lkin.s, Subj = "PARTID", CODE = "TRANS", 
                          Salience = "Salience", dealWithDoubles = "MAX") 
lkinsal <- lkinsal[with(lkinsal, order(-SmithsS)), ]

duplk <- duplicated(lkin[,c("PARTID", "TRANS")])
duplkin <- lkin[!duplk, ]

MM2 <- length(table(duplkin$TRANS)) # total # of all possible listed items (i.e., word-bank)
TT2 <- sum(table(duplkin$TRANS)) # total # of items listed by sample (NOTE: REPEATS)
N2 <- length(unique(duplkin$PARTID))# sample size
C2 <- MM2/N2 # ratio of distinct items by sample
CSD(MM2, TT2, N2)

# Figure 4.2: Relationship between length of M, n, and CSD
par(mar = c(4, 4, 1, 1))
plot(CSD(MM, TT, 200)$CSD ~ MM, 
     type = "l", xlab = expression(italic(M)), ylab = "CSD",
     xlim = c(20, 100), ylim = c(0, 1), lty = 1, lwd = 2)
lines(CSD(MM, TT, 100)$CSD ~ MM, lty = 2, lwd = 2)
lines(CSD(MM, TT, 50)$CSD ~ MM, lty = 3, lwd = 2)
lines(CSD(MM, TT, 25)$CSD ~ MM, lty = 4, lwd = 2)
legend(70, .9, title = expression(italic(n)*" = "), c("200", "100", "50", "25"), lty = c(1:4),
       cex = .8, lwd = 2)

### Comparing Groups
## Figure 4.3. Fasting in India
fasting <- read.csv("Fasting_Mysore.csv")
fasting.s <- CalculateSalience(fasting, Order = "Order", Subj = "Subj", CODE = "CODE",
                               GROUPING = "Religion", Salience = "Salience")
fastS <- SalienceByCode(fasting.s, Subj = "Subj", CODE = "CODE",
                        GROUPING = "Religion", Salience = "Salience", dealWithDoubles = "MAX")

labs <- c("SmithsS", "GROUPING", "CODE")
dat <- fastS[labs]

d <- reshape(dat, idvar = "CODE",
             timevar = "GROUPING",
             direction = "wide")
row.names(d) <- d$CODE
d$CODE <- NULL
colnames(d) <- c("Muslim", "Hindu")
d <- d[order(-d$Muslim),]
d <- as.matrix(t(d))
par(mar = c(7, 4, 1, 1))
barplot(height = d, beside = T, ylim = c(0, .5), las = 2,
        ylab = expression("Smith's "*italic(S)))
legend(20, .4, legend = c("Muslims", "Hindus"), fill = c("black", "lightgray"))
box()

## Figure 4.4. Salience of Church Free-List in R. of Ireland
d <- read.delim("Church_Ireland.txt")
d2 <- read.delim("Full_Ireland.txt")

d.s <- CalculateSalience(d, Order = "Order", Subj = "Subj", CODE = "Code", GROUPING = "Rej_Church", Salience = "Salience")
g.s <- SalienceByCode(d.s, CODE = "Code", Salience = "Salience", Subj = "Subj", GROUPING = "Rej_Church", dealWithDoubles = "MAX")

g.sorted <- g.s[with(g.s, order(-SmithsS)), ]

Data <- merge(d, d2, by = c("Subj", "Age", "Gender","SES", "Believe_God","Rej_Church"))

# General analyses
GenFL <- CalculateSalience(Data, Order = "Order", Subj = "Subj", CODE = "Code", GROUPING = NA, Rescale = FALSE, Salience = "Salience")
GenFL.S <- SalienceByCode(GenFL, Subj = "Subj", CODE = "Code", GROUPING = NA, Salience = "Salience", dealWithDoubles = "MAX")
GenFL.S <- GenFL.S[order(-GenFL.S$SmithsS),]
View(GenFL.S)

barplot(table(d2$God_Always_Never))
# 1: Always believed (believers)
# 2: Believe now but not before (apostates)
# 3: Don't believe now but did before
# 4: Don't believe and never did

FLfaith <- SalienceByCode(GenFL, Subj = "Subj", 
                          CODE = "Code", GROUPING = "God_Always_Never", 
                          Salience = "Salience", dealWithDoubles = "MAX")

#We order the Smith's S for each groups, starting with the highest Smith's S
grandcath <- FLfaith[order(-FLfaith$SmithsS),]

#splitting the groups
G1 <- subset(grandcath, GROUPING == 1) # 1: Always believed
G2 <- subset(grandcath, GROUPING == 2) # 2: Believe now but not before
G3 <- subset(grandcath, GROUPING == 3) # 3: Don't believe now but did before
G4 <- subset(grandcath, GROUPING == 4) # 4: Don't believe and never did

believers <- c(
  G1$SmithsS[G1$CODE =="Abuse/Paedophilia"],
  G1$SmithsS[G1$CODE =="Corrupt/Materialistic"],
  G1$SmithsS[G1$CODE =="Authoritarian/Dominant"],
  G1$SmithsS[G1$CODE =="Outdated/Conservative"],
  G1$SmithsS[G1$CODE =="Dishonest/Untrustworthy"]
)

apostates <- c(
  G3$SmithsS[G3$CODE =="Abuse/Paedophilia"],
  G3$SmithsS[G3$CODE =="Corrupt/Materialistic"],
  G3$SmithsS[G3$CODE =="Authoritarian/Dominant"],
  G3$SmithsS[G3$CODE =="Outdated/Conservative"],
  G3$SmithsS[G3$CODE =="Dishonest/Untrustworthy"]
)

d <- data.frame(believers, apostates)
labs <- c("Abuse", "Corrupt", "Authoritarian", "Outdated", "Dishonest") # put in order of apostates
rownames(d) <- labs
d <- d[order(-d$apostates),]

par(mar = c(5.5, 4, 1, 1))
plot(d$believers, ylim = c(0, .5), xaxt = "n",
     xlab = NA, ylab = expression("Smith's "*italic(S)), lwd = 2, type = "b", pch = 16)
lines(d$apostates, lty = 2, lwd = 2, type = "b", pch = 16)
axis(1, at = 1:5, labels = FALSE)
text(1:5-.3, par("usr")[3] - 0.05, labels = labs, 
     srt = 45, xpd = NA, adj = c(.55, 2))
legend(3, .5, legend = c("Believers", "Apostates"), lty = c(1:2), lwd = 2)

### Figure 4.5. Informal consensus analysis of RoI data
mycol1 <- rgb(255, 255, 255, max = 255, alpha = 100, names = "white")
mycol2 <- rgb(224, 224, 224, max=255, alpha = 100, names = "lightgray") 
mycol3 <- rgb(0, 0, 0, max = 255, alpha = 100, names = "darkgray")

ireland <- read.delim("Church_Ireland.txt")
ireland$Subj <- paste0("IRE", ireland$Subj) # add text to IDs

## Salience Analysis
ireland.s <- CalculateSalience(ireland, Order = "Order", Subj = "Subj",
                               CODE = "Code", Salience = "Salience")
sal.ireland <- SalienceByCode(ireland.s, CODE = "Code", Salience = "Salience",
                              dealWithDoubles = "MAX")
sal.ireland <- sal.ireland[order(-sal.ireland$SmithsS),, drop = F] # sort

## Informal Consensus Analysis
irebin <- FreeListTable(ireland, CODE = "Code", Order = "Order",
                        tableType = "PRESENCE", Subj = "Subj")
iresal <- FreeListTable(ireland.s, CODE = "Code", Order = "Order",
                        tableType = "MAX_SALIENCE", Subj = "Subj", 
                        Salience = "Salience")
irerank <- FreeListTable(ireland.s, CODE = "Code", Order = "Order",
                         tableType = "HIGHEST_RANK", Subj = "Subj", 
                         Salience = "Salience")

iremax <- irebin # enter one of the above to streamline analyses

iremaxlabs <- sal.ireland$CODE[1:10] # take top 10 salient items from sorted set
#iremaxsub <- iremax # for full set
iremaxsub <- iremax[iremaxlabs] # subset

tiremax <- data.frame(t(iremaxsub))

ire.dist1 <- dist(t(tiremax), method = "binary") # euclidean distances between the rows 
fitiredist1 <- cmdscale(ire.dist1, eig = TRUE, k = 2) # k is the number of dim 

coordinates <- data.frame(fitiredist1$points)
coordinates$Subj <- rownames(coordinates)

iresublabs <- c("Subj", "Rej_Church")
iresub <- ireland[iresublabs]
iresub <- iresub[!duplicated(iresub),]
xxx <- merge(coordinates, iresub, by = "Subj")

xxx$church <- NA
xxx$church[xxx$Rej_Church == 1] <- 2 # symbol for rejectors
xxx$church[xxx$Rej_Church == 0] <- 16 # symbol for believers
xxx <- xxx[complete.cases(xxx),]
table(xxx$Rej_Church)

(m1 <- lm(X1 ~ Rej_Church, data = xxx))
confint(m1)

splitdat <- split(xxx, as.factor(xxx$Rej_Church))
rej <- data.frame(splitdat[2])
chu <- data.frame(splitdat[1])
densrej <- density(rej$X1.X1)
denschu <- density(chu$X0.X1)

### plot
par(mfrow = c(1, 2), mar = c(2, 2, 0.5, 1))
plot(xxx$X1, xxx$X2, xlab = "Dimension 1", ylab = "Dimension 2",
     main = NA, type = "n", xlim = c(-0.7, 0.7), ylim = c(-0.4, 0.7)) 
points(xxx$X1, xxx$X2, pch = xxx$church, cex = 1) 
legend(0.02, 0.7, title = "Reject the church?", 
       legend = c("Yes", "No"), pch = c(2, 16),
       cex = 1.2, bty = "n")
plot(NA, xlim = c(-1, 1), ylim = c(0, 2))
polygon(denschu, col = mycol3) # believers
polygon(densrej, col = mycol1) # rejectors
legend(-0.9, 2, title = "Reject the church?", 
       legend = c("Yes", "No"), fill = c(mycol1, mycol3),
       cex = 1.2, bty = "n")

## Cultural F_ST
### R Code Box 4.3: Cultural F_ST for two sites
FST <- function(ni, nj, xi, xj){ 
  pi <- xi/ni
  pj <- xj/nj
  pbar <- (xi + xj)/(ni + nj)
  num <- (ni/(ni + nj))*(pi - pbar)^2 + (nj/(ni + nj))*(pj - pbar)^2
  denom <- pbar*(1 - pbar)
  FST <- num/denom
  return(FST)
}

### Figure 4.6: Values of CF_ST across levels of xj and xi.
par(mar = c(4, 4, 1, 1))
plot(NA, xlim = c(0, 100), ylim = c(0, 1),
     xlab = expression(italic('x'['j'])), ylab = expression(italic('CF'['ST'])))
lines(FST(100, 100, 0, 1:100), lty = 1, lwd = 1.5)
lines(FST(100, 100, 25, 1:100), lty = 2, lwd = 1.5)
lines(FST(100, 100, 50, 1:100), lty = 3, lwd = 1.5)
lines(FST(100, 100, 75, 1:100), lty = 4, lwd = 1.5)
lines(FST(100, 100, 100, 1:100), lty = 5, lwd = 1.5)
legend(37, 1, c("0", "25", "50", "75", "100"), title = expression(italic('x'['i'])*' ='), 
       lty = 1:5, cex = .9)

# Example
d <- read.csv("Cross-cultural_ERM1.csv", sep = ";")
dp <- FreeListTable(d, CODE = "BGD", Order = "Order", 
                    Subj = "CERCID", tableType = "PRESENCE", 
                    GROUPING = "Culture")
dp$freq <- rowSums(dp[,3:12])
dp <- dp[dp$freq != 0, ]  

labs <- c("Group", "Subject", "Morality", "freq")
dat <- dp[labs]

mortab <- (table(dat$Morality, dat$Group))
mortab1 <- mortab[2,]
size <- colSums(mortab)
slab <- (as.data.frame((rbind(size, mortab1))))

### Table 4.3 (trunc.): Sample size, n, and number who listed "moral" items
tslab <- as.data.frame(t(slab))

### R Code Box 4.4: Cultural F_ST matrix
fstmatrix <- function(tslab, matrixtype = NULL) {
  m <- matrix(NA, nrow = nrow(tslab), ncol = nrow(tslab))
  rownames(m) <- colnames(m) <- rownames(tslab)
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
      m[i, j] <- FST(tslab[i, 1], tslab[j, 1], tslab[i, 2], tslab[j, 2])   
    }
  }
  if(!is.null(matrixtype)) {
    if(matrixtype == "upper") {
      m[lower.tri(m)] <- 0
    }
    if(matrixtype == "lower") {
      m[upper.tri(m)] <- 0
    }
  }
  return(m)
}

fstmatrix(tslab)
fstmatrix(tslab, matrixtype = "lower") 
fstmatrix(tslab, matrixtype = "upper")

### Figure 4.7: Dendogram of CFST scores for "moral" items
fstdist <- as.dist(fstmatrix(tslab, matrixtype = "lower"))
round(fstdist, 2) ### Table 4.3
plot(hclust(fstdist))
