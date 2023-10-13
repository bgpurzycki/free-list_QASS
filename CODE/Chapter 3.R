##############################################
### Ethnographic Free-List Data
### Chapter 3: Structure Analysis
### Benjamin Grant Purzycki
### Last Updated: August 7, 2023
##############################################

### Preliminaries
rm(list = ls()) # clear your workspace
setwd()
setwd("C:/Users/au624473/Dropbox/2. In Progress/Articles and Books/Books/Methods with Free-List Data/3. Workflow/Data")

library(AnthroTools)
library(igraph)
library(xtable)

### Figure 3.1: Absolute distance and logarithmic transformation
x <- 1:50
plot(log(x) ~ x, type = "l",
     xlab = "Absolute difference", ylab = "log(Absolute difference)")

#######################################################################
## Distance function for adjacency matrix
UBdistance <- function(d) {
        dmain <- t(combn(colnames(d), 2))
        dout <- rep(0, nrow(dmain))
        for(i in 1:nrow(d)) {
                row <- d[i, ]
                order <- row[row > 0]
                items <- names(row)[row > 0]
                couplings <- t(combn(items, 2))
                for(j in 1:nrow(couplings)) {
                        distance <- abs(order[items == couplings[j,][1]] - order[items == couplings[j, ][2]])
                        {
                                distance <- log(distance)
                        }
                        inds <- which(rowSums(matrix(dmain %in% c(couplings[j, ][1], 
                                                                  couplings[j, ][2]), nrow(dmain), ncol(dmain), 
                                                     byrow = FALSE)) == 2)
                        dout[inds] <- dout[inds] + distance
                }
        }
        d.bin <- apply(d, 2,
                       function(x) ifelse(x > 0, 1, 0)) # recode
        d.corr <- crossprod(d.bin) # correspondence matrix
        diag(d.corr) <- 0 # make the diagonal 0s
        items <- rownames(d.corr)
        dyadind <- seq_along(items)
        dyadpairs <- vector("list", length(dyadind) * (length(dyadind) - 1) / 2)
        coocs <- numeric(0)
        pairind <- 1
        for (i in 1:(length(dyadind) - 1)) {
                for (j in (i + 1):length(dyadind)) {
                        dyadpairs[[pairind]] <- c(items[i], items[j])
                        coocs <- c(coocs, d.corr[i, j])
                        pairind <- pairind + 1
                }
        }
        dyadpairs.mat <- matrix(unlist(dyadpairs), ncol = 2, byrow = TRUE)
        dyadmat <- data.frame(item1 = dyadpairs.mat[, 1], item2 = dyadpairs.mat[, 2], cooccur = coocs)
        dout <- dout / dyadmat$cooccur
        dout <- exp(dout)
        dat <- data.frame(dmain)
        colnames(dat) <- c("item1", "item2")
        dat$distance <- dout
        dat$cooccur <- dyadmat$cooccur
        dat <- dat[complete.cases(dat),]
        return(dat)
}
#######################################################################

### R Code Box 3.2: Multi-dimensional scaling (MDS)
## Fake Example
tuvani <- read.delim("Animals_SIM.txt")
tv.min <- FreeListTable(tuvani, CODE = "CODE", tableType = "HIGHEST_RANK",
                        Order = "ORDER", Subj = "PARTID")
tv.min$Subject <- NULL ### Table 3.2. Minimum order matrix

tv.bin <- FreeListTable(tuvani, CODE = "CODE", tableType = "PRESENCE",
                        Order = "ORDER", Subj = "PARTID")
tv.bin$Subject <- NULL ### Table 3.2. Minimum order matrix

tv.dist1 <- dist(t(tv.min)) # euclidean distances between the rows 
tv.dist2 <- dist(t(tv.bin)) # euclidean distances between the rows 

fit.tv.dist1 <- cmdscale(tv.dist1, eig = TRUE, k = 2) # k is the number of dim 
fit.tv.dist2 <- cmdscale(tv.dist2, eig = TRUE, k = 2) # k is the number of dim 

x1 <- fit.tv.dist1$points[,1] 
y1 <- fit.tv.dist1$points[,2] 

x2 <- fit.tv.dist2$points[,1] 
y2 <- fit.tv.dist2$points[,2] 

### R Code Boxes 3.3-3.4: Multi-dimensional scaling
### Figure 3.2: MDS plot and dendogram
par(mfrow = c(1, 2))
par(mar = c(4, 4, 1, 1))
plot(x1, y1, xlab = "Dimension 1", ylab = "Dimension 2",
     main = NA, type = "n", xlim = c(-8, 7)) 
animals <- c("sheep", "goat", "yak", "cow", "camel", "snake", "fox", "crane") 
text(x1, y1, labels = animals, cex = .9) 

par(mar = c(4, 4, 1, 1))
plot(x2, y2, xlab = "Dimension 1", ylab = "Dimension 2",
     main = NA, type = "n", xlim = c(-8, 7)) 
animals <- c("sheep", "goat", "yak", "cow", "camel", "snake", "fox", "crane") 
text(x2, y2, labels = animals, cex = .9) 

par(mar = c(0, 1, 0, 0))
plot(hclust(tv.dist1), main = NULL, sub = NULL, 
     xlab = NA, ylab = NA, axes = F)
plot(hclust(tv.dist2), main = NULL, sub = NULL, 
     xlab = NA, ylab = NA, axes = F)

### Table 3.1. Table in LaTeX
library(xtable)
df1 <- tuvani[row.names(tuvani) %in% 1:25, ]
df2 <- tuvani[row.names(tuvani) %in% (26):nrow(tuvani), ]
df2[nrow(df2) + 1,] <- NA
dfm <- cbind(df1, df2)
xtable(dfm, include.rownames = F)

## Spain Example
dd <- read.delim("Colors_Spain.txt")
d <- reshape(dd, varying = names(dd[,2:31]),
             timevar = "order", idvar = "PARTID",
             direction = "long", sep = "")
d <- d[complete.cases(d$LIST),]

d.s <- CalculateSalience(d, Order = "order", CODE = "LIST", Subj = "PARTID", Salience = "Salience")
ds <- SalienceByCode(d.s, CODE = "LIST", Subj = "PARTID", Salience = "Salience")
ds <- ds[order(-ds$SmithsS),, drop = F] # sort
dssub <- ds[ds$SmithsS > 0.09,]
dssublabs <- dssub$CODE
dssublabs.t <- c("red", "blue", "green", "yellow", "black", "white", "pink", "orange",
                 "gray", "purple", "brown", "violet", "lightblue","lilac", "aquamarine",
                 "turquoise", "watergreen", "beige", "magenta", "fuschia")
dssub$Trans. <- dssublabs.t
dssub$nlisted <- dssub$SumSalience/dssub$MeanSalience
dssub <- dssub[, c(1, 5, 6, 2, 3, 4)]
dssubtab <- cbind(dssub[,1:3], round(dssub[4:6], 2))
xtable(dssubtab)

colmin <- FreeListTable(d, CODE = "LIST", tableType = "HIGHEST_RANK",
                        Order = "order", Subj = "PARTID")
colmin$Subject <- NULL           
colminsub <- colmin[dssublabs]
colnames(colminsub) <- dssublabs.t

## Euclidean Distance
coldist1 <- dist(t(colminsub)) # euclidean distances between the rows 
fitcoldist1 <- cmdscale(coldist1, eig = TRUE, k = 2) # k is the number of dim 
x1 <- fitcoldist1$points[,1] 
y1 <- fitcoldist1$points[,2] 

## UB Distance
colubdist <- UBdistance(colminsub) # use UBdistance function from above

categories <- unique(c(colubdist[["item1"]], colubdist[["item2"]]))
distmat <- matrix(0, nrow = length(categories), ncol = length(categories))
for (i in 1:nrow(colubdist)) {
        item1 <- colubdist[i, "item1"]
        item2 <- colubdist[i, "item2"]
        distance <- colubdist[i, "distance"]
        rowindex <- match(item1, categories)
        colindex <- match(item2, categories)
        distmat[rowindex, colindex] <- distance
        distmat[colindex, rowindex] <- distance
}
rownames(distmat) <- colnames(distmat) <- dssublabs.t
coldist2 <- as.dist(distmat)

fitcoldist2 <- cmdscale(distmat, eig = TRUE, k = 2) # k is the number of dim 
x2 <- fitcoldist2$points[,1] 
y2 <- fitcoldist2$points[,2] 

# Plot
par(mfrow = c(1, 2))
par(mar = c(4, 4, 1, 1))
#plot(x1, y1, xlab = "Dimension 1", ylab = "Dimension 2",
#     main = NA, type = "n", xlim = c(-25, 25), ylim = c(-25, 25)) 
#text(x1, y1, labels = dssublabs.t, cex = .9) 
#par(mar = c(0, 1, 0, 0))
#plot(hclust(coldist1), main = NULL, sub = NULL, 
#     xlab = NA, ylab = NA, axes = F)
#par(mar = c(4, 4, 1, 1))
plot(x2, y2, xlab = "Dimension 1", ylab = "Dimension 2",
     main = NA, type = "n", xlim = c(-6, 7)) 
text(x2, y2, labels = dssublabs.t, cex = .9) 
par(mar = c(0, 1, 0, 0))
plot(hclust(coldist2), main = NULL, sub = NULL, 
     xlab = NA, ylab = NA, axes = F)

## Pemba Example
d <- read.delim("LivingThings_Pemba.txt", header = T, sep = "\t")
pemba.s <- CalculateSalience(d, Order = "ORDER", Subj = "anonyme_id", CODE = "response")
pemba.sal <- SalienceByCode(pemba.s, CODE = "response", Salience = "Salience", Subj = "anonyme_id",
                            dealWithDoubles = "MAX")
pemba.sal <- pemba.sal[order(-pemba.sal$SmithsS),, drop = F] # sort
rownames(pemba.sal) <- NULL # reset row numbers
par(mar = c(0, 0, 0, 0))
AnthroTools:::FlowerPlot(pemba.sal, "Living things")
pembatab <- pemba.sal[1:10,]
pembatab$trans <- c("cow", "mango", "goat", "jackfruit", "coconut", 
                    "orange", "chicken", "guava", "dove", "lychee")
pembatab$n <- pembatab$SumSalience/pembatab$MeanSalience
colorder <- c("CODE", "trans", "n", "MeanSalience", "SumSalience", "SmithsS")
pembatab <- pembatab[, colorder]

### Table 3.5: Salience analyses in Pemba
xtable(pembatab[1:10,])

## Structure analysis
top10labs <- pemba.sal$CODE[1:10] # names of top 10-most salient items

top10 <- d[d$response %in% top10labs,] # subset all instances of these items

tv.min <- FreeListTable(top10, CODE = "response", tableType = "HIGHEST_RANK",
                        Order = "ORDER", Subj = "anonyme_id")
tv.min$Subject <- NULL # delete ID's 
colnames(tv.min) <- c("cow", "goat", "orange", "mango", 
                      "jackfruit", "coconut", "guava",
                      "lychee", "chicken", "dove")
tv.dist <- dist(t(tv.min)) # euclidean distances between the rows 

fit.tv.dist <- cmdscale(tv.dist, eig = TRUE, k = 2) # k is the number of dim 

x <- fit.tv.dist$points[,1] 
y <- fit.tv.dist$points[,2] 

### Figure 3.3: MDS of ten-most salient items in Pemba
fit.tv.dist3 <- cmdscale(tv.dist, eig = TRUE, k = 3) # k is the number of dim 

x3 <- fit.tv.dist3$points[,1] 
y3 <- fit.tv.dist3$points[,2] 
z3 <- fit.tv.dist3$points[,3] 

par(mfrow = c(2, 2))
par(mar = c(4, 4, 1, 1))
plot(x3, y3, xlab = "Dimension 1", ylab = "Dimension 2",
     main = NA, type = "n", xlim = c(-300, 300)) 
#entities <- rownames(fit.tv.dist$points)
entities <- c("cow", "goat", "orange", "mango", 
              "jackfruit", "coconut", "guava",
              "lychee", "chicken", "dove")
text(x3, y3, labels = entities, cex = .9) 

plot(x3, z3, xlab = "Dimension 1", ylab = "Dimension 3",
     main = NA, type = "n", xlim = c(-300, 300)) 
text(x3, z3, labels = entities, cex = .9) 

plot(z3, y3, xlab = "Dimension 3", ylab = "Dimension 2",
     main = NA, type = "n", xlim = c(-300, 300)) 
text(z3, y3, labels = entities, cex = .9)

par(mar = c(0, 1, 0, 0))
plot(hclust(tv.dist), main = NULL, sub = NULL, 
     xlab = NA, ylab = NA, axes = F)

## Conceptual cd.bin
## Conceptual chunking

## Conceptual Network

library(AnthroTools)
library(igraph)

# Fruit example
data("FruitList")
dat <- FruitList
labs <- c("Subj", "CODE")
d <- dat[labs]
d.tab <- table(d[1:2])
d.bin <-  apply(d.tab, 2, 
                function(x) ifelse(x > 0, 1, 0)) # turn all >0 into 1
V <- crossprod(d.bin) # correspondence matrix
diag(V) <- 0
par(mar = c(0, 0, 0, 0))
network1 <- graph_from_adjacency_matrix(V, mode = "undirected", diag = F)

# Tuvan virtues example
tuva <- read.delim("Tyva Republic_Virtues_2010.txt")
labs <- c("Subj", "GC2")
td <- tuva[labs]
td <- td[!(td$GC2 == "I don't know"),] # drop "I don't know"
td.tab <- table(td[1:2])
td.bin <-  apply(td.tab, 2, 
                 function(x) ifelse(x > 0, 1, 0)) # recode
tV <- crossprod(td.bin) # correspondence matrix
diag(tV) <- 0
tV
par(mar = c(0, 0, 0, 0))
network2 <- graph_from_adjacency_matrix(tV, mode = "undirected")
plot(network2)

par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
plot(network1)
plot(network2)

# Conceptual Chunking
### Table 3.7: Table illustrating clustering
d <- data.frame(
        ID1 = c("f", "f", "f", "f", "f", "n", "n", "n", "n", "n"),
        ID2 = c("f", "f", "f", "n", "n", "n", "n", "n", "f", "f"),
        ID3 = c("f", "n", "f", "n", "f", "n", "f", "n", "f", "n"),
        ID4 = c("f", "f", "f", "n", "n", "n", "f", "f", "f", "n"),
        ID5 = c("f", "n", "n", "n", "n", "n", "n", "n", "n", "f"),
        ID6 = c("f", "f", "n", "n", "f", "f", "n", "n", "f", "f")
)

### R Code Box 3.7: Function for calculating clusters
Cluster.fun <- function(var){
        varnums <- as.numeric(as.factor(var))
        y <- rle(varnums)
        n1 <- sum(y$lengths[y$values == 1])
        n2 <- sum(y$lengths[y$values == 2])
        r1 <- length(y$lengths[y$values==1])
        r2 <- length(y$lengths[y$values==2])
        C1 <- (n1 - r1)/(n1 - 1)
        C2 <- (n2 - r2)/(n2 - 1)
        N <- length(var)
        R <- length(y$lengths)
        K <- length(unique(var))
        Cprime <- (N - R)/(N - K)
        return(data.frame(C1 = C1, C2 = C2, Cprime = Cprime))
}

lapply(d[, 1:6], Cluster.fun)

### Figure 3.5: Conceptual networks of what eats what
FN <- data.frame(read_excel("Animals_VancouverIsland.xlsx", sheet = "First Nations"))
NonFN <- data.frame(read_excel("Animals_VancouverIsland.xlsx", sheet = "Non-First Nations"))

gFN <- graph_from_data_frame(FN, directed = T, vertices = FN$EATEN)
gNFN <- graph_from_data_frame(NonFN, directed = T, vertices = NonFN$EATEN)

par(mfrow = c(2 ,1), mar = c(0, 0, 0, 0))
plot(gFN, edge.width = E(gFN)$SMITHSS*15, edge.arrow.size = E(gFN)$SMITHSS*7, 
     vertex.color = NA,
     vertex.label.color = "black")
plot(gNFN, edge.width = E(gNFN)$SMITHSS*15, edge.arrow.size = E(gNFN)$SMITHSS*3,
     vertex.color = "gray",
     vertex.label.color = "black")

## Triads
# Preliminary functions
nitems <- function(n){ # n = number of items; calculates number of triads needed
        x <- (n * (n - 1) * (n - 2))/6
        print(x)
}

pairs <- function(n){ # calculates how many pairs there will be
        x <- n*(n-1)/2
        print(x)
}

## Examine relationship between # of items and # of triads
items <- 3:20 # vector of different list lengths
triads.required <- nitems(3:20) # run through function

### Figure 3.6: Relationship between word-bank size and triads needed
par(mar = c(4, 4, 1, 1)) # plot
plot(items, triads.required, pch = 16, xlab = "Number of items in bank", ylab = "Number of triads required")
lines(items, triads.required)

### R Code Box 3.8: Triad Test Workflow
## 1. Create instrument
# Items to use
RVS <- c("Ben", "Uffe", "Jesper", 
         "Lene", "Jorn", "Marianne Q.-F.",
         "Thomas", "Martin", "Mark")

nitems(length(RVS))
pairs(length(RVS))

triadlist <- data.frame(t(combn(RVS, 3))) # all possible triad combs
triadlist$NUM <- seq(1, nrow(triadlist), 1) # assign triad a number
triadlist <- triadlist[, c(4, 1, 2, 3)] # rearrange variable order
triadlist$rand <- sample(1:nrow(triadlist), nrow(triadlist), replace = F)
triadlist <- triadlist[order(triadlist$rand),] # randomize!
#write.csv(triadlist, "facultytriadtask.csv", row.names = F) # make a .csv of instrument to print out and administer

## 2. Print, collect, and enter data into spreadsheet!
## 3. Enter data.
## 4. Load data.
d <- read.delim("FacultyTriad_Aarhus.txt") # Table 3.8

## 5. Analysis
d.diff <- triad.test(d) # Table 3.9: Dissimilarity matrix
d.sim <- as.dist(1 - d.diff) # Similarity matrix

### Figure 3.6: Dendogram of triad test
par(mar = c(0, 2, 0, 0))
plot(hclust(d.sim), main = NULL)

## Pile sorting
### Table 3.10: Similarity matrix of sorted animal data
Animals <- read.delim("Animals_PileSort.txt", header = T, sep = "\t")
rownames(Animals) <- Animals[,1]
Animals$X <- NULL
Animals <- 1 - Animals

animalscale <- cmdscale(Animals)

### Figure 3.8: MDS plot of animal pile sorts
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1))
plot(animalscale, type = "n", 
     xlab = NA, 
     ylab = NA,
     main = NA, 
     xlim = c(-1, .5), ylim = c(-.5, .6))
text(animalscale, labels = names(Animals))
ani <- as.dist(Animals)
plot(hclust(ani), main = NA, sub = NULL, 
     xlab = NULL, ylab = NA, axes = F)
