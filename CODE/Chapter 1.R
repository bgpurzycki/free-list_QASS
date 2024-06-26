##############################################
### Ethnographic Free-List Data
### Chapter 1: Introduction
### Benjamin Grant Purzycki
### Last Updated: March 22, 2024
##############################################

##############################################
### Preliminaries

setwd() # set to where your data are
library(AnthroTools) # load packages (to install, see below)

##############################################
### R Code Box 1.1: R Basics

x <- 46.2 # create an object called "x" and put 46.2 in it
ezg <- c("ha", "HA", "Ha", "hA")
getwd() # see where your working directory is
setwd("C:") # set your working directory to the C: drive
str(x) # see structure of x
summary(x) # summarize x
View(x)
plot(x)
install.packages("psych") # install "psych" package
library(psych) # load "psych" package
y <- 4 # create "y"
merge(x, y)
tolower(ezg)
toupper(ezg)
z <- merge(x, y)
read.csv()
read.delim()
write.table(z, "z.txt", sep = "\t")
z[1,2]
z$x
z <- as.data.frame(z)

##############################################
### R Code Box 1.2: Installing AnthroTools

install.packages("devtools")
library("devtools")
install_github('alastair-JL/AnthroTools')
library(AnthroTools)

##############################################
### Footnote 1:  Convert from FLAME format.

# 1. Manually delete all rows and columns that aren't data.
# 2. Insert NA into all empty cells in the data frame.
# 3. Save as .csv.
FLa <- read.csv("Taboos_Mongolia.csv") # see citation below
View(FLa) # View data set in original state
FLT <- data.frame(t(FLa))
FLT$SUBJ <- NA
FLT$SUBJ <- rownames(FLT)
FL.U <- cbind(FLT$SUBJ, stack(FLT[1:9])) # if grouping var, include with FLT$SUBJ, e.g., FL.U <- cbind(FLT[10:11], stack(FLT[1:9]))
FL.U$order <- as.numeric(FL.U$ind)
FL <- FL.U[complete.cases(FL.U), ]
FL$ind <- NULL
FL.S <- CalculateSalience(FL, Order = "order", Subj = "FLT$SUBJ",
                          CODE = "values")
model <- SalienceByCode(FL.S, CODE = "values", 
                        Subj = "FLT$SUBJ", Salience = "Salience", 
                        dealWithDoubles = "MAX")
par(mar = c(0, 0, 0, 0)) # for margins
FlowerPlot(model, "taboos") # unpleasant, but you get the idea

##############################################
### R Code Box 1.3: Loading and Saving Data

FL <- read.delim("Tyva Republic_Virtues_2010.txt") # Load data
str(FL) # structure of FL
View(FL) # View it

write.table(FL, file = "TVtest.txt", sep = "\t", # Save data
  row.names = F, col.names = T)
testset <- read.delim("TVtest.txt")

##############################################
### R Code Box 1.4: Reshaping Data

id <- c("Winston", "Egon", "Ray", "Peter")
item1 <- c("librarian", "librarian", "slimer", "librarian")
item2 <- c("gozer", "gozer", "zuul", "zuul")
item3 <- c("slimer", "zuul", "marsh.man", "slimer")
item4 <- c("zuul", "slimer", "librarian", "cab.driver")

(Fwide <- data.frame(cbind(id, item1, item2, item3, item4))) # notice the () around this code
(Flong <- reshape(Fwide,
                  varying = c("item1", "item2", "item3", "item4"),
                  timevar = "order", idvar = "id",
                  direction = "long", sep = ""))
row.names(Flong) <- NULL
Flong

(wide.again <- reshape(Flong, 
                       timevar = "order",
                       idvar = "id",
                       v.names = "item",
                       direction = "wide"))

##############################################
### R Code Box 1.5: Merging Data

FL1 <- data.frame(PARTID = c("FL001", "FL001", "FL001",
                             "FL002", "FL002", "FL002",
                             "FL003", "FL003"),
                  Order = c(1, 2, 3, 1, 2, 3, 1, 2),
                  Item = c("fee", "fi", "fo",
                           "fum", "fi", "fo",
                           "foo", "fum"))
FL2 <- data.frame(PARTID = c("FL001", "FL001", "FL001",
                             "FL002", "FL002", "FL002",
                             "FL003", "FL003"),
                  Order = c(1, 2, 3, 1, 2, 3, 1, 2),
                  Item = c("la", "di", "da",
                           "dee", "da", "la",
                           "doo", "dum"))
FL3 <- data.frame(PARTID = c("FL001", "FL001", 
                             "FL002", "FL002", "FL002",
                             "FL003", "FL003", "FL003"),
                  Order = c(1, 2, 1, 2, 3, 1, 2, 3),
                  Item = c("tim", "tum", 
                           "tiddle", "um", "tum",
                           "tee", "tie", "tiddle"))

mr1 <- merge(FL1, FL2, by = c("PARTID", "Order"),
             all.x = T, all.y = T)
mr2 <- merge(mr1, FL3, by = c("PARTID", "Order"),
             all.x = T, all.y = T)
collabs <- c("PARTID", "Order", "FL1", "FL2", "FL3")
colnames(mr2) <- collabs
mr2 # see the auto-NA's

##############################################
### Footnote 2: Automatically numbering pre-sorted data

ID <- c("ID1", "ID1", "ID1", "ID1", "ID2", "ID2", "ID2", "ID3", "ID3", "ID3", "ID3", "ID3")
items <- c("cereal", "apples", "oranges", "gruel", "apples", "fish.heads", "gruel", "cereal", "apples", "muesli", "fruit", "oranges")
data <- data.frame(ID, items) 
View(data)
runs <- rle(data$ID)$lengths
data$order <- sequence(runs)
View(data)

##############################################
### Data Cleaning Function

# Don't use this blindly, especially the "ejectBadSubj"
# command!!!

CleanData <- CleanFreeList(mydata, Order = "Order",
                           Subj = "Subj", CODE = "CODE",
                           ejectBadSubj = F, deleteDoubleCode = T,
                           ConsolidateOrder = T, RemoveMissingData = T)

data(UglyList)
View(UglyList)
View(CleanFreeList(UglyList))
View(CleanFreeList(UglyList, deleteDoubleCode = T))
View(CleanFreeList(UglyList, ejectBadSubj = F))

##############################################
### Footnote 3: Cohen's kappa of inter-rater agreement

# kappa = (p_0 - p_c)/(1 - p_c)

# For simple 2 x 2 category agreement

cohensk <- function(d){
  n <- sum(d)
  po <- (d[1, 1] + d[2, 2])/n
  pc <- (sum(d[1, ])/n) * (sum(d[, 1])/n) +
    (sum(d[2, ])/n) * (sum(d[, 2])/n)
  kappa <- (po - pc) / (1 - pc)
  return(data.frame(kappa = kappa))
}

(d <- data.frame(yes = c(25, 15), no = c(10, 20), row.names = c("yes", "no")))
cohensk(d)

# For more than two categories
cohensk.m <- function(d){
  diag <- diag(d)
  n <- sum(d)
  colFreqs <- colSums(d)/n
  rowFreqs <- rowSums(d)/n
  po <- sum(diag)/n
  pc <- crossprod(colFreqs, rowFreqs)[1]
  kappa <- (po - pc)/(1 - pc)
  return(kappa)
}

dat <- read.csv("Cross-cultural_ERM1.csv", sep = ";")
labs <- c("POD_GEN_NC", "POD_GEN_TL") # what do the police dislike? two coders
d <- dat[labs]
#d <- d[complete.cases(d),] # should be square
(dtab <- table(d))

agreements <- as.table(rbind( # prepare data
  as.vector(dtab[1, ]), as.vector(dtab[2, ]), 
  as.vector(dtab[3, ]), as.vector(dtab[4, ]), 
  as.vector(dtab[5, ]), as.vector(dtab[6, ]), 
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), as.vector(dtab[7, ]), 
  as.vector(dtab[8, ]), as.vector(dtab[9, ])))
labs <- colnames(dtab)
dimnames(agreements) <- list(coder1 = labs, coder2 = labs)
agreements

cohensk.m (agreements) 

# See Landis and Koch (1977: 165)
# Agreement Levels
# < 0.00 Poor
# 0.00-0.20 Slight
# 0.21-0.40 Fair
# 0.41-0.60 Moderate
# 0.61-0.80 Substantial
# 0.81-1.00 Almost Perfect

##############################################
# If using the data, please be sure to read, refer, and cite the following:

# Taboos_Mongolia.csv: Berniūnas, R. (2020). Mongolian yos surtakhuun and WEIRD “morality”. Journal of Cultural Cognitive Science, 4(1), 59-71.
# Tyva Republic_Virtues_2010.txt: Purzycki, B. G., & Bendixen, T. (2020). Examining Values, Virtues, and Tradition in the Republic of Tuva with Free-List and Demographic Data. Новые исследования Тувы, (4), 6-18.
# Cross-cultural_ERM1.csv: Bendixen, T., Apicella, C., Atkinson, Q., Cohen, E., Henrich, J., McNamara, R. A., ... & Purzycki, B. G. (2023). Appealing to the minds of gods: Religious beliefs and appeals correspond to features of local social ecologies. Religion, Brain & Behavior, 1-23.
