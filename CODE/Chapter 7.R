#############################
### Ethnographic Free-List Data: Chapter 7
### SAGE
### Benjamin Grant Purzycki
### Last Updated: October 13, 2023
################################

### Preliminaries

library(dagitty)

mycol1 <- rgb(255, 255, 255, max = 255, alpha = 100, names = "white")
mycol2 <- rgb(224, 224, 224, max=255, alpha = 100, names = "lightgray") 
mycol3 <- rgb(0, 0, 0, max = 255, alpha = 100, names = "darkgray")

### Figure 7.3: Simulated effects and the consequences of "controls"

par(mfrow = c(3, 2), mar = c(4, 1, 5, 1) + 0.5) #bottom, left, top, right

# Model 1
plot(dagitty('dag {
             Z [pos = "0.5, 0"]
             X [pos = "0, 0.5"]
             Y [pos = "1, 0.5"]
             Z -> X
             Z -> Y
             X -> Y
             }'))

fd1 <- function(n, a, z1, z2) {
  Z <- rnorm(n, 0, 1) # Z
  e_x <- rnorm(n, 0, 1) # error in X
  e_y <- rnorm(n, 0, 1) # error in Y
  X <- z1 * Z + e_x # X
  Y <- a * X + z2 * Z + e_y # Y
  df <- data.frame(X, Y, Z)
  open <- coef(lm(Y ~ X, data = df))[2]
  controlled <- coef(lm(Y ~ X + Z, data = df))[2]
  return(c(open, controlled))
}

trisim1 <- data.frame(t(replicate(1000, fd1(100, .5, .5, .5))))
names(trisim1) <- c("open", "controlled")

densop1 <- density(trisim1$open)
densco1 <- density(trisim1$controlled)

par(mar = c(2, 1, 1, 1))
plot(NA, xlab = NA, ylab = "", 
     xlim = c(-.75, 1.3), 
     ylim = c(0, 4.5), 
     cex.lab = 1.3, yaxt = 'n')
polygon(densop1, col = mycol1) # open
polygon(densco1, col = mycol3) # closed
abline(v = 0.5, lty = 2)
legend("topleft", legend =c("Z Open", "Z Controlled"), 
       fill = c(mycol1, mycol3), cex = 1, 
       horiz = F, bty = T, inset = c(0.03, 0.15), box.col = "white")

# Model 8
fd8 <- function(n, a, z1) {
  Z <- rnorm(n, 0, 1) # Z
  X <- rnorm(n, 0, 1) # X
  e_y <- rnorm(n, 0, 1) # error in Y
  Y <- X * a + Z * z1 + e_y # Y
  df <- data.frame(Z, X, Y)
  open <- coef(lm(Y ~ X, data = df))[2]
  controlled <- coef(lm(Y ~ X + Z, data = df))[2]
  return(c(open, controlled))
}

trisim8 <- data.frame(t(replicate(1000, fd8(100, .5, .5))))
names(trisim8) <- c("open", "controlled")

densop8 <- density(trisim8$open)
densco8 <- density(trisim8$controlled)

plot(NA, xlab = NA, ylab = "", 
     xlim = c(-.75, 1.3), 
     ylim = c(0, 4.5), 
     cex.lab = 1.3, yaxt = 'n')
polygon(densop8, col = mycol1) # Y ~ X
polygon(densco8, col = mycol3) # Y ~ X + Z
abline(v = 0.5, lty = 2)
legend("topleft", legend = c("Z Open", "Z Controlled"), 
       fill = c(mycol1, mycol3), cex = 1, 
       horiz = F, bty = T, inset = c(0.03, 0.15), box.col = "white")

plot(dagitty('dag {
             bb = "0, 0, 1, 1"
             X [pos = "0, 1"]
             Y [pos = "1, 1"]
             Z [pos = "1, 0"]
             Z -> Y
             X -> Y
             }'))

# Model 17
plot(dagitty('dag {
             bb = "0, 0, 1, 1"
             X [pos = "0, 0"]
             Y [pos = "1, 0"]
             Z [pos = ".5, .5"]
             Y -> Z
             X -> Y
             X -> Z
             }'))

fd17 <- function(n, a, y1, x1) {
  X <- rnorm(n, 0, 1) # Z
  e_y <- rnorm(n, 0, 1) # error in Y
  e_z <- rnorm(n, 0, 1) # error in Z
  Y <- X * a + e_y # Y
  Z <- X * x1 + Y * y1 + e_z # Z
  df <- data.frame(Z, X, Y)
  open <- coef(lm(Y ~ X, data = df))[2]
  controlled <- coef(lm(Y ~ X + Z, data = df))[2]
  return(c(open, controlled))
}

trisim17 <- data.frame(t(replicate(1000, fd17(100, .5, .5, .5))))
names(trisim17) <- c("open", "controlled")

densop17 <- density(trisim17$open)
densco17 <- density(trisim17$controlled)

plot(NA, xlab = NA, ylab = "", 
     xlim = c(-0.8, 1), 
     ylim = c(0, 4.5), 
     cex.lab = 1.3, yaxt = 'n')
polygon(densop17, col = mycol1) # Y ~ X
polygon(densco17, col = mycol3) # Y ~ X + Z
abline(v = 0.5, lty = 2)
legend("topleft", legend = c("Z Open", "Z Controlled"), 
       fill = c(mycol1, mycol3), cex = 1, 
       horiz = F, bty = T, inset = c(0.03, 0.15),
       border = T, box.col = "white")


