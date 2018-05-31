# Regression - Master Data Science - ST 2018
# Exercises 3

##################
### Exercise 1 ###
##################

setwd("~/git-reps/regression/")

df <- read.csv2("Miete2003.csv")

lm1.out <- lm(df$nettomiete ~ df$wohnflaeche)

plot(lm1.out$fitted.values, lm1.out$residuals, asp=TRUE)
plot(lm1.out$residuals, lm1.out$fitted.values, asp=TRUE)

1/dim(df)[1] * sum(lm1.out$fitted.values) 
mean(df$nettomiete)

round(1/dim(df)[1] * sum(lm1.out$residuals),10)
round(mean(lm1.out$residuals),10)

##################
### Exercise 2 ###
##################

set.seed(666)
x <- rnorm(500)
e <- rnorm(500, mean=0, sd=0.25)
y <- 1 + 2*x + x^2 + e

lm <- lm(y~x)
lm2 <- lm(y ~ x + I(x^2))
lm3 <- lm(y ~ x + I(x^2) + I(x^3))

summary(lm)$adj.r.squared
summary(lm2)$adj.r.squared
summary(lm3)$adj.r.squared

# Residual Sum of Squares (RSS)
deviance(lm)
deviance(lm2)
deviance(lm3)

par(mfrow=c(2,3))

# regression line plots
plot(x,y, pch=19, main="linear")
abline(lm,col="blue")
plot(x,y, pch=19, asp=TRUE, main="quadratic")
curve(1 + 2*x + x^2, col="blue", add=TRUE)
plot(x,y, pch=19, asp=TRUE, main="cubic")
curve(1 + 2*x + x^2 + x^3, col="blue", add=TRUE)

# residual plots    
plot(x,residuals(lm), pch=19, col="red", ylim=c(-1,1), main="linear")
plot(x, residuals(lm2), pch=19, col="red", ylim=c(-1,1), asp=TRUE, main="quadratic")
plot(x, residuals(lm3), pch=19, col="red", ylim=c(-1,1), asp=TRUE, main="cubic")

par(mfrow=c(1,1))

## ggplot experiment

require(ggplot2)

qplot(x,y)

df <- data.frame(x,e,y)

ggplot(df,aes(x,y)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
  

##################
### Exercise 3 ###
##################

set.seed(666)
xsamp <- sample(x, 10)

### (a) Construct the matrix X for all 3 models (the matrix X is called “design matrix”).
# according to p. 18 in the script

X1 <- cbind(rep(1,10), xsamp); head(X1)
X2 <- cbind(rep(1,10), xsamp, xsamp^2); head(X2)
X3 <- cbind(rep(1,10), xsamp, xsamp^2, xsamp^3); head(X3)

### (b) Check, if X and X.transposed_X are of full rank.

require(Matrix)
rankMatrix(X1)[1]
rankMatrix(X2)[1]
rankMatrix(X3)[1]
# => all 3 are full rank

### (c) Do also calculate the “hat matrix” for each of the 3 models.
### Determine (for each model) the trace and the eigenvalues of P.

# the standard R function for matrix inverse is solve()
round(solve((t(X1) %*% X1)),5)
library(matlib)
round(inv((t(X1) %*% X1)),5)


### Matrix–Vector Notation (Cont’d) (p. 20) ###
###  find the solution (LSE): ###
P1 <- X1 %*% inv(t(X1) %*% X1) %*% t(X1)
P2 <- X2 %*% inv(t(X2) %*% X2) %*% t(X2)
P3 <- X3 %*% inv(t(X3) %*% X3) %*% t(X3)
dim(P1); dim(P2); dim(P3)

# the trace of a (square) matrix is just the sum of the diagonal elements
library(psych)
tr(P1)
tr(P2)
tr(P3)

P1.eig.val <- eigen(P1)$values
P2.eig.val <- eigen(P2)$values
P3.eig.val <- eigen(P3)$values

# The trace of a matrix is the sum of the (complex) eigenvalues, 
# and it is invariant with respect to a change of basis. 

# (d) By I we denote the identity matrix (a matrix with diagonal elements 1 and 0 otherwise).
# Show that it holds: P^2 = P and (I-P)^2 = I-P

(P1%*%P1)-P1
all.equal(round(P1%*%P1,5), round(P1,5))

ImP <- diag(10)-P1
all.equal(round(ImP%*%ImP,5), round(diag(10)-P1,5))



