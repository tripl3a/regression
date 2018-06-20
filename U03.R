# Regression - Master Data Science - ST 2018
# Exercises 3

##################
### Exercise 1 ###
##################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dat = read.csv2("Miete2003.csv")
summary(dat)

lm1 = lm(dat$nettomiete~dat$wohnflaeche)
lm2 = lm(dat$nettomiete~dat$zimmerzahl)

# show diagnostic plots
par(mfrow=c(2,2))
plot(lm1)
plot(lm2)
par(mfrow=c(1,1))

plot(predict(lm1), residuals(lm1)) # there's heteroscedasticity
plot(predict(lm2), residuals(lm2))

1/dim(dat)[1] * sum(predict(lm1))
mean(dat$nettomiete)
mean(predict(lm1))
mean(predict(lm2))

round(1/dim(dat)[1] * sum(residuals(lm1)),10)
mean(residuals(lm1))
mean(residuals(lm2))

# as there's heteroscedasticity in lm1 let's try a model with log(Y) or sqrt(Y) [James et al. p.95]

lm.log = lm(dat$nettomiete~I(log(dat$wohnflaeche)))
lm.sqrt = lm(dat$nettomiete~I(sqrt(dat$wohnflaeche)))

plot(predict(lm.log), residuals(lm.log))
plot(predict(lm.sqrt), residuals(lm.sqrt))

##################
### Exercise 2 ###
##################

n = 500
x = rnorm(n)
error = rnorm(n, sd=sqrt(0.25))
y = 1 + 2*x + x^2 + error

lm1 = lm(y ~ x)
lm2 = lm(y ~ x + I(x^2))
lm3 = lm(y ~ x + I(x^2) + I(x^3))

summary(lm1)$r.squared
summary(lm2)$r.squared
summary(lm3)$r.squared

summary(lm1)$adj.r.squared
summary(lm2)$adj.r.squared
summary(lm3)$adj.r.squared

# Residual Sum of Squares (RSS)
sum(residuals(lm1)^2); deviance(lm1)
sum(residuals(lm2)^2); deviance(lm2)
sum(residuals(lm3)^2); deviance(lm3)

# residual plots    
boxplot(residuals(lm1), residuals(lm2), residuals(lm3))
plot(predict(lm1), residuals(lm1)) # clear pattern
plot(predict(lm2), residuals(lm2))
plot(predict(lm3), residuals(lm3))

# regression line plots
plot(x,y)
abline(lm1, col="red")
curve(coef(lm2)[1] + x*coef(lm2)[2] + x^2*coef(lm2)[3], add=T, col="green", lwd=2)
curve(coef(lm3)[1] + x*coef(lm3)[2] + x^2*coef(lm3)[3] + x^3*coef(lm3)[4], add=T, col="blue", lty=3)

# quadratic model is appropriate as it yields the same high R² value as the cubic model.
# the simpler model is to be preferred, but let's check with ANOVA.

anova(lm1, lm2)
anova(lm2, lm3)
anova(lm1, lm2, lm3)


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



