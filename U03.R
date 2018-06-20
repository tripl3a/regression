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

n = 10
x = rnorm(n)
error = rnorm(n, sd=sqrt(0.25))
y = 1 + 2*x + x^2 + error

lm1 = lm(y ~ x)
lm2 = lm(y ~ x + I(x^2))
lm3 = lm(y ~ x + I(x^2) + I(x^3))

### (a) Construct the matrix X for all 3 models (the matrix X is called “design matrix”).
# according to p. 18 in the script

# manually construct the design matrices
X1 = cbind(rep(1,n),x); colnames(X1)=c("intercept","x"); X1
X2 = cbind(rep(1,n),x,x^2); colnames(X2)=c("intercept","x","x^2"); X2
X3 = cbind(rep(1,n),x,x^2,x^3); colnames(X3)=c("intercept","x","x^2","x^3"); X3

# automatically get design matrices from LMs
model.matrix(lm1)
model.matrix(lm2)
model.matrix(lm3)

# check equality (should be 0)
sum(X1-model.matrix(lm1))
sum(X2-model.matrix(lm2))
sum(X3-model.matrix(lm3))

### (b) Check, if X and t(X)%*%X are of full rank.

require(Matrix)
rankMatrix(X1)[1]
rankMatrix(X2)[1]
rankMatrix(X3)[1]
rankMatrix(t(X1)%*%X1)[1]
rankMatrix(t(X2)%*%X2)[1]
rankMatrix(t(X3)%*%X3)[1]

# => all are full rank

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

sum(diag(P1))
sum(diag(P2))
sum(diag(P3))

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

##################
### Exercise 4 ###
##################

require(AER)
data(CPS1985)
?CPS1985

# Check the contents of the dataset (?CPS1985) and consider the variables 
# Y = wage, X1 = education and X2 = experience. Estimate the following models:
# regression of log(Y) on X1,
# regression of log(Y) on X1 and X2,
# regression of log(Y) on X1, X2 and X2^2.
#------------------------------------------

plot(CPS1985$education, CPS1985$wage)
plot(CPS1985$education, log(CPS1985$wage))
plot(CPS1985$experience, CPS1985$wage)
plot(CPS1985$experience, log(CPS1985$wage))

mod1 = lm(log(wage)~education, data=CPS1985); summary(mod1)
mod2 = lm(log(wage)~education+experience, data=CPS1985); summary(mod2)
mod3 = lm(log(wage)~education+experience+I(experience^2), data=CPS1985); summary(mod3)

# Interprete the estimated coefficients. Do they make sense? 
# (Plot a graph for the quadratic part of the 3rd model.)
#------------------------------------------

coef(mod1)
coef(mod2)
coef(mod3)

plot(CPS1985$experience, coef(mod3)[3] * CPS1985$experience + coef(mod3)[4] * CPS1985$experience^2)

# Analyse and compare the residuals for all 3 model fits (e.g. by comparing R2 
# and by residual plots or by boxplots). What do you conclude with respect to 
# which of the 3 models seems to be appropriate?
#------------------------------------------

summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
summary(mod3)$adj.r.squared

par(mfrow=c(1,3))
plot(predict(mod1),residuals(mod1))
plot(predict(mod2),residuals(mod2))
plot(predict(mod3),residuals(mod3))
par(mfrow=c(1,1))
boxplot(residuals(mod1),residuals(mod2),residuals(mod3))

anova(mod1,mod2,mod3)
