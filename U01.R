#############################
#####     REGRESSION    #####
#####    EXERCISES 01   #####
#############################

##### Exercise 4 #####

# X ~ N(2,9)

# (a) P(X<=0)
pnorm(0, mean=2, sd=3)

# (b) P(X<=-1)
pnorm(-1, mean=2, sd=3)

# (c) P(X>=5)
1 - pnorm(5, mean=2, sd=3)

# (d) P(-2<=X<=2)
pnorm(2, mean=2, sd=3) - pnorm(-2, mean=2, sd=3)

##### Exercise 5 #####

X = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
Y = c(18, 26, 33, 40, 46, 59, 72, 85, 97, 120, 141)

# (b) What are the R functions to calculate them? (Check with R!)

mean(X)
mean(Y)
var(X)
var(Y)
cov(X,Y)

# (c) Display the data in R using a scatterplot.

plot(X, Y
     , main="Exercise 5 Observations"
     , xlab="Speed (in km/h)"
     , ylab="Braking distance (in m)")

# (d) Calculate the correlation and check with R. What could we conclude from this value?

cor(X,Y)

# (e) Calculate the linear regression coefficients and display the line in the scatterplot.

linmod = lm(Y~X)
linmod$coefficients
abline(linmod)

##### Exercise 6 #####

# On Moodle you find the file Miete2003.csv. This is a sample of 2053 appartments in
# Munich from 2003 (Munich was already an expensive city at this time ...).

# (a) For which pairs of variables would it be useful to estimate a simple linear regression
#     model? (Choose at least two different examples. Explain which of the variables do you
#     consider the dependent and the independent one.)

# Example 1:
# independent var: wohnflaeche
# dependent var: nettomiete

# Example 2:
# independent var: baujahr
# dependent var: nettomiete

# Explanation: independent variable should be a potential reason for variation in the dependent variable

# (b) Load the data into R. If you don’t know what to do, check ?read.csv2.
#     (Extra task: Try also to load the original data into R!)

setwd("~/git-reps/regression")
df = read.csv2("U01_Miete2003.csv")
summary(df)

# (c) Estimate the model that you have chosen in (a), i.e. calculate the coefficients, draw
#     scatteplots and regression lines, determine R-squared.

plot(df$wohnflaeche, df$nettomiete)
mod1 = lm(nettomiete ~ wohnflaeche, data=df)
abline(mod1)
summary(mod1) # => Adjusted R-squared:  0.5003 

plot(df$baujahr, df$nettomiete)
mod2 = lm(nettomiete ~ baujahr, data=df)
abline(mod2)
summary(mod2) # => Adjusted R-squared:  0.001728 

# (d) Now consider nettomiete and wohnlage. Do you think it is useful to consider simple
#     linear regression here? Do you know any other technique(s) to analyse the relationship
#     between these two? (Maybe also a graphical technique?)

plot(df$wohnlage, df$nettomiete)
boxplot(nettomiete~wohnlage, data=df)

# (e) Again consider two variables: bad.kacheln and geh.kueche. Do you think there is
#     a relationship between these two? How could you analyse it? 

plot(df$geh.kueche, df$bad.kacheln)
table(df$geh.kueche, df$bad.kacheln)

# phi coefficient (psych library) to find the correlation between two binary variables:
library("psych")
phi(table(df$geh.kueche, df$bad.kacheln))

# alternatively a chi-squared test can be used on binary variabes:
chisq.test(table(df$geh.kueche, df$bad.kacheln))
# p-value < 0.05 
# => reject H0 => there is a significant relation between the two variables