# Regression
# Exercises 2

##################
### Exercise 1 ###
##################

sp <- data.frame(price=c(12.50, 10.00, 9.95, 11.50, 12.00, 10.00, 8.00, 9.00, 9.50, 12.50),
                 sales=c(1585, 1819, 1647, 1496, 921, 1278, 1810, 1987, 1612, 1413))

# (a) Plot the data. How would you describe this relationship? What sign of the correlation
# do you expect?

plot(sp)

# (b) Calculate (using R) the estimated coefficients β0 and β1 for a simple linear regression
# to model sales in dependence of prices. Add the regression line to your scatterplot.

beta1 <- cov(sp$price, sp$sales) / var(sp$price)
beta0 <- mean(sp$sales) - beta1 * mean(sp$price)
abline(beta0, beta1)

lin.mod <- lm(sales~price, sp)
print(lin.mod)
summary(lin.mod)

# (c) Is there any connection between r.xy and β1?

beta1
cor(sp$sales, sp$price) # negative linear correlation 
# they are proportional and have the same sign

beta1 = -121.59 * sd(sp$price)/sd(sd$sales)

# (d) Using the estimated linear model: Predict the sales for a price of 13 Euro.

beta0 + beta1 * 13
predict(lin.mod, data.frame(price=c(13)))

# (e) Discuss: What happens to the estimated coefficients if the prices are converted into another currency? 
# How do the the correlation and the coefficient of determination change?

# the correlation is invariant to a linear transformation of it's predictor

sp$price.usd <- sp$price * 1.23723
lin.mod.usd <- lm(sales~price.usd, sp)
print(lin.mod)
print(lin.mod.usd) # the slope has changed proportional to the exchange rate
summary(lin.mod)
summary(lin.mod.usd)
cor(sp$price, sp$sales)
cor(sp$price.usd, sp$sales)

# if you would have used standardized data there would have been no change at all 

##################
### Exercise 3 ###
##################

# (a) Write a R function that calculates the polynomial
# p(x) = c0 + c1*x + c2*x^2 + ... + cp*x^p
# where the given input is the vector (c0, c1, c2, ..., cp). 
# The polynomial shall be calculated on a grid of x-values in [−5;5].

calc.poly <- function(c, iv=c(-5,5), show.plot=F, color="black", line.style=1, title="Polynomial Plot") {
  xgrid=seq(-5,5,by=0.1)
  result = rep(c[1],length(xgrid))
  for (i in 2:length(c)) {
    result = result + c[i]*xgrid^(i-1)
  }
  if (show.plot) { plot(xgrid, result, col=color, type="l", lty=line.style, main=title) }
  return(result)
}

v <- c(10,1,-2,4)
res <- calc.poly(v); res

# (b) Add an optional way to let the user modify the interval for the x-values.

res <- calc.poly(v,c(-10,10)); res
res <- calc.poly(v,c(-20,20)); res

# (c) Add another option to graph the function. The user should also have the possibility to
# change the color, linestyle and title of the graph.

res <- calc.poly(v,c(-50,50)); res
res <- calc.poly(v,c(-50,50), show.plot=TRUE); res
res <- calc.poly(v,c(-50,50), show.plot=TRUE, color="red"); res
res <- calc.poly(v,c(-50,50), show.plot=TRUE, line.style=2); res
res <- calc.poly(v,c(-50,50), show.plot=TRUE, title="custom plot title"); res

