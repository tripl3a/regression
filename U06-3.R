# exercise 3 from worksheet 6

library(AER)
data(Affairs)

# a)
Y<-rep(0, length(Affairs$affairs))
data<-cbind(Affairs, Y)
data[affairs!=0,]$Y<-1

## or alternatively
Affairs$Y <- as.numeric( Affairs$affairs > 0 )
data <- Affairs

with(data, spineplot(factor(gender), factor(Y)))
with(data, spineplot(factor(age), factor(Y)))
with(data, spineplot(factor(religiousness), factor(Y)))
with(data, spineplot(factor(education), factor(Y)))

with(data, spineplot(factor(yearsmarried), factor(Y)))
with(data, spineplot(factor(Y)~children))
with(data, spineplot(factor(rating), factor(Y)))
with(data, spineplot(factor(occupation), factor(Y)))

# b)
glm1<-glm(Y~gender, data=data, family=binomial())
summary(glm1)
glm2<-glm(Y~yearsmarried, data=data, family=binomial())
summary(glm2)
glm3<-glm(Y~yearsmarried+children, data=data, family=binomial())
summary(glm3)
glm4<-glm(Y~yearsmarried+religiousness, data=data, family=binomial())
summary(glm4)
glm5<-glm(Y~yearsmarried+religiousness+gender, data=data, family=binomial())
summary(glm5)
glm6<-glm(Y~yearsmarried+religiousness+age, data=data, family=binomial())
summary(glm6)
glm7<-glm(Y~yearsmarried+religiousness+age+rating, data=data, family=binomial())
summary(glm7)
glm8<-glm(Y~yearsmarried+religiousness+age+rating+children, data=data, family=binomial())
summary(glm8)
glm9<-glm(Y~yearsmarried+religiousness+age+rating+education, data=data, family=binomial())
summary(glm9)
glm10<-glm(Y~yearsmarried+religiousness+age+rating+gender, data=data, family=binomial())
summary(glm10)
glm11<-glm(Y~yearsmarried+religiousness+age+rating+gender+occupation+children+education, data=data, family=binomial())
summary(glm11)

anova(glm2,  glm10, test="Chisq")
anova(glm3,  glm4,  test="Chisq")
anova(glm4,  glm7,  test="Chisq")
anova(glm7,  glm8,  test="Chisq")
anova(glm7,  glm9,  test="Chisq")
anova(glm7,  glm10, test="Chisq")
anova(glm10, glm11, test="Chisq")

## best model -> glm10

require(MASS)
step11 <- stepAIC(glm11)
step11$anova # display results 
summary(step11)
## stepAIC also finds glm10 to be the best model

