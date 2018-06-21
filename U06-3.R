# Regression Worksheet 6
# Exercise 3 

# Important: Logistic regression: anova chi-square test vs. significance of coefficients (anova() vs summary() in R)
# https://stats.stackexchange.com/questions/59879/logistic-regression-anova-chi-square-test-vs-significance-of-coefficients-ano)

library(AER)
data(Affairs)

Affairs$affair=as.factor(as.numeric(Affairs$affairs>0))
table(Affairs$affair)

Affairs = Affairs[ ,!(names(Affairs) %in% c("affairs"))] # drop column

with(Affairs, spineplot(gender,affair))
with(Affairs, spineplot(age,affair))
with(Affairs, spineplot(yearsmarried,affair))

summary(Affairs)
glm1 = glm(affair~age, data=Affairs, family=binomial()) 
summary(glm1) # NOT significant
glm2 = glm(affair~yearsmarried, data=Affairs, family=binomial())
summary(glm2) # significant
glm3 = glm(affair~yearsmarried+children, data=Affairs, family=binomial())
summary(glm3) # only significant at 0.05 level
glm4 = glm(affair~yearsmarried+religiousness, data=Affairs, family=binomial())
summary(glm4) # both significant
glm5 = glm(affair~yearsmarried+religiousness+education, data=Affairs, family=binomial())
summary(glm5) # education NOT significant
glm6 = glm(affair~yearsmarried+religiousness+occupation, data=Affairs, family=binomial())
summary(glm6) # occupation NOT significant
glm7 = glm(affair~yearsmarried+religiousness+rating, data=Affairs, family=binomial())
summary(glm7) # all 3 siginificant
glm8 = glm(affair~yearsmarried+religiousness+rating+age, data=Affairs, family=binomial())
summary(glm8)
glmA = glm(affair~., data=Affairs, family=binomial())
summary(glmA)

# if not setting test="Chisq" parameter, anova just returns a analysis of deviance table for GLMs
anova(glm2, glm4, glm7, glm7, glmA)

# single model test
anova(glm4, test="Chisq")
anova(glm7, test="Chisq")
anova(glm8, test="Chisq")
anova(glmA, test="Chisq")
summary(glmA)

# model comparison
anova(glm2, glm4, test="Chisq")
anova(glm4, glm7, test="Chisq")
anova(glm4, glm7, glm8, glmA, test="Chisq") # glmA is not significantly better than glm8

# => glm8 seems to be the best

require(MASS)
stepAll <- stepAIC(glmA)
stepAll$anova # display results 
summary(stepAll)

# => confirms glm8 to be best