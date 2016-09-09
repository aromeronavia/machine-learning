install.packages("ISLR")
library(ISLR)
hitters <- Hitters[!is.na(hitters$Salary), ]
hitters$Salary = as.factor(hitters$Salary)
meanSalary = mean(hitters$Salary)
hitters$ClassSalary = 'low'
hitters$ClassSalary[hitters$Salary < meanSalary] = 'high'
glm.fit = glm(Salary ~ Runs + PutOuts + AtBat + Assists + Hits + Errors,
              data = hitters,
              family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type="response")
glm.pred = rep('low', nrow(hitters))
glm.pred[glm.probs > .5] = 'high'
mean(glm.pred == hitters$Salary)
table(glm.pred, hitters$Salary)
