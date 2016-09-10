library(ISLR)
library(class)

# ---- logistic regression
hitters <- Hitters[!is.na(Hitters$Salary), ]
hitters$Salary = hitters$Salary
meanSalary = mean(as.numeric(hitters$Salary))
hitters$ClassSalary = 'low'
hitters$ClassSalary[as.numeric(hitters$Salary) < meanSalary] = 'high'

glm.fit = glm(as.factor(ClassSalary) ~ Hits + Runs + Walks,
              data = hitters,
              family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type="response")
glm.pred = rep('low', nrow(hitters))
glm.pred[glm.probs > .5] = 'high'
mean(glm.pred == hitters$ClassSalary)
table(glm.pred, hitters$ClassSalary)

# ---- knn
train <- hitters[1: nrow(hitters) * 0.70, ]
train.X <- cbind(hitters$Hits, hitters$Runs, hitters$Walks)
test.X <- cbind(hitters$Hits, hitters$Runs, hitters$Walks)
train.ClassSalary <- hitters$ClassSalary
set.seed(1)
knn.pred = knn(train.X, test.X, train.ClassSalary, k = 3)
table(knn.pred, train.ClassSalary)
mean(knn.pred == train.ClassSalary)
