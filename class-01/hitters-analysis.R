library(ISLR)
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

train <- hitters[1: nrow(hitters) / 2, ]
test <- hitters[(nrow(hitters) / 2) + 1: nrow(hitters), ]
knn.reg(train, test = NULL, y, k = 3, use.all = FALSE, 
        algorithm=c("VR", "brute", "kd_tree", "cover_tree"))
