library(ISLR)
library(class)
library(dplyr)

# ---- logistic regression
set.seed(123456)
hitters <- as.data.frame(Hitters[!is.na(Hitters$Salary), 
                         !names(Hitters) %in% c('League', 'NewLeague', 'Division')])
medianSalary = median(as.numeric(hitters$Salary))
hitters$ClassSalary = 0
hitters$ClassSalary[as.numeric(hitters$Salary) >= medianSalary] = 1
glm.fit = glm(factor(ClassSalary)~ Runs + Walks + Hits + 
                CHits + RBI + CAtBat + CHmRun + 
                CRuns + CRBI + CWalks + Errors,
              data = hitters,
              family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type="response")
glm.pred = rep(0, nrow(hitters))
glm.pred[glm.probs >= .5] = 1
mean(glm.pred == hitters$ClassSalary)
table(glm.pred, hitters$ClassSalary)


# ---- knn
# train.X <- cbind(hitters$Hits, hitters$Runs, hitters$Walks)
train.X <- cbind(hitters$Walks, hitters$PutOuts, hitters$Errors, hitters$Assists)
test.X <- cbind(hitters$Walks, hitters$PutOuts, hitters$Errors, hitters$Assists)
train.ClassSalary <- hitters$ClassSalary
set.seed(1)
knn.pred = knn(train.X, test.X, train.ClassSalary, k = 3)
table(knn.pred, train.ClassSalary)
mean(knn.pred == train.ClassSalary)

# Verificar importancia "empírica" de variables: 
# comparar el accuracy en conjunto de prueba usando todas las variables vs ir quitando algunas

# Regresion logistica
#Checar la "calibración" del modelo: 
# (1) calcular el score en el conjunto de prueba, 
# (2) dividir el conjunto de prueba por cuantiles del score, 
# (3) en cada grupo, calcular el promedio del score, 
# (4) en cada grupo calcular el porcentaje de elementos tales que Y = 1, 
# (5) comparar los dos puntos anteriores. Si la diferencia es "pequeña", el modelo está "bien calibrado".

# 1)
trainSize <- floor(0.75 * nrow(hitters))
set.seed(123)
partition <- sample(seq_len(nrow(hitters)), size = trainSize)
train <- as.data.frame(hitters[partition, ])
test <- as.data.frame(hitters[-partition, ])
glm.fit = glm(factor(ClassSalary)~ AtBat + Runs + Walks + CAtBat +
                CHits + Assists + HmRun + RBI + Years + CAtBat + CHmRun + 
                CRuns + CRBI + CWalks + PutOuts + Errors,
              data = train, family = binomial)

glm.probs = predict(glm.fit, test, type="response")
q1 = ntile(gml.probs, 4)
q2 = ntile(gml.probs, 4)
glm.pred = rep(0, nrow(hitters))
glm.pred[glm.probs >= .5] = 1
mean(glm.pred == hitters$ClassSalary)
table(glm.pred, hitters$ClassSalary)

# - En KNN:
# Estandarizar variables!
# hacer gráfica de K vs accuracy promedio (en los CV's) tanto en conj de entrenam como en conj de prueba (ver gráfica en pag 42 del libro)
train.X <- cbind(hitters$Walks, hitters$PutOuts, hitters$Errors, hitters$Assists)
test.X <- cbind(hitters$Walks, hitters$PutOuts, hitters$Errors, hitters$Assists)
train.ClassSalary <- hitters$ClassSalary
set.seed(1)
knn.pred = knn(train.X, test.X, train.ClassSalary, k = 3)
table(knn.pred, train.ClassSalary)
mean(knn.pred == train.ClassSalary)
for (i in 1:100) {
  set.seed(1)
  knn.pred = knn(train.X, test.X, train.ClassSalary, k = 3)
  table(knn.pred, train.ClassSalary)
  mean(knn.pred == train.ClassSalary)
}
