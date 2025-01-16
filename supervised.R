library(tidyverse)
library(mlbench)  
library(rpart)
library(rpart.plot)
library(randomForest)
library(randomForestExplainer)
library(ggplot2)
library(randomForestExplainer)
library(GGally)
library(caret)
# 1. Charger et observer les données ----
load("supervisee.RData")  # Charger les données

View(data)  # Observer les données
summary(data)  # Voir le résumé des données

# Vérifier la structure des données
str(data)


set.seed(123)

# Diviser les données en ensemble d'entraînement et de test
indTrain = sample(1:(dim(data)[1]), 850)
dataTrain = data[indTrain, ]
dataTest = data[-indTrain, ]

# 2. Entraînement d'un modèle d'arbre de décision ----
modele1 = rpart(target ~ ., data = dataTrain)

# Visualisation de l'arbre de décision
rpart.plot(modele1)

# Prédictions avec le modèle
predictions1proba = predict(modele1, newdata = dataTest)
predictions1 = ifelse(predictions1proba[, 1] < 0.5, "1", "2") 

# Afficher la matrice de confusion
table(dataTest$target, predictions1)
conf_matrix <- table(dataTest$target, predictions1)
print(conf_matrix)

# Extraire les éléments de la matrice de confusion
TP_2 <- conf_matrix[1, "2"]
FN_2 <- conf_matrix[1, "1"]
FP_2 <- conf_matrix[2, "2"]
TN_2 <- conf_matrix[2, "1"]

# Calculer les métriques
accuracy <- (TP_2 + TN_2) / sum(conf_matrix)
precision_2 <- TP_2 / (TP_2 + FP_2)
precision_1 <- TN_2 / (TN_2 + FN_2)
recall_2 <- TP_2 / (TP_2 + FN_2)
recall_1 <- TN_2 / (TN_2 + FP_2)
F1_2 <- 2 * (precision_2 * recall_2) / (precision_2 + recall_2)
F1_1 <- 2 * (precision_1 * recall_1) / (precision_1 + recall_1)

# Afficher les résultats
cat("Exactitude (Accuracy): ", accuracy, "\n")
cat("Précision pour 2: ", precision_2, "\n")
cat("Précision pour 1: ", precision_1, "\n")
cat("Rappel pour 2: ", recall_2, "\n")
cat("Rappel pour 1: ", recall_1, "\n")
cat("Score F1 pour 2: ", F1_2, "\n")
cat("Score F1 pour 1: ", F1_1, "\n")

# 3. Entraînement d'un modèle d'arbre de décision avec élargissement des paramètres ----
modele2 = rpart(target ~ ., data = dataTrain, minsplit = 1, minbucket = 1, cp = 0.01)
rpart.plot(modele2)

predictions2proba = predict(modele2, newdata = dataTest)
predictions2 = ifelse(predictions2proba[, 1] < 0.5, "1", "2")
table(dataTest$target, predictions2)
# Afficher la matrice de confusion
conf_matrix2 <- table(dataTest$target, predictions2)
print(conf_matrix2)

# Extraire les éléments de la matrice de confusion
TP_2 <- conf_matrix2[1, "2"]
FN_2 <- conf_matrix2[1, "1"]
FP_2 <- conf_matrix2[2, "2"]
TN_2 <- conf_matrix2[2, "1"]

# Calculer les métriques
accuracy <- (TP_2 + TN_2) / sum(conf_matrix2)
precision_2 <- TP_2 / (TP_2 + FP_2)
precision_1 <- TN_2 / (TN_2 + FN_2)
recall_2 <- TP_2 / (TP_2 + FN_2)
recall_1 <- TN_2 / (TN_2 + FP_2)
F1_2 <- 2 * (precision_2 * recall_2) / (precision_2 + recall_2)
F1_1 <- 2 * (precision_1 * recall_1) / (precision_1 + recall_1)

# Afficher les résultats
cat("Exactitude (Accuracy): ", accuracy, "\n")
cat("Précision pour 2: ", precision_2, "\n")
cat("Précision pour 1: ", precision_1, "\n")
cat("Rappel pour 2: ", recall_2, "\n")
cat("Rappel pour 1: ", recall_1, "\n")
cat("Score F1 pour 2: ", F1_2, "\n")
cat("Score F1 pour 1: ", F1_1, "\n")







# 4. Entraînement d'un modèle Random Forest ----
modele10 = randomForest(target ~ ., data = dataTrain)
plot(modele10)  # Visualisation de l'importance des variables
predictions10 = predict(modele10, newdata = dataTest)
table(dataTest$target, predictions10)

conf_matrix10 <- table(dataTest$target, predictions10)
print(conf_matrix10)

# Extraire les éléments de la matrice de confusion
TP_2 <- conf_matrix10[1, "2"]
FN_2 <- conf_matrix10[1, "1"]
FP_2 <- conf_matrix10[2, "2"]
TN_2 <- conf_matrix10[2, "1"]

# Calculer les métriques
accuracy <- (TP_2 + TN_2) / sum(conf_matrix10)
precision_2 <- TP_2 / (TP_2 + FP_2)
precision_1 <- TN_2 / (TN_2 + FN_2)
recall_2 <- TP_2 / (TP_2 + FN_2)
recall_1 <- TN_2 / (TN_2 + FP_2)
F1_2 <- 2 * (precision_2 * recall_2) / (precision_2 + recall_2)
F1_1 <- 2 * (precision_1 * recall_1) / (precision_1 + recall_1)

# Afficher les résultats
cat("Exactitude (Accuracy): ", accuracy, "\n")
cat("Précision pour 2: ", precision_2, "\n")
cat("Précision pour 1: ", precision_1, "\n")
cat("Rappel pour 2: ", recall_2, "\n")
cat("Rappel pour 1: ", recall_1, "\n")
cat("Score F1 pour 2: ", F1_2, "\n")
cat("Score F1 pour 1: ", F1_1, "\n")

# 5. Entraînement d'un Random Forest avec mtry ajusté ----
modele11 = randomForest(target ~ ., data = dataTrain, mtry = 12)
plot(modele11)
predictions11 = predict(modele11, newdata = dataTest)
table(dataTest$target, predictions11)

conf_matrix11 <- table(dataTest$target, predictions11)
print(conf_matrix11)

# Extraire les éléments de la matrice de confusion
TP_2 <- conf_matrix11[1, "2"]
FN_2 <- conf_matrix11[1, "1"]
FP_2 <- conf_matrix11[2, "2"]
TN_2 <- conf_matrix11[2, "1"]

# Calculer les métriques
accuracy <- (TP_2 + TN_2) / sum(conf_matrix11)
precision_2 <- TP_2 / (TP_2 + FP_2)
precision_1 <- TN_2 / (TN_2 + FN_2)
recall_2 <- TP_2 / (TP_2 + FN_2)
recall_1 <- TN_2 / (TN_2 + FP_2)
F1_2 <- 2 * (precision_2 * recall_2) / (precision_2 + recall_2)
F1_1 <- 2 * (precision_1 * recall_1) / (precision_1 + recall_1)

# Afficher les résultats
cat("Exactitude (Accuracy): ", accuracy, "\n")
cat("Précision pour 2: ", precision_2, "\n")
cat("Précision pour 1: ", precision_1, "\n")
cat("Rappel pour 2: ", recall_2, "\n")
cat("Rappel pour 1: ", recall_1, "\n")
cat("Score F1 pour 2: ", F1_2, "\n")
cat("Score F1 pour 1: ", F1_1, "\n")



# 6. Interprétation du modèle Random Forest ----
modele20 = randomForest(target ~ ., data = dataTrain, localImp = TRUE)

# Visualisation de la distribution des profondeurs de l'arbre
plot_min_depth_distribution(modele20)

# Visualisation de l'importance des variables
plot_multi_way_importance(modele20)
# Convertion de "var1" en numerique
#dataTrain$var1_numeric <- as.numeric(as.factor(dataTrain$var1))
#dataTest$var1_numeric <- as.numeric(as.factor(dataTest$var1))

# Visualisation des interactions entre variables
plot_predict_interaction(modele20, data = dataTrain, variable1 = "var5", variable2 = "var12")

# Plot d'un partial plot pour la variable "V1"
#partialPlot(modele20, pred.data = dataTrain, x.var = "var1")
partialPlot(modele20, pred.data = dataTrain, x.var = "var2")
partialPlot(modele20, pred.data = dataTrain, x.var = "var3")
partialPlot(modele20, pred.data = dataTrain, x.var = "var4")
partialPlot(modele20, pred.data = dataTrain, x.var = "var5")
partialPlot(modele20, pred.data = dataTrain, x.var = "var6")
partialPlot(modele20, pred.data = dataTrain, x.var = "var7")
partialPlot(modele20, pred.data = dataTrain, x.var = "var8")
partialPlot(modele20, pred.data = dataTrain, x.var = "var9")
partialPlot(modele20, pred.data = dataTrain, x.var = "var10")
partialPlot(modele20, pred.data = dataTrain, x.var = "var11")
partialPlot(modele20, pred.data = dataTrain, x.var = "var12")







# 2. Entraînement d'un modèle d'arbre de décision avec l'entropie comme critère de division ----
modele1 = rpart(target ~ ., data = dataTrain, parms = list(split = "information"))

# Visualisation de l'arbre de décision
rpart.plot(modele1)

# Prédictions avec le modèle
predictions1proba = predict(modele1, newdata = dataTest)
predictions1 = ifelse(predictions1proba[, 1] < 0.5, "R", "M") 

# Afficher la matrice de confusion
table(dataTest$target, predictions1)
print(table)