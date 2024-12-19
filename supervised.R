library(tidyverse)
library(mlbench)  
library(rpart)
library(rpart.plot)
library(randomForest)
library(randomForestExplainer)
library(ggplot2)

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
predictions1 = ifelse(predictions1proba[, 1] < 0.5, "R", "M") 

# Afficher la matrice de confusion
table(dataTest$Class, predictions1)

# 3. Entraînement d'un modèle d'arbre de décision avec élargissement des paramètres ----
modele2 = rpart(target ~ ., data = dataTrain, minsplit = 1, minbucket = 1, cp = 0.01)
rpart.plot(modele2)

predictions2proba = predict(modele2, newdata = dataTest)
predictions2 = ifelse(predictions2proba[, 1] < 0.5, "R", "M")
table(dataTest$target, predictions2)

# 4. Entraînement d'un modèle Random Forest ----
modele10 = randomForest(target ~ ., data = dataTrain)
plot(modele10)  # Visualisation de l'importance des variables
predictions10 = predict(modele10, newdata = dataTest)
table(dataTest$target, predictions10)

# 5. Entraînement d'un Random Forest avec mtry ajusté ----
modele11 = randomForest(target ~ ., data = dataTrain, mtry = 30)
plot(modele11)
predictions11 = predict(modele11, newdata = dataTest)
table(dataTest$target, predictions11)

# 6. Interprétation du modèle Random Forest ----
modele20 = randomForest(target ~ ., data = dataTrain, localImp = TRUE)

# Visualisation de la distribution des profondeurs de l'arbre
plot_min_depth_distribution(modele20)

# Visualisation de l'importance des variables
plot_multi_way_importance(modele20)

# Visualisation des interactions entre variables
plot_predict_interaction(modele20, data = dataTrain, variable1 = "V12", variable2 = "V11")

# Plot d'un partial plot pour la variable "V1"
partialPlot(modele20, x.var = "V1", pred.data = dataTrain)
