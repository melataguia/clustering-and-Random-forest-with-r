#Library 
library(factoextra)
library(cluster)
library(ggplot2)
library(datasets)


#charger notre dataset et l'observer 
load("nonsupervisee.RData")
View(data)
summary(data)

# verification des valeurs absentes
is.na(data)
str(data)


# Calcul de la matrice de distances
distance_matrix <- get_dist(data, method = "euclidean")  # Méthode par défaut : euclidienne

# Visualisation de la matrice de distances
fviz_dist(distance_matrix, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  labs(title = "Matrice de distances (méthode euclidienne)")


# Normalisation des données
data_normalized <- scale(data)


# Calcul de la matrice de distances après normalisation
distance_matrix_normalized <- get_dist(data_normalized, method = "euclidean")


# Visualisation de la matrice de distances normalisées
fviz_dist(distance_matrix_normalized, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  labs(title = "Matrice de distances (normalisées_m)")






# Appliquer K-means sur les données normalisées avec 2 clusters
set.seed(123)  # Pour des résultats reproductibles
kmeans_result_normalized <- kmeans(data_normalized, centers = 2, nstart = 25)

# Explorer la structure de l'objet résultant
str(kmeans_result_normalized)

# Visualiser les clusters obtenus avec les données normalisées
fviz_cluster(kmeans_result_normalized, data = data_normalized, geom = "point", main = "K-means avec 2 clusters sur données normalisées")



# Appliquer K-means avec 3 clusters sur les données normalisées
kmeans_result_3_normalized <- kmeans(data_normalized, centers = 3, nstart = 25)
fviz_cluster(kmeans_result_3_normalized, data = data_normalized, geom = "point", main = "K-means avec 3 clusters sur données normalisées")

# Appliquer K-means avec 4 clusters sur les données normalisées
kmeans_result_4_normalized <- kmeans(data_normalized, centers = 4, nstart = 25)
fviz_cluster(kmeans_result_4_normalized, data = data_normalized, geom = "point", main = "K-means avec 4 clusters sur données normalisées")



# Appliquer K-means avec 6 clusters et nstart = 25 sur les données normalisées
kmeans_result_6_normalized <- kmeans(data_normalized, centers = 6, nstart = 25)
fviz_cluster(kmeans_result_6_normalized, data = data_normalized, geom = "point", main = "K-means avec 6 clusters (stabilisé) sur données normalisées")


