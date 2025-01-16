#Library 
library(factoextra)
library(cluster)
library(ggplot2)
library(datasets)
library(dplyr)
library(rgl)




#charger notre dataset et l'observer 
load("nonsupervisee.RData")
View(data)
summary(data)

# verification des valeurs absentes
is.na(data)
sum(is.na(data))
str(data)


# Calcul de la matrice de distances
distance <- get_dist(data, method = "euclidean")

# Visualisation de la matrice de distances
fviz_dist(distance) +
  labs(title = "Matrice de distances (méthode euclidienne)")


# Normalisation des données
data_normalized <- scale(data)
summary(data_normalized)

# Calcul de la matrice de distances après normalisation
distance2<- get_dist(data_normalized, method = "euclidean")


# Visualisation de la matrice de distances normalisées
#fviz_dist(distance2) +
  #labs(title = "Matrice de distances (normalisées_m)")





# 3. Nombre optimal de clusters : méthode du coude ## avec fonction
fviz_nbclust(data_normalized, FUNcluster = kmeans, method = "wss", k.max = 15)



# Créer un vecteur pour les valeurs de K à tester (par exemple de 1 à 16)
K_values <- 1:16

# Créer un vecteur pour stocker les valeurs de tot.withinss (somme des distances intra-clusters)
dintra_values <- numeric(length(K_values))

# Boucle pour appliquer K-means et extraire la valeur de tot.withinss
for (i in 1:length(K_values)) {
  kmeans_result <- kmeans(data_normalized, centers = K_values[i], nstart = 25)
  dintra_values[i] <- kmeans_result$tot.withinss
}

# Visualiser les résultats (somme des distances intra-clusters en fonction de K)
plot(K_values, dintra_values, type = "b", pch = 19, 
     xlab = "Nombre de clusters ", ylab = "Distances intra-cluster",
     main = "Methode du coude pour notre Clustering", xaxt = "n")  # Empêcher la génération automatique de l'axe x

# Ajouter un axe x avec des valeurs de 1 à 16
axis(1, at = K_values, labels = K_values)




## Methode silhouette 
# Créer un vecteur pour les valeurs de K à tester (par exemple de 2 à 15)
K_values <- 2:15

# Créer un vecteur pour stocker les scores moyens de silhouette
silhouette_scores <- numeric(length(K_values))

# Boucle pour appliquer K-means et calculer le score moyen de silhouette
for (i in 1:length(K_values)) {
  # Appliquer K-means avec un nombre de clusters spécifique
  kmeans_result <- kmeans(data_normalized, centers = K_values[i], nstart = 25)
  
  # Calcul de la matrice de distance entre les points
  distance_matrix <- get_dist(data_normalized, method = "euclidean")
  
  # Calcul des scores de silhouette
  silhouette_result <- silhouette(kmeans_result$cluster, distance_matrix)
  
  # Calcul de la moyenne des scores de silhouette
  silhouette_scores[i] <- mean(silhouette_result[, 3])
}

# Visualiser les résultats (score de silhouette moyen en fonction de K)
plot(K_values, silhouette_scores, type = "b", pch = 19, 
     xlab = "Nombre de clusters", ylab = "Silhouette Moyenne",
     main = "Méthode de la silhouette")











# Appliquer K-means sur les données normalisées avec 2 clusters
set.seed(123)  # Pour des résultats reproductibles
kmeans_result_normalized <- kmeans(data_normalized, centers = 2, nstart = 25)

# Explorer la structure de l'objet résultant
str(kmeans_result_normalized)
fviz_cluster(kmeans_result_normalized, data = data_normalized,geom = "point", main = "K-means avec 2 clusters sur données normalisées")

# Appliquer K-means avec 3 clusters sur les données normalisées
kmeans_result_3_normalized <- kmeans(data_normalized, centers = 3, nstart = 25)
fviz_cluster(kmeans_result_3_normalized, data = data_normalized, geom = "point", main = "K-means avec 3 clusters sur données normalisées")

# Appliquer K-means avec 4 clusters sur les données normalisées
kmeans_result_4_normalized <- kmeans(data_normalized, centers = 4, nstart = 100)
fviz_cluster(kmeans_result_4_normalized, data = data_normalized, geom = "point", main = "K-means avec 4 clusters sur données normalisées")







# Résultat K-means avec 3 clusters et introductions des centroides 
kmeans_result_3_normalized <- kmeans(data_normalized, centers = 3, nstart = 1000)

# Récupération des coordonnées des centroïdes
centroids <- as.data.frame(kmeans_result_3_normalized$centers)

# Renommer les colonnes pour correspondre aux axes du graphique
colnames(centroids) <- colnames(data_normalized)

# Calcul des distances entre les centroïdes
distances_between_centroids <- dist(centroids)
# Afficher les distances 
print(distances_between_centroids)
# Convertir les distances en une matrice complète 
dist_matrix <- as.matrix(distances_between_centroids) 


fviz_cluster(kmeans_result_3_normalized, 
             data = data_normalized, 
             geom = "point", 
             main = "K-means avec 3 clusters sur données normalisées") +
  geom_point(data = centroids, 
             aes(x = centroids[[1]], y = centroids[[2]]), 
             color = "red", 
             size = 4, 
             shape = 8) # Superposition des centroïdes

# Afficher la matrice complète 
print(dist_matrix)


# Résultat K-means avec 4 clusters et introductions des centroides 
kmeans_result_4_normalized <- kmeans(data_normalized, centers = 4, nstart = 1000)

# Récupération des coordonnées des centroïdes
centroids2 <- as.data.frame(kmeans_result_4_normalized$centers)

# Renommer les colonnes pour correspondre aux axes du graphique
colnames(centroids2) <- colnames(data_normalized)

# Calcul des distances entre les centroïdes
distances_between_centroids2 <- dist(centroids2)

# Afficher les distances 
print(distances_between_centroids2)

# Convertir les distances en une matrice complète 
dist_matrix2 <- as.matrix(distances_between_centroids2)

# Visualisation des clusters et superposition des centroïdes
fviz_cluster(kmeans_result_4_normalized, 
             data = data_normalized, 
             geom = "point", 
             main = "K-means avec 4 clusters sur données normalisées") +
  geom_point(data = centroids2, 
             aes(x = centroids2[[1]], y = centroids2[[2]]), 
             color = "red", 
             size = 4, 
             shape = 8) # Superposition des centroïdes

# Afficher la matrice complète 
print(dist_matrix2)









# Ajouter les clusters aux données normalisées
data_with_clusters <- data.frame(data_normalized, cluster = factor(kmeans_result$cluster))

# Analyser chaque cluster de manière automatisée
for (i in 1:4) {  # 4 correspond au nombre de clusters
  cat("\nRésumé pour le Cluster", i, ":\n")
  
  # Filtrer les observations du cluster actuel
  cluster_data <- data_with_clusters %>% filter(cluster == i)
  
  # Afficher un résumé statistique
  print(summary(cluster_data))
  
  #  Enregistrer les données du cluster dans un fichier CSV
  # write.csv(cluster_data, paste0("cluster_", i, ".csv"), row.names = FALSE)
}

