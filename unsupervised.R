#Library 
library(factoextra)
library(cluster)
library(ggplot2)
library(datasets)
library(dplyr)




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






# 3. Nombre optimal de clusters : méthode du coude ----
# Methode du coude à la main
valeursK = 2:15
sommeCarresTotale = array(NA, length(valeursK))

for (i in 1:length(valeursK)){
  sommeCarresTotale[i] = kmeans(data_normalized, centers = valeursK[i], nstart = 100)$tot.withinss
}

toPlot = tibble(K = valeursK, metrique = sommeCarresTotale)
ggplot(toPlot) + 
  geom_point(aes(x = K, y = metrique)) + 
  geom_line(aes(x = K, y = metrique))

# Methode du coude avec une fonction
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
plot(K_values, dintra_values, type = "b", pch = 19, col = "blue", 
     xlab = "Nombre de clusters (K)", ylab = "Totale des distances intra-cluster",
     main = "Elbow Method (Coude) pour K-means", xaxt = "n")  # Empêcher la génération automatique de l'axe x

# Ajouter un axe x avec des valeurs de 1 à 16
axis(1, at = K_values, labels = K_values)









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
plot(K_values, silhouette_scores, type = "b", pch = 19, col = "blue", 
     xlab = "Nombre de clusters (K)", ylab = "Score moyen de la silhouette",
     main = "Méthode de la silhouette pour déterminer le nombre optimal de clusters")




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

# Appliquer K-means avec 5 clusters sur les données normalisées
kmeans_result_4_normalized <- kmeans(data_normalized, centers = 5, nstart = 100)
fviz_cluster(kmeans_result_4_normalized, data = data_normalized, geom = "point", main = "K-means avec 4 clusters sur données normalisées")









# Appliquer K-means clustering (par exemple avec 4 clusters)
kmeans_result <- kmeans(data_normalized, centers = 4, nstart = 25)

# Visualisation des clusters
fviz_cluster(kmeans_result, data = data_normalized,
             geom = "point",  # Utiliser des points pour les observations
             ellipse.type = "convex",  # Dessiner des ellipses convexes autour des clusters
             main = "Visualisation des clusters avec K-means")

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












