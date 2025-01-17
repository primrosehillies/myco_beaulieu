library(tidyverse)
library(ggplot2)
centroids <- read.csv("test_k_means_2.csv", header = T, stringsAsFactors = F)
str(centroids)
centroids <- centroids %>%
  select(X, Y)


X_scaled <- scale(centroids$X)
Y_scaled <- scale(centroids$Y)
Perimeter_scaled <- scale(centroids$Perim.)
Area_scaled <- scale(centroids$Area)


library(stats)
distance_matrix <- dist(
  as.matrix(centroids[, c("X_scaled", "Y_scaled", "Perimeter_scaled", "Area_scaled")]),
  method = "euclidean"
)

hc <- hclust(distance_matrix, method = "complete")
centroids$Cluster <- cutree(hc, k = 5)  # Replace '5' with the desired number of clusters


library(ggplot2)
ggplot(centroids, aes(x = X, y = Y, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  labs(title = "Hierarchical Clustering of Vessels", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()

db_result <- dbscan(centroids[, c("X_scaled", "Y_scaled", "Perimeter_scaled", "Area_scaled")], eps = 1, minPts = 3)
centroids$Cluster <- as.factor(db_result$cluster)

# Visualize the clusters
ggplot(centroids, aes(x = X, y = Y, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "DBSCAN with Vessel Parameters", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()





#####

# Use only proximity (X, Y) and lightly include size (Perimeter, Area)
db_data <- as.matrix(centroids[, c("X_scaled", "Y_scaled", "Perimeter_scaled", "Area_scaled")])

# Run DBSCAN with a small epsilon
db_result <- dbscan(db_data, eps = 5, minPts = 2)  # Adjust `eps` as needed

# Add cluster results to your dataset
centroids$Cluster <- as.factor(db_result$cluster)

# Visualize
ggplot(centroids, aes(x = X, y = Y, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "DBSCAN Clustering of Vessels", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()






#####
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggforce)


# Define a distance threshold
distance_threshold <- 30  # Adjust this based on your dataset

# Compute pairwise distances (X, Y)
distances <- dist(centroids[, c("X", "Y")], method = "euclidean")
dist_matrix <- as.matrix(distances)

# Initialize groups
centroids$Cluster <- 0
cluster_id <- 1

for (i in seq_len(nrow(dist_matrix))) {
  if (centroids$Cluster[i] == 0) {
    # Assign a new cluster
    centroids$Cluster[i] <- cluster_id
    
    # Group nearby vessels
    nearby <- which(dist_matrix[i, ] < distance_threshold)
    centroids$Cluster[nearby] <- cluster_id
    
    cluster_id <- cluster_id + 1
  }
}

# Define a distinct color palette for 100 groups
colors <- brewer.pal(12, "Set3")  # 12 colors from Set3 palette
# Repeat the palette until you cover the number of clusters you have
colors <- rep(colors, length.out = length(unique(centroids$Cluster)))
# Visualize
ggplot(centroids, aes(x = X, y = Y, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  labs(title = "Custom Proximity-Based Grouping", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()
#####


distance_threshold <- 30  # Adjust this based on your dataset

centroids$Radius <- sqrt(centroids$Area / pi)
# Compute pairwise distances and adjust for radii
distances <- as.matrix(dist(centroids[, c("X", "Y")], method = "euclidean"))
radius_matrix <- outer(centroids$Radius, centroids$Radius, "+")  # Sum of radii
adjusted_distances <- distances - radius_matrix  # Effective distances

# Initialize groups
centroids$Cluster <- 0
cluster_id <- 1

for (i in seq_len(nrow(adjusted_distances))) {
  if (centroids$Cluster[i] == 0) {
    # Assign a new cluster
    centroids$Cluster[i] <- cluster_id
    
    # Group nearby objects considering radii
    nearby <- which(adjusted_distances[i, ] <= 0)  # Overlapping circles
    centroids$Cluster[nearby] <- cluster_id
    
    cluster_id <- cluster_id + 1
  }
}
ggplot(centroids, aes(x = X, y = Y, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  geom_circle(aes(x0 = X, y0 = Y, r = Radius), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Grouping Based on Adjusted Distances", 
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()











#####No overlapping####
# Load required libraries
library(ggplot2)
library(ggforce)
library(RColorBrewer)

# Compute radii from areas
centroids$Radius <- sqrt(centroids$Area / pi)

# Compute pairwise distances and adjust for radii
distances <- as.matrix(dist(centroids[, c("X", "Y")], method = "euclidean"))
radius_matrix <- outer(centroids$Radius, centroids$Radius, "+")  # Sum of radii
perimeter_distances <- distances - radius_matrix  # Perimeter-to-perimeter distances


centroids$Cluster <- 0
cluster_id <- 1
# Ensure distances can't be negative (overlap scenario)
perimeter_distances[perimeter_distances < 0] <- 0


for (i in seq_len(nrow(perimeter_distances))) {
  if (centroids$Cluster[i] == 0) {
    # Assign a new cluster
    centroids$Cluster[i] <- cluster_id
    
    # Find nearby objects based on perimeter distance
    nearby <- which(perimeter_distances[i, ] <= 0)  # Overlapping or touching circles (distance <= 0)
    centroids$Cluster[nearby] <- cluster_id
    
    cluster_id <- cluster_id + 1
  }
}

# Define a distinct color palette for 100 groups
colors <- brewer.pal(12, "Set3")  # 12 colors from Set3 palette
# Repeat the palette until you cover the number of clusters you have
colors <- rep(colors, length.out = length(unique(centroids$Cluster)))
# Visualize with circles
ggplot(centroids, aes(x = X, y = Y, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  geom_circle(aes(x0 = X, y0 = Y, r = Radius), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Distances Between Circle Perimeters", 
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()




#####Increase radius for overlapping according to how close they are together####
# Load required libraries
library(ggplot2)
library(ggforce)
library(RColorBrewer)

# Compute radii from areas
centroids$Radius <- sqrt(centroids$Area / pi)
centroids$Radius_enhanced <- centroids$Radius + 4

# Compute pairwise distances and adjust for radii
distances <- as.matrix(dist(centroids[, c("X", "Y")], method = "euclidean"))
radius_matrix <- outer(centroids$Radius_enhanced, centroids$Radius_enhanced, "+")  # Sum of radii
perimeter_distances <- distances - radius_matrix  # Perimeter-to-perimeter distances


centroids$Cluster <- 0
cluster_id <- 1


for (i in seq_len(nrow(perimeter_distances))) {
  if (centroids$Cluster[i] == 0) {
    # Assign a new cluster
    centroids$Cluster[i] <- cluster_id
    
    # Find nearby objects based on perimeter distance
    nearby <- which(perimeter_distances[i, ] <= 0)  # Overlapping or touching circles (distance <= 0)
    centroids$Cluster[nearby] <- cluster_id
    
    cluster_id <- cluster_id + 1
  }
}

# Define a distinct color palette for 100 groups
colors <- brewer.pal(12, "Set3")  # 12 colors from Set3 palette
# Repeat the palette until you cover the number of clusters you have
colors <- rep(colors, length.out = length(unique(centroids$Cluster)))
# Visualize with circles
ggplot(centroids, aes(x = X, y = Y, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  geom_circle(aes(x0 = X, y0 = Y, r = Radius_enhanced), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Distances Between Circle Perimeters", 
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()
#####


















k <- 20
kmeans_results <- kmeans(centroids, centers = k)
centroids$Cluster <- as.factor(kmeans_results$cluster)


ggplot(centroids, aes(x = X, y = Y, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "K-Means Clustering of Vessels", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()


wss <- sapply(1:10, function(k) kmeans(centroids, centers = k)$tot.withinss)

# Plot WSS vs. number of clusters
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares")


library(dbscan)

db_result <- dbscan(centroids[, c("X", "Y")], eps = 10, minPts = 2)
centroids$Cluster <- as.factor(db_result$cluster)


ggplot(centroids, aes(x = X, y = Y, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "DBSCAN Clustering of Vessels", x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()
