library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(jpeg)
centroids <- read.csv("test_k_means_2.csv", header = T, stringsAsFactors = F)

#####Calculating euclidian distances####
# Compute radii from areas
centroids$Radius <- sqrt(centroids$Area / pi)
centroids$Radius_enhanced <- centroids$Radius + 4.5

# Compute pairwise distances and adjust for radii
distances <- as.matrix(dist(centroids[, c("X", "Y")], method = "euclidean"))
radius_matrix <- outer(centroids$Radius_enhanced, centroids$Radius_enhanced, "+")  # Sum of radii
perimeter_distances <- distances - radius_matrix  # Perimeter-to-perimeter distances

centroids$Cluster <- 0
cluster_id <- 1

# Iterate to merge clusters based on proximity
repeat {
  changes <- FALSE  # Track if any change happens in this iteration
  
  for (i in seq_len(nrow(perimeter_distances))) {
    if (centroids$Cluster[i] == 0) {
      # Assign a new cluster
      centroids$Cluster[i] <- cluster_id
      # Find nearby objects based on perimeter distance
      nearby <- which(perimeter_distances[i, ] <= 0)  # Overlapping or touching circles (distance <= 0)
      centroids$Cluster[nearby] <- cluster_id
      changes <- TRUE  # Record that a change has occurred
      cluster_id <- cluster_id + 1
    }
  }
  
  # After assigning initial clusters, check if any centroids should be merged
  for (i in seq_len(nrow(perimeter_distances) - 1)) {  # Corrected: limit to nrow - 1
    for (j in seq(i + 1, nrow(perimeter_distances))) {  # Corrected: starting from i+1 to avoid redundant checks
      if (perimeter_distances[i, j] <= 0) {  # If circles are overlapping
        # Merge clusters if they are different
        if (centroids$Cluster[i] != centroids$Cluster[j]) {
          old_cluster <- centroids$Cluster[j]
          new_cluster <- centroids$Cluster[i]
          centroids$Cluster[centroids$Cluster == old_cluster] <- new_cluster
          changes <- TRUE  # Record that a merge has occurred
        }
      }
    }
  }
  
  # Exit loop if no changes were made
  if (!changes) break
}

# Define a distinct color palette for 100 groups
colors <- brewer.pal(12, "Set3")  # 12 colors from Set3 palette
# Repeat the palette until you cover the number of clusters you have
colors <- rep(colors, length.out = length(unique(centroids$Cluster)))

centroids$Y_flipped <- max(centroids$Y) - centroids$Y



#####Putting on picture####
library(ggplot2)
library(ggforce)
library(jpeg)  # For reading the PNG image

conversion_factor <- 1.9111

# Load the cross-sectional image
image_path <- "square/1826_T1-Ho2-2_Tige_X200_09µm_square.jpg"
img <- readJPEG(image_path)

img_height <- dim(img)[1]
img_width <- dim(img)[2]

# Convert the X,Y coordinates from µm to pixels
centroids$X_px <- centroids$X * conversion_factor
centroids$Y_px <- centroids$Y * conversion_factor


centroids$Y_px_flipped <- img_height - centroids$Y_px

#####Visualization####

# Visualize with circles
ggplot(centroids, aes(x = X, y = Y_flipped, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  geom_circle(aes(x0 = X, y0 = Y_flipped, r = Radius_enhanced), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Distances Between Circle Perimeters", 
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()



ggplot() +
  # Add the image as a background
  annotation_raster(img, xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
  # Add vessel points on top
  geom_point(data = centroids, aes(x = X_px, y = Y_px_flipped, color = as.factor(Cluster)), size = 2) +
  scale_color_manual(values = colors) +  # You can define your own color palette
  #geom_circle(aes(x0 = X_px, y0 = Y_flipped, r = Radius_enhanced), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Vessel Coordinates Overlay") +
  theme_minimal() +
  theme(legend.position = "none")


#####Whole dataset tryouts####
library(dplyr)
library(purrr)

# Define the clustering function
cluster_vessels <- function(data) {
  # Compute radii
  data$Radius <- sqrt(data$Area / pi)
  data$Radius_enhanced <- data$Radius + 4.5
  
  # Compute pairwise distances and adjust for radii
  distances <- as.matrix(dist(data[, c("X", "Y")], method = "euclidean"))
  radius_matrix <- outer(data$Radius_enhanced, data$Radius_enhanced, "+")
  perimeter_distances <- distances - radius_matrix
  
  data$Cluster <- 0
  cluster_id <- 1
  
  # Clustering logic (same as your provided code)
  repeat {
    changes <- FALSE
    
    for (i in seq_len(nrow(perimeter_distances))) {
      if (data$Cluster[i] == 0) {
        data$Cluster[i] <- cluster_id
        nearby <- which(perimeter_distances[i, ] <= 0)
        data$Cluster[nearby] <- cluster_id
        changes <- TRUE
        cluster_id <- cluster_id + 1
      }
    }
    
    for (i in seq_len(nrow(perimeter_distances) - 1)) {
      for (j in seq(i + 1, nrow(perimeter_distances))) {
        if (perimeter_distances[i, j] <= 0) {
          if (data$Cluster[i] != data$Cluster[j]) {
            old_cluster <- data$Cluster[j]
            new_cluster <- data$Cluster[i]
            data$Cluster[data$Cluster == old_cluster] <- new_cluster
            changes <- TRUE
          }
        }
      }
    }
    
    if (!changes) break
  }
  
  return(data)
}

# Group by `id_amap` and apply clustering
result <- vessel %>%
  group_by(id_amap) %>%
  group_split() %>%
  map_dfr(cluster_vessels)



#####centroids_1835####
centroids_1835 <- result %>%
  filter(id_amap == "1835") %>%
  filter(Area < 1000)
centroids_1835$Y_flipped <- max(centroids_1835$Y) - centroids_1835$Y

ggplot(centroids_1835, aes(x = X, y = Y_flipped, color = as.factor(Cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = colors) +
  geom_circle(aes(x0 = X, y0 = Y_flipped, r = Radius_enhanced), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Vessel map", 
       x = "X", y = "Y") +
  theme_minimal()


library(ggplot2)
library(ggforce)
library(jpeg)  # For reading the PNG image

conversion_factor <- 1.9111

# Load the cross-sectional image
image_path <- "square/1835_T1-Ft1-13_Racine_X200_09µm_square.jpg"
img <- readJPEG(image_path)

img_height <- dim(img)[1]
img_width <- dim(img)[2]

# Convert the X,Y coordinates from µm to pixels
centroids_1835$X_px <- centroids_1835$X * conversion_factor
centroids_1835$Y_px <- centroids_1835$Y * conversion_factor


centroids_1835$Y_px_flipped <- img_height - centroids_1835$Y_px



ggplot() +
  # Add the image as a background
  annotation_raster(img, xmin = 0, xmax = img_width, ymin = 0, ymax = img_height) +
  # Add vessel points on top
  geom_point(data = centroids_1835, aes(x = X_px, y = Y_px_flipped, color = as.factor(Cluster)), size = 2) +
  scale_color_manual(values = colors) +  # You can define your own color palette
  #geom_circle(aes(x0 = X, y0 = Y, r = Radius_enhanced), inherit.aes = FALSE, color = "black", alpha = 0.3) +
  labs(title = "Vessel grouping") +
  theme_minimal() +
  theme(legend.position = "none")
