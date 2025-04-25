library(tidyverse)

vessel <- read.csv(file = "vessel_4.csv", header = T, stringsAsFactors = T)
vessel$file <- vessel$Label
anat <- read.csv(file = "anat_3.csv", header = T, stringsAsFactors = T)
anat$id_amap <- as.character(anat$id_amap)


#####Getting proper surface by getting rid of big empty areas####

vessel$id_amap <- as.factor(substr(vessel$file, 1, 4))

#####Incorporating clusters####
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
vessel <- vessel %>%
  group_by(id_amap) %>%
  group_split() %>%
  map_dfr(cluster_vessels)





#####Getting rid of extremes####
vessel_mean <- vessel %>%
  group_by(id_amap) %>%
  summarize(mean = mean(Area))

vessel_extreme <- vessel %>%
  filter(Area > 1200)


vessel_extreme_sum <- vessel_extreme %>%
  group_by(id_amap) %>%
  summarize(sum = sum(Area))

vessel_extreme_sum$id_amap <- as.character(vessel_extreme_sum$id_amap)
anat$id_amap <- as.character(anat$id_amap)
vessel_extreme_sum <- vessel_extreme_sum %>%
  full_join(anat, by = "id_amap")

vessel_extreme_sum$sum[is.na(vessel_extreme_sum$sum)] <- 0
vessel_extreme_sum$image_surface_rectified_µm2 <- vessel_extreme_sum$image_surface_µm2 - vessel_extreme_sum$sum

vessel_extreme_sum <- vessel_extreme_sum %>%
  select(id_amap, image_surface_rectified_µm2)

anat <- anat %>%
  left_join(vessel_extreme_sum, by = "id_amap")

#####Calculating mean, standard deviation, sum, count, dh####
#Getting rid of big vessels
vessel_1200 <- vessel %>%
  select(id_amap, Area, Perim., Cluster) %>%
  filter(Area <= 1200) %>%
  mutate(diameter = 2*sqrt(Area/pi),
         d4 = diameter^4)

vessel_summary_pre <- vessel_1200 %>%
  group_by(id_amap) %>%
  distinct(Cluster) %>%
  summarize(count_cluster = n())

  
vessel_summary_pre2 <- vessel_1200 %>%
  group_by(id_amap) %>%
  summarize(mean_vessel_size_µm2 = mean(Area),
            std_dev_vessel_size_µm2 = sd(Area),
            mean_vessel_perim_µm = mean(Perim.),
            std_dev_perim_µm = sd(Perim.),
            sum_vessel_area_µm2 = sum(Area),
            dh = (sum(d4)/n())^0.25,
            count_vessel = n()
            ) 

vessel_summary <- vessel_summary_pre2 %>%
  left_join(vessel_summary_pre, by = "id_amap") %>%
  mutate(vessel_grouping = count_vessel / count_cluster)

#####Calculating vessel density and Kth####
vessel_summary <- anat %>%
  select(id_amap, image_surface_rectified_µm2) %>%
  left_join(vessel_summary, by = "id_amap")

vessel_summary$image_surface_rectified_mm2 <- vessel_summary$image_surface_rectified_µm2 / 1000000

vessel_summary$vessel_density_per_mm2 <- vessel_summary$count_vessel / vessel_summary$image_surface_rectified_mm2
vessel_summary$vessel_density_per_m2 <- vessel_summary$vessel_density_per_mm2 * 10^6
vessel_summary$dh_m <- vessel_summary$dh * 10^-6
vessel_summary$vessel_area <- vessel_summary$sum_vessel_area_µm2 / vessel_summary$image_surface_rectified_µm2


#Getting Kth

water_density <- 998.2
water_viscosity <- 1.002*10^-9
hagen_poiseuille <- (pi * water_density) / (128 * water_viscosity)

vessel_summary$Kth <- hagen_poiseuille * vessel_summary$vessel_density_per_m2 * vessel_summary$dh_m^4




plot(vessel_summary$vessel_density_per_mm2, vessel_summary$Kth)

summary(vessel_summary)

write.csv(vessel_summary, file = "vessel_summary_4.csv")
write.csv(anat, file = "anat_6.csv")

