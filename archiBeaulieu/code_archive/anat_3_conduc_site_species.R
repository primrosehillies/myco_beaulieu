library(psych)



anat <- read.csv(file = "anat_6.csv", header = T, stringsAsFactors = T)
anat <- anat %>%
  select(-X.1, -X)

anat$id_amap <- as.character(anat$id_amap)
vessel_summary$id_amap <- substr(image_dimensions_df$id_complete, 1, 4)

anat <- anat %>%
  left_join(vessel_summary, by = "id_amap")
anat$number <- as.factor(anat$number)
anat$count_vessel <- as.numeric(anat$count_vessel)
anat$id_amap <- as.numeric(anat$id_amap)

str(anat)
numerical_cols <- c("image_surface_µm2", "image_surface_rectified_µm2.x", 
                    "mean_vessel_size_µm2", "std_dev_vessel_size_µm2", "mean_vessel_perim_µm", 
                    "std_dev_perim_µm", "sum_vessel_area_µm2", "count_vessel", "count_cluster", "vessel_grouping", "dh", 
                    "image_surface_rectified_mm2", "vessel_density_per_mm2", "vessel_density_per_m2", 
                    "dh_m", "vessel_area", "Kth")

anat_Racine <- anat %>% 
  filter(organ == "R") %>%
  dplyr::select(all_of(numerical_cols), number) %>%
  rename_with(~ paste0(., "_Racine"), all_of(numerical_cols))

anat_Tige <- anat %>% 
  filter(organ == "T") %>%
  dplyr::select(all_of(numerical_cols), number) %>%
  rename_with(~ paste0(., "_Tige"), all_of(numerical_cols))

anat_Tige_Racine <- anat_Tige %>%
  full_join(anat_Racine, by = "number")

anat_metadata <- anat %>%
  dplyr::select(number, site, site_type, site_replicate, species_site_replicate, species, species_replicate, micro) %>%
  distinct(number, site, site_type, site_replicate, species_site_replicate, species, species_replicate, micro)


anat_combined <- anat_metadata %>%
  left_join(anat_Tige_Racine, by = "number")

one_ninety <- read.csv("1_90.csv", header = T, stringsAsFactors = T)
anat_combined$number <- as.character(anat_combined$number)
one_ninety$number <- as.character(one_ninety$number)

anat_combined <- anat_combined %>%
  full_join(one_ninety, by = "number")
anat_combined$number <- as.integer(anat_combined$number)

write.csv(anat_combined, "anat_combined_3.csv")

library(psych)
pairs.panels(anat_combined[,c("Kth_Tige", "vessel_density_per_mm2_Tige", "dh_Tige", 
                     "site_type", "species",
                     "Kth_Racine", "vessel_density_per_mm2_Racine", "dh_Racine")],
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

# 
# lmAnat01 <- lm(Kth ~ species, data = anat)
# plot(lmAnat01)
# anova(lmAnat01)
# summary(lmAnat01)
# boxplot(Kth ~ species, data = anat)
# 
# lmAnat02 <- lm(Kth ~ site_type, data = anat)
# plot(lmAnat02)
# anova(lmAnat02)
# summary(lmAnat02)
# boxplot(Kth ~ site_type, data = anat)
# 
# lmAnat03 <- lm(Kth ~ site_type * species, data = anat)
# plot(lmAnat03)
# anova(lmAnat03)
# summary(lmAnat03)
# boxplot(Kth ~ site_type * species, data = anat)
# 
# 
# lmAnat04 <- lm(vessel_density_per_mm2 ~ site_type, data = anat)
# plot(lmAnat04)
# anova(lmAnat04)
# summary(lmAnat04)
# 
