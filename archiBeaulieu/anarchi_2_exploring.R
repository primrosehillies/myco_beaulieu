
library(ggplot2)
library(tidyverse)
library(psych)

anarchi <- read.csv("anarchi_2.csv", header = T, stringsAsFactors = T)
anarchi <- anarchi %>%
  select(-X, -num)
anarchi <- anarchi %>%
  select(number, site, site_type, site_replicate, species_site_replicate, species, species_replicate, micro, everything())

write.csv(anarchi, "anarchi_3.csv")
#####First look####
plot(anarchi$diam_tig_princip_mm, anarchi$mean_vessel_size_µm2_Tige)

anarchi %>%
  select(number, mean_vessel_size_µm2_Tige) %>%
  filter(mean_vessel_size_µm2_Tige < 100)

pairs.panels(anarchi[,c("Kth_Tige", "vessel_density_per_mm2_Tige", "dh_Tige", 
                        "site_type", "species",
                        "diam_tig_princip_mm", "diam_rac_pivot_mm",
                        "diam_rac_lat_mm", "diam_tig_lat_mm",
                        "empreinte_photo_mm", "y_photo_mm")],
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)



anarchi_Ft <- anarchi %>%
  filter(species == "Ft") 
anarchi_Fe <- anarchi %>%
  filter(species == "Fe")
anarchi_Ho <- anarchi %>%
  filter(species == "Ho")

plot(anarchi_Ft$mean_vessel_size_µm2_Tige, anarchi_Ft$axes_caul_totaux)
plot(anarchi_Fe$mean_vessel_size_µm2_Tige, anarchi_Fe$axes_caul_totaux)
plot(anarchi_Ho$mean_vessel_size_µm2_Tige, anarchi_Ho$axes_caul_totaux)

boxplot(mean_vessel_size_µm2_Tige ~ species * site_type, data = anarchi)
boxplot(mean_vessel_size_µm2_Tige ~ species, data = anarchi)

plot(mean_vessel_size_µm2_Tige ~ mean_vessel_size_µm2_Racine, data = anarchi)

plot(mean_vessel_size_µm2_Tige ~ vessel_density_per_mm2_Tige, data = anarchi)
lmAnat05 <- lm(mean_vessel_size_µm2_Tige ~ vessel_density_per_mm2_Tige, data = anarchi)

anova(lmAnat05)

plot(mean_vessel_size_µm2_Racine ~ vessel_density_per_mm2_Racine, data = anarchi)
lmAnat06 <- lm(mean_vessel_size_µm2_Racine ~ vessel_density_per_mm2_Racine, data = anarchi)
anova(lmAnat06)




#####Species####
#Stem
lmAnat07 <-  lm(mean_vessel_size_µm2_Tige ~ vessel_density_per_mm2_Tige * species, data = anarchi)
plot(lmAnat07)
anova(lmAnat07)
summary(lmAnat07)


anarchi_no_na1 <- anarchi %>% 
  filter(!is.na(vessel_density_per_mm2_Tige) & !is.na(mean_vessel_size_µm2_Tige)) 

observed_ranges <- anarchi_no_na1 %>%
  group_by(species) %>%
  summarize(
    min_density = min(vessel_density_per_mm2_Tige),
    max_density = max(vessel_density_per_mm2_Tige)
  )

new_data <- expand.grid(
  vessel_density_per_mm2_Tige = seq(min(anarchi_no_na1$vessel_density_per_mm2_Tige),
                                    max(anarchi_no_na1$vessel_density_per_mm2_Tige),
                                    length.out = 100),
  species = unique(anarchi_no_na1$species)
)

new_data_filtered <- new_data %>%
  left_join(observed_ranges, by = "species") %>%
  filter(
    vessel_density_per_mm2_Tige >= min_density & 
      vessel_density_per_mm2_Tige <= max_density
  )

new_data_filtered$predicted_mean_vessel_size <- predict(lmAnat07, newdata = new_data_filtered)

# Plot with filtered prediction lines
ggplot(data = anarchi_no_na1, aes(x = vessel_density_per_mm2_Tige, y = mean_vessel_size_µm2_Tige, color = species)) +
  geom_point(alpha = 0.6) +  # Scatter plot for raw data
  geom_line(data = new_data_filtered, aes(x = vessel_density_per_mm2_Tige, y = predicted_mean_vessel_size, color = species), linewidth = 1) +  # Prediction lines
  labs(
    x = "Stem Vessel Density (per mm²)",
    y = "Stem Mean Vessel Size (µm²)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




plot(tig_ratio ~ rac_ratio, data = anarchi)
plot(tig_ratio ~ mean_vessel_size_µm2_Tige, data = anarchi)
plot(rac_ratio ~ mean_vessel_size_µm2_Racine, data = anarchi)




#Root
lmAnat08 <-  lm(mean_vessel_size_µm2_Racine ~ vessel_density_per_mm2_Racine * species, data = anarchi_no_na1)
plot(lmAnat08)
anova(lmAnat08)
summary(lmAnat08)


anarchi_no_na1 <- anarchi %>% 
  filter(!is.na(vessel_density_per_mm2_Tige) & !is.na(mean_vessel_size_µm2_Tige)) %>%
  filter(mean_vessel_size_µm2_Tige < 300)

observed_ranges <- anarchi_no_na1 %>%
  group_by(species) %>%
  summarize(
    min_density = min(vessel_density_per_mm2_Tige),
    max_density = max(vessel_density_per_mm2_Tige)
  )

new_data <- expand.grid(
  vessel_density_per_mm2_Tige = seq(min(anarchi_no_na1$vessel_density_per_mm2_Tige),
                                    max(anarchi_no_na1$vessel_density_per_mm2_Tige),
                                    length.out = 100),
  species = unique(anarchi_no_na1$species)
)

new_data_filtered <- new_data %>%
  left_join(observed_ranges, by = "species") %>%
  filter(
    vessel_density_per_mm2_Tige >= min_density & 
      vessel_density_per_mm2_Tige <= max_density
  )

new_data_filtered$predicted_mean_vessel_size <- predict(lmAnat07, newdata = new_data_filtered)


ggplot(data = anarchi, aes(x = y_photo_mm, y = Kth_Racine, color = site_type)) +
  geom_point(alpha = 0.6)

lmAnat09 <- lm(Kth_Racine ~ y_photo_mm * site_type, data = anarchi)
plot(lmAnat09)
anova(lmAnat09)
summary(lmAnat09)
# Plot with filtered prediction lines
ggplot(data = anarchi_no_na1, aes(x = vessel_density_per_mm2_Racine, y = mean_vessel_size_µm2_Racine, color = species)) +
  geom_point(alpha = 0.6) #+  # Scatter plot for raw data
  # geom_line(data = new_data_filtered, aes(x = vessel_density_per_mm2_Tige, y = predicted_mean_vessel_size, color = species), linewidth = 1) +  # Prediction lines
  # labs(
  #   x = "Stem Vessel Density (per mm²)",
  #   y = "Stem Mean Vessel Size (µm²)",
  #   color = "Species"
  # ) +
  # theme_minimal() +
  # theme(legend.position = "top")




plot(Kth_Tige ~ empreinte_photo_mm, data = anarchi)
ggplot(data = anarchi, aes(x = empreinte_photo_mm, y = Kth_Tige, color = site_type)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0.6434242, slope = 0.0024336)
lmAnat10 <- lm(Kth_Tige ~ empreinte_photo_mm + species , data = anarchi)
plot(lmAnat10)
anova(lmAnat10)
summary(lmAnat10)



lmAnat11 <- lm(vessel_density_per_mm2_Tige ~ site_type + species, data = anarchi)
lmAnat11aov <- aov(vessel_density_per_mm2_Tige ~ site_type + species, data = anarchi)
plot(lmAnat11)
anova(lmAnat11)
summary(lmAnat11)

hsdAnat11 <-TukeyHSD(lmAnat11aov) # fonction pour faire un test post hoc de Tukey
hsdAnat11
plot(hsdAnat11)

plot(vessel_density_per_mm2_Tige ~ site_type, data = anarchi)


lmAnat12 <- lm(Kth_Tige ~ species, data = anarchi)
anova(lmAnat12)
summary(lmAnat12)


#####
plot(Kth_Racine ~ mean_vessel_size_µm2_Racine, data = anarchi)

plot(Kth_Tige ~ vessel_density_per_mm2_Tige, data = anarchi)

plot(Kth_Tige ~ Kth_Racine, data = anarchi)
abline(lm(Kth_Tige ~ Kth_Racine, data = anarchi))


str(anarchi$species)
anarchi %>%
  group_by(species) %>%
  count()

anarchi %>%
  select(num,mean_vessel_size_µm2_Tige) %>%
  filter(mean_vessel_size_µm2_Tige > 300)

anarchi %>%
  select(num,mean_vessel_size_µm2_Tige) %>%
  filter(mean_vessel_size_µm2_Tige > 200)

anarchi %>%
  select(num,mean_vessel_size_µm2_Racine) %>%
  filter(mean_vessel_size_µm2_Racine > 400)

anarchi %>%
  select(num,mean_vessel_size_µm2_Racine) %>%
  filter(mean_vessel_size_µm2_Racine < 100)

#####ACP####
library(psych)
library(ade4)
library(missMDA)
library(tidyverse)
anarchi_imputed <- anarchi %>%
  select(diam_tig_princip_mm,
         diam_rac_pivot_mm,
         prof_pivot_mm,
         conicite_mm,
         empreinte_photo_mm,
         y_photo_mm,
         mean_vessel_size_µm2_Tige,
         mean_vessel_size_µm2_Racine,
         vessel_density_per_mm2_Tige,
         vessel_density_per_mm2_Racine,
         Kth_Tige,
         Kth_Racine)


anarchi_imputed <- imputePCA(anarchi_imputed)$completeObs
anarchi_imputed <- as.data.frame(anarchi_imputed)


acp3 <- dudi.pca(anarchi_imputed, scale = TRUE, center = TRUE)
acp3$co
acp3$eig


s.corcircle(acp3$co)




species_col <- anarchi[, "species"]
unique_species <- unique(species_col)
color_vector <- 1:length(unique_species)
# Plot with s.class
s.class(acp3$li, fac = factor(species_col), col = color_vector)




site_type_col <- anarchi[, "site_type"]
unique_site_type <- unique(site_type_col)
site_type_vector <- 1:length(unique_site_type)
# Plot with s.class
s.class(acp3$li, fac = factor(site_type_col), col = site_type_vector)




# site_col <- anarchi[, "site"]
# unique_site <- unique(site_col)
# site_vector <- 1:length(unique_site)
# # Plot with s.class
# s.class(acp3$li, fac = factor(site_col), col = site_vector)




###############ACP2####
anarchi_imputed2 <- anarchi %>%
  select(diam_tig_princip_mm,
         prof_pivot_mm,
         conicite_mm,
         empreinte_photo_mm,
         y_photo_mm,
         mean_vessel_size_µm2_Tige,
         vessel_density_per_mm2_Tige,
         Kth_Tige)

anarchi_imputed2 <- imputePCA(anarchi_imputed2)$completeObs
acp4 <- dudi.pca(anarchi_imputed2, scale = TRUE, center = TRUE)
acp4$co
acp4$eig
s.corcircle(acp4$co)



species_col <- anarchi[, "species"]
unique_species <- unique(species_col)
color_vector <- 1:length(unique_species)
# Plot with s.class
s.class(acp4$li, fac = factor(species_col), col = color_vector)


s.class(acp4$li, fac = factor(site_type_col), col = site_type_vector)







#####Vessel grouping####
plot(anarchi$vessel_grouping_Tige, anarchi$vessel_grouping_Racine)

anarchi_no_Ho <- anarchi %>%
  filter(species != "Ho")


ggplot(data = anarchi_no_Ho, aes(x = vessel_grouping_Tige, y = vessel_grouping_Racine, color = species)) +
  geom_point(alpha = 0.6) +  # Scatter plot for raw data
  # geom_line(data = new_data_filtered, aes(x = vessel_density_per_mm2_Tige, y = predicted_mean_vessel_size, color = species), linewidth = 1) +  # Prediction lines
  # labs(
  #   x = "Stem Vessel Density (per mm²)",
  #   y = "Stem Mean Vessel Size (µm²)",
  #   color = "Species"
  # ) +
  theme_minimal()


ggplot(data = anarchi_no_Ho, aes(x = vessel_grouping_Tige, y = vessel_grouping_Racine, color = site_type)) +
  geom_point(alpha = 0.6) +  # Scatter plot for raw data
  # geom_line(data = new_data_filtered, aes(x = vessel_density_per_mm2_Tige, y = predicted_mean_vessel_size, color = species), linewidth = 1) +  # Prediction lines
  # labs(
  #   x = "Stem Vessel Density (per mm²)",
  #   y = "Stem Mean Vessel Size (µm²)",
  #   color = "Species"
  # ) +
  theme_minimal()

anarchi_no_Ho$log_grouping_Tige <- log(anarchi_no_Ho$vessel_grouping_Tige)
anarchi_no_Ho$log_grouping_Racine <- log(anarchi_no_Ho$vessel_grouping_Racine)

ggplot(data = anarchi_no_Ho, aes(x = log_grouping_Tige, y = log_grouping_Racine, color = site_type)) +
  geom_point(alpha = 0.6) +  # Scatter plot for raw data
  # geom_line(data = new_data_filtered, aes(x = vessel_density_per_mm2_Tige, y = predicted_mean_vessel_size, color = species), linewidth = 1) +  # Prediction lines
  # labs(
  #   x = "Stem Vessel Density (per mm²)",
  #   y = "Stem Mean Vessel Size (µm²)",
  #   color = "Species"
  # ) +
  theme_minimal()


anarchi_no_Ho_no_ext <- anarchi_no_Ho %>%
  filter(vessel_grouping_Racine < 4)
