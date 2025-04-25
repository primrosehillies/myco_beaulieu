archi <- read.csv(file = "archi1.csv", header = T, stringsAsFactors = T, dec = ",")

library(multcomp)
library(lmtest)



#####Species effect####
mod1 <- lm(diam_tig_princip_mm ~ species, data = archi)
mod1_aov <- aov(diam_tig_princip_mm ~ species, data = archi)

anova(mod1)
plot(mod1)
#kind of good, 3 outliers
summary(mod1)
boxplot(diam_tig_princip_mm ~ species, data = archi)



tuk1 <- glht(mod1_aov, linfct = mcp(species = "Tukey"))
# test de Tukey:
summary(tuk1) 

tuk.cld1 <- cld(tuk1, decreasing = TRUE)   
tuk.cld1 $mcletters$Letters
# old.par <- par(mai=c(1,1,2,1), no.readonly=TRUE)
# plot(tuk.cld)




library(tidyverse)
archi %>%
  arrange(prof_pivot_mm)

library(ggplot2)

#Profondeur de pivot
archi %>%
  arrange(prof_pivot_mm) %>%
  ggplot(aes(x = reorder(num, prof_pivot_mm), y = prof_pivot_mm)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Profondeur de pivot (mm)") +
  theme_minimal()

archi %>%
  mutate(prof_pivot_log = log(prof_pivot_mm)) %>%
  arrange(prof_pivot_log) %>%
  ggplot(aes(x = reorder(num, prof_pivot_log), y = prof_pivot_log)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Log Profondeur de pivot (mm)") +
  theme_minimal()


#Diamètre tige principale
archi %>%
  arrange(diam_tig_princip_mm) %>%
  ggplot(aes(x = reorder(num, diam_tig_princip_mm), y = diam_tig_princip_mm)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Diamètre tige principale (mm)") +
  theme_minimal()

archi %>%
  mutate(diam_tig_princip_log = log(diam_tig_princip_mm)) %>%
  arrange(diam_tig_princip_log) %>%
  ggplot(aes(x = reorder(num, diam_tig_princip_log), y = diam_tig_princip_log)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Log Diamètre tige principale (mm)") +
  theme_minimal()



plot(log(archi$diam_tig_princip_mm), log(archi$prof_pivot_mm))

archi <- archi %>%
  mutate(prof_pivot_log = log(prof_pivot_mm)) %>%
  mutate(diam_tig_princip_log = log(diam_tig_princip_mm))



#######
mod10 <- lm(prof_pivot_log ~ diam_tig_princip_log, data = archi)
plot(mod10)
shapiro.test(mod10$residuals)
bptest(mod10)
dwtest(mod10)
#p < 0.05, il y a auto-corrélation des résidus du modèle linéaire. Sinon, tout semble bon

anova(mod10)
summary(mod10)     

plot(prof_pivot_log ~ diam_tig_princip_log, data = archi, col="blue")
abline(mod10, col="red")



#######
mod11 <- lm(prof_pivot_log ~ diam_tig_princip_log * species, data = archi)
plot(mod11)
shapiro.test(mod11$residuals)
bptest(mod11)
dwtest(mod11)
#p passe > 0.05, il n'y a plus auto-corrélation des résidus du modèle linéaire.

anova(mod11)
summary(mod11)     
#R² : 0.25


# Define the range for the predictor
diam_range <- seq(min(archi$diam_tig_princip_log, na.rm = TRUE), 
                  max(archi$diam_tig_princip_log, na.rm = TRUE), 
                  length.out = 100)

# Create a new data frame for predictions
new_data <- expand.grid(diam_tig_princip_log = diam_range,
                        species = unique(archi$species))

# Get predictions
new_data$predicted_prof <- predict(mod11, newdata = new_data)

species_colors <- c("red", "green", "purple")  # Adjust colors as needed
names(species_colors) <- unique(archi$species)

# Plot the original data with points colored by species
plot(prof_pivot_log ~ diam_tig_princip_log, data = archi, pch = 16,
     col = species_colors[archi$species], 
     xlab = "diam_tig_princip_log", ylab = "prof_pivot_log")

# Add lines for each species
species_colors <- c("red", "green", "purple")  # Set a color for each species
for(i in seq_along(unique(new_data$species))) {
  species <- unique(new_data$species)[i]
  lines(new_data$diam_tig_princip_log[new_data$species == species], 
        new_data$predicted_prof[new_data$species == species], 
        col = species_colors[i], lwd = 2)
}

# Add a legend
legend("topright", legend = unique(new_data$species), col = species_colors, 
       lwd = 2, title = "Species")



#######
mod12 <- lm(prof_pivot_log ~ diam_tig_princip_log * site_type, data = archi)
plot(mod12)
shapiro.test(mod12$residuals)
bptest(mod12)
dwtest(mod12)
#p passe > 0.05, il n'y a plus auto-corrélation des résidus du modèle linéaire.

anova(mod12)
#Interaction pas significative : on l'enlève

mod13 <- lm(prof_pivot_log ~ diam_tig_princip_log + site_type, data = archi)
plot(mod13)
shapiro.test(mod13$residuals)
bptest(mod13)
dwtest(mod13)

anova(mod13)
summary(mod13)     
#R² : 0.14



# Define the range for the predictor
diam_range13 <- seq(min(archi$diam_tig_princip_log, na.rm = TRUE), 
                  max(archi$diam_tig_princip_log, na.rm = TRUE), 
                  length.out = 100)

# Create a new data frame for predictions
new_data13 <- expand.grid(diam_tig_princip_log = diam_range,
                        site_type = unique(archi$site_type))

# Get predictions
new_data13$predicted_prof <- predict(mod13, newdata = new_data13)

site_colors <- c("red", "green", "purple")  
names(site_colors) <- unique(archi$site_type)

# Plot the original data with points colored by species
plot(prof_pivot_log ~ diam_tig_princip_log, data = archi, pch = 16,
     col = site_colors[archi$site_type], 
     xlab = "diam_tig_princip_log", ylab = "prof_pivot_log")

# Add lines for each species
site_colors <- c("red", "green", "purple")  
for(i in seq_along(unique(new_data13$site_type))) {
  site_type <- unique(new_data13$site_type)[i]
  lines(new_data13$diam_tig_princip_log[new_data13$site_type == site_type], 
        new_data13$predicted_prof[new_data13$site_type == site_type], 
        col = site_type[i], lwd = 2)
}

# Add a legend
legend("topright", legend = unique(new_data13$site_type), col = site_colors, 
       lwd = 2, title = "Species")



#####



archi %>%
  arrange(diam_tig_lat_mm) %>%
  ggplot(aes(x = reorder(num, diam_tig_lat_mm), y = diam_tig_lat_mm)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Diamètre tige principale (mm)") +
  theme_minimal()

archi %>%
  mutate(diam_tig_lat_log = log(diam_tig_lat_mm)) %>%
  arrange(diam_tig_lat_log) %>%
  ggplot(aes(x = reorder(num, diam_tig_lat_log), y = diam_tig_lat_log)) +
  geom_bar(stat = "identity") +
  labs(x = "ID", y = "Log Diamètre tige principale (mm)") +
  theme_minimal()



archi <- archi %>%
  mutate(diam_tig_lat_log = log(diam_tig_lat_mm))



mod14 <- lm(diam_tig_princip_log ~ diam_tig_lat_log, data = archi)
plot(mod14)
shapiro.test(mod14$residuals)
bptest(mod14)
dwtest(mod14)
#p passe > 0.05, il n'y a plus auto-corrélation des résidus du modèle linéaire.

anova(mod14)
summary(mod14)

plot(diam_tig_princip_log ~ diam_tig_lat_log, data = archi)
abline(mod14)