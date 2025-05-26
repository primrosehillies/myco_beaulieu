library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)

anat_long <- read.csv(file = "anat_long.csv", header = T, stringsAsFactors = F)
anat_long <- anat_long %>%
  select(-X)
metadata <- read.csv(file = "metadata.csv", header = T, stringsAsFactors = F)


species <- c("Ho", "Fe", "Ft")

anat_sp <- map(species, ~filter(anat_long, species == .x)) %>%
  set_names(species) %>%
  map(~ mutate(.x, organ = as.factor(organ)))

anat_sp_organ <- imap(anat_sp, function(df, name) {
  split(df, df$organ) %>% 
    set_names(~ paste0(name, "_", .x))  
}) %>%
  flatten()

anat_0to1 <- anat_sp_organ %>%
  map(~mutate(.x,
              ring_length_n = (ring_length_µm - min(ring_length_µm, na.rm = TRUE)) / (max(ring_length_µm, na.rm = TRUE) - min(ring_length_µm, na.rm = TRUE)),
              mean_vessel_size_n = (mean_vessel_size_µm2 - min(mean_vessel_size_µm2, na.rm = TRUE)) / (max(mean_vessel_size_µm2, na.rm = TRUE) - min(mean_vessel_size_µm2, na.rm = TRUE)),
              vessel_density_n = (vessel_density_per_mm2 - min(vessel_density_per_mm2, na.rm = TRUE)) / (max(vessel_density_per_mm2, na.rm = TRUE) - min(vessel_density_per_mm2, na.rm = TRUE)),
              Kth_n = (Kth - min(Kth, na.rm = TRUE)) / (max(Kth, na.rm = TRUE) - min(Kth, na.rm = TRUE))
              )
      )
  

##################
#####per year#####
##################
#####Ring length####


plot_ring_length_by_year <- function(data, year_col = "year", value_col = "ring_length_µm",
                                     facet_col = "site_type") {

  dataset_full <- deparse(substitute(data))
  dataset_id <- tail(strsplit(dataset_full, "\\$|\\[\\[|\\]\\]")[[1]], 1)
  

  species_lookup <- c(
    Fe = "Fumana ericifolia",
    Ft = "Fumana thymifolia",
    Ho = "Helianthemum oelandicum"
  )
  
  organ_lookup <- c(
    T = "Stem",
    R = "Root"
  )
  
  
  parts <- unlist(strsplit(dataset_id, "_"))
  species_code <- parts[1]
  organ_code <- parts[2]
  
  species_name <- species_lookup[[species_code]]
  organ_name <- organ_lookup[[organ_code]]
  
  title_text <- paste(species_name, "–", organ_name, ": Ring length per year according to site type")
  
  data <- data %>%
    mutate(!!year_col := factor(.data[[year_col]], levels = c(2021, 2022, 2023, 2024)))
  
  ggplot(data, aes(x = .data[[year_col]], y = .data[[value_col]])) +
    geom_boxplot() +
    facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x", nrow = 1) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Ring Length (µm)",
      title = title_text
    )
}

plot_ring_length_by_year(anat_sp_organ$Fe_T)
plot_ring_length_by_year(anat_sp_organ$Fe_R)

plot_ring_length_by_year(anat_sp_organ$Ft_T)
plot_ring_length_by_year(anat_sp_organ$Ft_R)

plot_ring_length_by_year(anat_sp_organ$Ho_T)
plot_ring_length_by_year(anat_sp_organ$Ho_R)

#####Vessel size####

plot_vessel_size_by_year <- function(data, year_col = "year", value_col = "mean_vessel_size_µm2",
                                     facet_col = "site_type") {
  
  dataset_full <- deparse(substitute(data))
  dataset_id <- tail(strsplit(dataset_full, "\\$|\\[\\[|\\]\\]")[[1]], 1)
  
  
  species_lookup <- c(
    Fe = "Fumana ericifolia",
    Ft = "Fumana thymifolia",
    Ho = "Helianthemum oelandicum"
  )
  
  organ_lookup <- c(
    T = "Stem",
    R = "Root"
  )
  
  
  parts <- unlist(strsplit(dataset_id, "_"))
  species_code <- parts[1]
  organ_code <- parts[2]
  
  species_name <- species_lookup[[species_code]]
  organ_name <- organ_lookup[[organ_code]]
  
  title_text <- paste(species_name, "–", organ_name, ": Vessel size per year according to site type")
  
  
  data <- data %>%
    mutate(!!year_col := factor(.data[[year_col]], levels = c(2021, 2022, 2023, 2024)))
  
  
  ggplot(data, aes(x = .data[[year_col]], y = .data[[value_col]])) +
    geom_boxplot() +
    facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x", nrow = 1) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Vessel size (µm²)",
      title = title_text
    )
}

plot_vessel_size_by_year(anat_sp_organ$Fe_T)
plot_vessel_size_by_year(anat_sp_organ$Fe_R)

plot_vessel_size_by_year(anat_sp_organ$Ft_T)
plot_vessel_size_by_year(anat_sp_organ$Ft_R)

plot_vessel_size_by_year(anat_sp_organ$Ho_T)
plot_vessel_size_by_year(anat_sp_organ$Ho_R)



#####Vessel density####
plot_vessel_density_by_year <- function(data, year_col = "year", value_col = "vessel_density_per_mm2",
                                     facet_col = "site_type") {
  
  dataset_full <- deparse(substitute(data))
  dataset_id <- tail(strsplit(dataset_full, "\\$|\\[\\[|\\]\\]")[[1]], 1)
  
  
  species_lookup <- c(
    Fe = "Fumana ericifolia",
    Ft = "Fumana thymifolia",
    Ho = "Helianthemum oelandicum"
  )
  
  organ_lookup <- c(
    T = "Stem",
    R = "Root"
  )
  
  
  parts <- unlist(strsplit(dataset_id, "_"))
  species_code <- parts[1]
  organ_code <- parts[2]
  
  species_name <- species_lookup[[species_code]]
  organ_name <- organ_lookup[[organ_code]]
  
  title_text <- paste(species_name, "–", organ_name, ": Vessel density per year according to site type")
  
  
  data <- data %>%
    mutate(!!year_col := factor(.data[[year_col]], levels = c(2021, 2022, 2023, 2024)))
  
  # Generate plot
  ggplot(data, aes(x = .data[[year_col]], y = .data[[value_col]])) +
    geom_boxplot() +
    facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x", nrow = 1) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Vessel density (mm ^-2)",
      title = title_text
    )
}

plot_vessel_density_by_year(anat_sp_organ$Fe_T)
plot_vessel_density_by_year(anat_sp_organ$Fe_R)

plot_vessel_density_by_year(anat_sp_organ$Ft_T)
plot_vessel_density_by_year(anat_sp_organ$Ft_R)

plot_vessel_density_by_year(anat_sp_organ$Ho_T)
plot_vessel_density_by_year(anat_sp_organ$Ho_R)



#####Conductivity####
plot_conductivity_by_year <- function(data, year_col = "year", value_col = "Kth",
                                        facet_col = "site_type") {
  
  dataset_full <- deparse(substitute(data))
  dataset_id <- tail(strsplit(dataset_full, "\\$|\\[\\[|\\]\\]")[[1]], 1)
  
  
  species_lookup <- c(
    Fe = "Fumana ericifolia",
    Ft = "Fumana thymifolia",
    Ho = "Helianthemum oelandicum"
  )
  
  organ_lookup <- c(
    T = "Stem",
    R = "Root"
  )
  
  
  parts <- unlist(strsplit(dataset_id, "_"))
  species_code <- parts[1]
  organ_code <- parts[2]
  
  species_name <- species_lookup[[species_code]]
  organ_name <- organ_lookup[[organ_code]]
  
  title_text <- paste(species_name, "–", organ_name, ": Theoretical conductivity (Kth) per year according to site type")
  
  
  data <- data %>%
    mutate(!!year_col := factor(.data[[year_col]], levels = c(2021, 2022, 2023, 2024)))
  
  
  ggplot(data, aes(x = .data[[year_col]], y = .data[[value_col]])) +
    geom_boxplot() +
    facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x", nrow = 1) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Kth (kg m-1 MPa-1 s-1)",
      title = title_text
    )
}

plot_conductivity_by_year(anat_sp_organ$Fe_T)
plot_conductivity_by_year(anat_sp_organ$Fe_R)

plot_conductivity_by_year(anat_sp_organ$Ft_T)
plot_conductivity_by_year(anat_sp_organ$Ft_R)

plot_conductivity_by_year(anat_sp_organ$Ho_T)
plot_conductivity_by_year(anat_sp_organ$Ho_R)


##################
#####all years####
##################
anat_all_years <- anat_sp_organ %>%
  map(~mutate(.x, 
              sizeXcount = mean_vessel_size_µm2 * count_vessel,
              densityXcount = vessel_density_per_mm2 * count_vessel,
              KthXcount = Kth * count_vessel
              )
      ) %>%
  map(~group_by(.x, number)) %>%
  map(~summarize(.x,
                 vessel_total = sum(count_vessel),
                 ring_length_µm = sum(ring_length_µm),
                 mean_vessel_size_µm2 = sum(sizeXcount) / sum(vessel_total),
                 vessel_density_per_mm2 = sum(densityXcount) / sum(vessel_total),
                 Kth = sum(KthXcount) / sum(vessel_total)
                   )
      ) %>%
  map(~mutate_if(.x, is.integer, as.character)) %>%
  map(~mutate(.x, number = ifelse(nchar(number) == 1, paste0("0", number), number))) %>%
  map(~left_join(.x, metadata, by = "number"))


#Make a dataset for stem and root data:
anat_T <- anat_0to1[names(anat_sp_organ) %>% str_detect("_T")] %>%
  bind_rows(.id = "source")
anat_T <- anat_T %>%
  group_by(site_type, species, year) %>%
  summarize(ring_length = mean(ring_length_µm, na.rm=TRUE),
            ring_length_sd = sd(ring_length_µm, na.rm=TRUE),
            vessel_size = mean(mean_vessel_size_µm2, na.rm=TRUE),
            vessel_size_sd = sd(mean_vessel_size_µm2, na.rm=TRUE),
            vessel_density = mean(vessel_density_per_mm2, na.rm=TRUE),
            vessel_density_sd = sd(vessel_density_per_mm2, na.rm=TRUE),
            Kth_mean = mean(Kth, na.rm=TRUE),
            Kth_mean_sd = sd(Kth, na.rm=TRUE)
            )
anat_T_long <- anat_T %>%
  pivot_longer(
    cols = c(ring_length, vessel_size, vessel_density, Kth_mean),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_longer(
    cols = c(ring_length_sd, vessel_size_sd, vessel_density_sd, Kth_mean_sd),
    names_to = "sd_variable",
    values_to = "sd"
  ) %>%
  filter(sub("_sd$", "", sd_variable) == variable)  # Match each variable with its correct SD


ggplot(anat_T_long, aes(x = year, y = value, color = site_type, group = site_type)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.2, na.rm = TRUE) +
  facet_grid(variable ~ species, scales = "free_y") +
  labs(x = "Year", y = "Measurement Value", color = "Site Type") +
  theme_minimal()


anat_R <- anat_0to1[names(anat_sp_organ) %>% str_detect("_R")] %>%
  bind_rows(.id = "source")

anat_R <- anat_R %>%
  group_by(site_type, species, year) %>%
  summarize(ring_length = mean(ring_length_µm, na.rm=TRUE),
            ring_length_sd = sd(ring_length_µm, na.rm=TRUE),
            vessel_size = mean(mean_vessel_size_µm2, na.rm=TRUE),
            vessel_size_sd = sd(mean_vessel_size_µm2, na.rm=TRUE),
            vessel_density = mean(vessel_density_per_mm2, na.rm=TRUE),
            vessel_density_sd = sd(vessel_density_per_mm2, na.rm=TRUE),
            Kth_mean = mean(Kth, na.rm=TRUE),
            Kth_mean_sd = sd(Kth, na.rm=TRUE)
  )
anat_R_long <- anat_R %>%
  pivot_longer(
    cols = c(ring_length, vessel_size, vessel_density, Kth_mean),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_longer(
    cols = c(ring_length_sd, vessel_size_sd, vessel_density_sd, Kth_mean_sd),
    names_to = "sd_variable",
    values_to = "sd"
  ) %>%
  filter(sub("_sd$", "", sd_variable) == variable)  # Match each variable with its correct SD



ggplot(anat_R_long, aes(x = year, y = value, color = site_type, group = site_type)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.2, na.rm = TRUE) +
  facet_grid(variable ~ species, scales = "free_y") +
  labs(x = "Year", y = "Measurement Value", color = "Site Type") +
  theme_minimal()


