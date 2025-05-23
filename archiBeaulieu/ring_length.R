library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)

anat_long <- read.csv(file = "anat_long.csv", header = T, stringsAsFactors = F)
anat_long <- anat_long %>%
  select(-X)

species <- c("Ho", "Fe", "Ft")

anat_sp <- map(species, ~filter(anat_long, species == .x)) %>%
  set_names(species) %>%
  map(~ mutate(.x, organ = as.factor(organ)))

anat_sp_organ <- imap(anat_sp, function(df, name) {
  split(df, df$organ) %>% 
    set_names(~ paste0(name, "_", .x))  
}) %>%
  flatten()


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
  
  # Ensure year is a factor with levels in order
  data <- data %>%
    mutate(!!year_col := factor(.data[[year_col]], levels = c(2021, 2022, 2023, 2024)))
  
  # Generate plot
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

