library(psych)
library(tidyverse)
#anat_combined dataset in anat_3
archi <- read.csv("archi_2.csv", header =T, stringsAsFactors = T)
anat_combined <- read.csv("anat_combined_3.csv", header = T, stringsAsFactors = T)
metadata <- read.csv("metadata.csv", header = T, stringsAsFactors = T)
metadata$number <- as.character(metadata$number)
archi$number <- archi$num
anarchi <- archi %>%
  dplyr::select(-X, -code, -species, -site_type, -site_replicate, -site, -micro) %>%
  full_join(anat_combined, by = "number") %>%
  dplyr::select(-X)

anarchi$number <- as.character(anarchi$number)
anarchi <- anarchi %>%
  select(-site,-site_type, -site_replicate, -species_site_replicate, -species, -species_replicate, -micro) %>%
  left_join(metadata, by = "number")

write.csv(anarchi, "anarchi_2.csv")

