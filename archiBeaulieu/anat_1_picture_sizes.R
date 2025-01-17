library(jpeg)

# list JPG files in a directory
square_files <- list.files(path = "square", pattern = "\\.jpg$", full.names = TRUE)

#function to get dimensions of an image
get_image_dimensions <- function(image_path) {
  img <- readJPEG(image_path, native = TRUE)  # Load the image
  list(
    file = basename(image_path),              # File name
    width_px = attr(img, "dim")[2],              # Width
    height_px = attr(img, "dim")[1]              # Height
  )
}

#function to all images
image_dimensions <- lapply(square_files, get_image_dimensions)

#get data frame
image_dimensions_df <- do.call(rbind.data.frame, image_dimensions)



image_dimensions_df$id_complete <- gsub("_square\\.jpg$", "", image_dimensions_df$file)

image_dimensions_df$id_amap <- substr(image_dimensions_df$id_complete, 1, 4)


image_dimensions_df$id_complete2 <- ifelse(
  nchar(image_dimensions_df$id_complete) == 28,
  paste0(substr(image_dimensions_df$id_complete, 1, 12), "0", substr(image_dimensions_df$id_complete, 13, nchar(image_dimensions_df$id_complete))),
  image_dimensions_df$id_complete
)
image_dimensions_df$id_complete3 <- ifelse(
  nchar(image_dimensions_df$id_complete2) == 30,
  paste0(substr(image_dimensions_df$id_complete2, 1, 12), "0", substr(image_dimensions_df$id_complete2, 13, nchar(image_dimensions_df$id_complete2))),
  image_dimensions_df$id_complete2
)


image_dimensions_df$id_complete4 <- gsub("Tige", "T", image_dimensions_df$id_complete3)
image_dimensions_df$id_complete5 <- gsub("Racine", "R", image_dimensions_df$id_complete4)


image_dimensions_df$id_complete6 <- gsub("-", "_", image_dimensions_df$id_complete5)

split_names <- strsplit(image_dimensions_df$id_complete6, "_")
split_names

image_dimensions_df$site <- sapply(split_names, function(x) x[2])      # Second element (T1)
image_dimensions_df$species <- sapply(split_names, function(x) x[3])  # Third element (Ho2)
image_dimensions_df$id_lucas <- sapply(split_names, function(x) x[4]) # Fourth element (03)
image_dimensions_df$organ <- sapply(split_names, function(x) x[5])    # Fifth element (T)
image_dimensions_df$zoom <- sapply(split_names, function(x) x[6])     # Sixth element (X200)
image_dimensions_df$coupe_µm <- sapply(split_names, function(x) x[7]) # Seventh element (09µm)


image_dimensions_df$site_type <- substr(image_dimensions_df$site, 1, 1)  # First character (T)
image_dimensions_df$site_replicate <- substr(image_dimensions_df$site, 2, nchar(image_dimensions_df$site))  # Remaining characters (1)


image_dimensions_df$micro <- ifelse(grepl("[a-z]", image_dimensions_df$site_type), 1, 0)

image_dimensions_df$site_type <- toupper(image_dimensions_df$site_type)


image_dimensions_df$species_name <- substr(image_dimensions_df$species, 1, 2)  # First two characters (e.g., "Ho")
image_dimensions_df$species_site_replicate <- substr(image_dimensions_df$species, 3, nchar(image_dimensions_df$species))  # Remaining characters (e.g., "1")


image_dimensions_df$width_µm <- image_dimensions_df$width_px / 1.9111
image_dimensions_df$height_µm <- image_dimensions_df$height_px / 1.9111

image_dimensions_df$image_surface_µm2 <- image_dimensions_df$width_µm * image_dimensions_df$height_µm

anat <- data.frame(
  file = image_dimensions_df$file,
  id_complete = image_dimensions_df$id_complete6,
  image_surface_µm2 = image_dimensions_df$image_surface_µm2,
  id_amap = image_dimensions_df$id_amap,
  number = image_dimensions_df$id_lucas,
  site = image_dimensions_df$site,
  site_type = image_dimensions_df$site_type,
  site_replicate = image_dimensions_df$site_replicate,
  species_site_replicate = image_dimensions_df$species,
  species = image_dimensions_df$species_name,
  species_replicate = image_dimensions_df$species_site_replicate,
  micro = image_dimensions_df$micro,
  organ = image_dimensions_df$organ,
  
  
  stringsAsFactors = FALSE
)
