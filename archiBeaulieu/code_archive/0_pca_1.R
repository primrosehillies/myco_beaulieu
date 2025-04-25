library(psych)
library(ade4)
library(missMDA)


########PCA n°1####

pairs.panels(archi[,c("diam_tig_princip_mm", "diam_tig_lat_mm", "diam_rac_pivot_mm", "diam_rac_lat_mm", "conicite_mm", "empreinte_photo_mm", "prof_pivot_mm", "y_photo_mm")],
             method = "spearman", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

archi_imputed <- imputePCA(archi[, c("diam_tig_princip_mm", "diam_tig_lat_mm", "diam_rac_pivot_mm", 
                                     "diam_rac_lat_mm", "conicite_mm", "empreinte_photo_mm", 
                                     "prof_pivot_mm", "y_photo_mm")])$completeObs

archi_imputed <- as.data.frame(archi_imputed)
numeric_columns <- names(archi_imputed) != "species"
archi_imputed[numeric_columns] <- lapply(archi_imputed[numeric_columns], as.numeric)


species <- as.character(archi$species)
site <- as.character(archi$site_type)

archi_imputed <- cbind(archi_imputed, species)
archi_imputed <- cbind(archi_imputed, site)



acp1 <- dudi.pca(archi_imputed[,1:8], scale = TRUE, center = TRUE)
# Sous objets de resultats de l'ACP
names(acp1)
# Valeurs propres (VP) : chaque valeur propre est associé à un vecteur propre. Le vecteur propre donne la direction de la variation (direction de l'axe dans l'espace) et la valeur propre associée donne la quantité de variation sur cet axe.
acp1$eig
#Corrélation de chaque variable avec chaque axe
acp1$co





s.corcircle(acp1$co)
s.label(acp1$li[,1:2], clabel=0.5)



species_col <- archi_imputed[, "species"]
species_col <- archi_imputed[, 9]

unique_species <- unique(species_col)
color_vector <- 1:length(unique_species)

# Plot with s.class
s.class(acp1$li, fac = factor(species_col), col = color_vector)

site_col <- archi_imputed[, "site"]

unique_site <- unique(site_col)
site_vector <- 1:length(unique_site)
# Plot with s.class
s.class(acp1$li, fac = factor(site_col), col = site_vector)

########PCA n°2####
archi_imputed2 <- imputePCA(archi[,c("aire_conic", "prof_pivot_mm", 
                                     "tig_ratio", "rac_ratio",
                                     "surface", "inflo")])$completeObs



archi_imputed2 <- as.data.frame(archi_imputed2)
numeric_columns <- names(archi_imputed2) != "species"
archi_imputed2[numeric_columns] <- lapply(archi_imputed2[numeric_columns], as.numeric)


archi_imputed2 <- cbind(archi_imputed2, species)
archi_imputed2 <- cbind(archi_imputed2, site)


acp2 <- dudi.pca(archi_imputed2[,1:6], scale = TRUE, center = TRUE)

acp2$eig

s.corcircle(acp2$co)
s.label(acp2$li[,1:2], clabel=0.5)



s.class(acp2$li, fac = factor(species_col), col = color_vector)
s.class(acp2$li, fac = factor(site_col), col = site_vector)

