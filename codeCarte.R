# Installation des packages nécessaires

install.packages(c("geodata", "sf", "spdep", "sfdep",
                   "tmap", "ggplot2",
                   "leaflet", "spatialreg", "KernSmooth",
                   "RColorBrewer",
                   "viridis", "dplyr", "tidyr",
                   "patchwork", "gtsummary",
                   "flextable", "scales", "classInt"))

library(geodata) # Importation des données GADM
library(sf) # Manipulation des données spatiales
library(spdep) # Poids spatiaux et autocorrélation
library(sfdep) # Interface tidy pour spdep
library(tmap) # Cartographie thématique
library(ggplot2) # Visualisation avancée
library(leaflet) # Cartes interactives
library(spatialreg) # Modèles de régression spatiale
library(KernSmooth) # Lissage par noyau spatial
library(RColorBrewer)# Palettes de couleurs cartographiques
library(viridis) # Palettes perceptuellement uniformes
library(dplyr) # Manipulation de données
library(classInt) # Discrétisation pour cartographie


# Importation des niveaux adminstratifs de la Guinée

## Niveau 0
pays <- gadm(country = "GN", level = 0, tempdir())

## Conversion de l'objet en sf

Gn0 <- st_as_sf(pays)

## Niveau 1
GN1 <- gadm(country = "GN",level = 1, tempdir())
Gn1 <- st_as_sf(GN1)

## Niveau 2
GN2 <- gadm(country = "GN",level = 2, tempdir())
Gn2 <- st_as_sf(GN2)

## Niveau 3
GN3 <- gadm(country = "GN",level = 3, tempdir())
Gn3 <- st_as_sf(GN3)

## Reprojection sur le vrai système de coordonnées: Gunéée: Pour la Guinée (Conakry), 
#les systèmes de coordonnées les plus utilisés sont UTM zones 28N et 29N, 
#soit en WGS84 (EPSG:32628/32629)

Gn2_projection= st_transform(Gn2,crs=32628)

## Calcul de la superficie en km2
Gn2_projection$superficie <-as.numeric(st_area(Gn2_projection))/1e6

## Calcul des centroîdes
centroïdes <- st_centroid(Gn2_projection)
centroïdes_coords <- st_coordinates(centroïdes)
Gn2_projection$lon_centroide <- centroïdes_coords[, 1]
Gn2_projection$lat_centroide <- centroïdes_coords[, 2]


cat('Superficie totale de la Guinée (km²) :',
    round(sum(Gn2_projection$superficie), 0), '\n')

#  Fusion avec les géométries
Guinee_sante <- Gn2_projection %>%
  left_join(prefecture, by = "NAME_2")

##  Calcul des taux

Guinee_sante$`Population(moins de 5ans)` <- as.numeric(gsub(" ", "", Guinee_sante$`Population(moins de 5ans)`))
Guinee_sante$`Deces(enfants moins de 5 ans)` <- as.numeric(gsub(" ", "", Guinee_sante$`Deces(enfants moins de 5 ans)`))

# Vérifier qu'il n'y a plus de NA
sum(is.na(Guinee_sante$`Population(moins de 5ans)`))
sum(is.na(Guinee_sante$`Deces(enfants moins de 5 ans)`))
# Ensuite calculer le taux
Guinee_sante$taux_mortalite_5ans <- (Guinee_sante$`Deces(enfants moins de 5 ans)` / 
                                       Guinee_sante$`Population(moins de 5ans)`) * 1000


deces <- Guinee_sante$taux_mortalite_5ans
# 1. Quantiles (effectifs égaux dans chaque classe)

breaks_quantiles <- classIntervals(deces, n = 5,
                                   style = 'quantile')
# 2. Jenks (ruptures naturelles — minimise la variance intra-classe)

breaks_jenks <- classIntervals(deces, n = 5, style
                               = 'jenks')
# 3. Intervalles égaux
breaks_equal <- classIntervals(deces, n = 5, style
                              = 'equal')
# 4. Écart-type (centre sur la moyenne)

breaks_sd <- classIntervals(deces, n = 5, style =
                              'sd')
# 5. Seuils épidémiologiques personnalisés
breaks_epi <- classIntervals(deces, style = 'fixed',
                             fixedBreaks = c(0, 50, 100,150, 200, 300))



# ─── Comparaison des seuils
cat('Seuils Quantiles :', round(breaks_quantiles$brks, 1),
    '\n')
cat('Seuils Jenks :', round(breaks_jenks$brks, 1),
    '\n')
cat('Seuils Égaux :', round(breaks_equal$brks, 1),
    '\n')
cat('Seuils Épidémio. :', breaks_epi$brks, '\n')


# Cartographie Choroplèthe avec tmap

# ─── Configuration globale de tmap

tmap_mode('plot') # Mode statique (vs 'view' pour interactif)

# Carte choroplèthe : Incidence du paludisme par commune

GN2 <- merge(GN2, st_drop_geometry(Guinee_sante[, c("NAME_2", "taux_mortalite_5ans")]), by = "NAME_2")
GN2$Nom_commune <- paste0(GN2$NAME_2, "(", round(as.numeric(GN2$taux_mortalite_5ans), 1), ")")

carte_deces <- tm_shape(Guinee_sante) +
  tm_polygons(
    col = "taux_mortalite_5ans",
    style = "jenks",
    n = 5,
    palette = "PuBu",
    title = "Taux de mortalité pour 1000 enfants",
    border.col = 'white',
    border.lwd = 0.3,
    lwd = 0.3
  ) +
  tm_shape(GN2) +
  tm_borders(
    col = 'black',
    lwd = 1.5
  ) +
  tm_text(
    text = "Nom_commune",   
    size = 0.4,
    col = 'black',
    fontface = "bold",
    shadow = TRUE
  ) +
  tm_compass(
    type = "arrow",
    position = c('right', 'top'),
    size = 2
  ) +
  tm_scale_bar(
    breaks = c(0, 50, 100, 200),
    position = c("left", "bottom")
  ) +
  tm_layout(
    main.title = "Taux de mortalité du paludisme par prefecture en 2022 en Guinée ",
    main.title.size = 1.5,
    main.title.position = "center",
    main.title.fontface = "bold",
    legend.position = c("left", "bottom"),
    legend.outside = FALSE,
    inner.margins = c(0.05, 0.05, 0.10, 0.05),
    frame = TRUE,
    bg.color = "white"
  ) +
  tm_credits(
    text = "Source : MSHP Guinee, 2023.",
    position = c("right", "bottom"),
    size = 0.5
  )

print(carte_deces)






















