# =============================================================================
# 03_flux_insee.R
# Coût résidentiel avec les vrais lieux de travail (INSEE, mobilités pro 2022)
# Pour chaque commune de résidence : distance moyenne domicile-travail,
# pondérée par le nombre d'actifs allant travailler dans chaque commune.
# Pré-requis : 01_prix_dvf.R et 02_cout_residentiel.R déjà lancés
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(sf)

# --- 0. Hypothèses ---------------------------------------------------------------
hyp <- list(
  surface_m2       = 70,
  taux_credit      = 0.035,
  duree_credit     = 20,
  jours_travailles = 220,
  cout_km          = 0.30,
  facteur_route    = 1.3,
  dist_min_km      = 3,     # trajet quand on travaille dans sa propre commune
  dist_max_km      = 100,   # au-delà : pas un trajet quotidien, flux exclu
  code_dijon       = "21231"
)

# --- 1. Téléchargement du fichier INSEE ---------------------------------------------
url_insee <- "https://www.insee.fr/fr/statistiques/fichier/8582949/base-flux-mobilite-domicile-lieu-travail-2022_csv.zip"
zip_dest  <- "data/raw/flux_mobilite_pro_2022.zip"
if (!file.exists(zip_dest)) {
  message("Téléchargement du fichier INSEE (quelques Mo)...")
  download.file(url_insee, zip_dest, mode = "wb")
}
unzip(zip_dest, exdir = "data/raw/flux_insee")

# Le zip contient le fichier de données et parfois un fichier de métadonnées :
# on prend le plus gros fichier .csv
csvs <- list.files("data/raw/flux_insee", pattern = "\\.csv$", full.names = TRUE)
f    <- csvs[which.max(file.size(csvs))]
sep  <- if (grepl(";", readLines(f, n = 1))) ";" else ","

flux <- read_delim(f, delim = sep, col_types = cols(.default = "c"), progress = FALSE)
col_flux <- grep("^NBFLUX", names(flux), value = TRUE)[1]
message("Colonne utilisée pour les effectifs : ", col_flux)

flux <- flux |>
  transmute(dep_residence = CODGEO,
            lieu_travail  = DCLT,
            actifs        = as.numeric(gsub(",", ".", .data[[col_flux]]))) |>
  filter(substr(dep_residence, 1, 2) == "21", actifs > 0)

message(round(sum(flux$actifs)), " actifs résidant en Côte-d'Or.")

# --- 2. Coordonnées des communes (lieux de travail) -----------------------------------
# Côte-d'Or + départements voisins : au-delà, les trajets dépassent 100 km
# et sont exclus de toute façon.
deps <- c("21", "89", "58", "71", "39", "70", "52", "10", "25")
centres_fr <- lapply(deps, function(d) {
  st_read(sprintf(
    "https://geo.api.gouv.fr/departements/%s/communes?fields=code&format=geojson&geometry=centre", d),
    quiet = TRUE)
}) |>
  bind_rows() |>
  st_transform(2154)
coords <- st_coordinates(centres_fr)
centres_fr <- tibble(code = centres_fr$code, x = coords[, 1], y = coords[, 2])

# Communes de résidence (Côte-d'Or), avec un point à l'intérieur de chaque commune
communes <- st_read(
  "https://geo.api.gouv.fr/departements/21/communes?format=geojson&geometry=contour",
  quiet = TRUE
) |>
  st_transform(2154)
c21 <- st_coordinates(st_point_on_surface(st_geometry(communes)))
centres_21 <- tibble(code = communes$code, x_r = c21[, 1], y_r = c21[, 2])

# --- 3. Distance de chaque flux ------------------------------------------------------
flux_dist <- flux |>
  left_join(centres_21, by = c("dep_residence" = "code")) |>
  left_join(centres_fr, by = c("lieu_travail" = "code")) |>
  mutate(dist_km = sqrt((x - x_r)^2 + (y - y_r)^2) / 1000 * hyp$facteur_route,
         dist_km = if_else(dep_residence == lieu_travail, hyp$dist_min_km,
                           pmax(dist_km, hyp$dist_min_km)))

part_non_localisee <- with(flux_dist, sum(actifs[is.na(dist_km)]) / sum(actifs))
part_longue <- with(flux_dist, sum(actifs[!is.na(dist_km) & dist_km > hyp$dist_max_km]) / sum(actifs))
message(sprintf("Actifs non localisés : %.1f %% ; trajets > %d km exclus : %.1f %%",
                100 * part_non_localisee, hyp$dist_max_km, 100 * part_longue))

# --- 4. Indicateurs par commune de résidence -------------------------------------------
mobilite <- flux_dist |>
  filter(!is.na(dist_km), dist_km <= hyp$dist_max_km) |>
  group_by(code = dep_residence) |>
  summarise(dist_moy_km      = weighted.mean(dist_km, actifs),
            part_sur_place   = sum(actifs[lieu_travail == dep_residence]) / sum(actifs),
            part_vers_dijon  = sum(actifs[lieu_travail == hyp$code_dijon]) / sum(actifs),
            actifs           = sum(actifs),   # en dernier : sinon il écrase le détail
            .groups = "drop")

# --- 5. Coût résidentiel avec les vrais trajets -------------------------------------------
prix <- read_csv("data/processed/prix_m2_communes_21.csv",
                 col_types = cols(code_commune = "c", nom_commune = "c"))
ancien <- read_csv("data/processed/cout_residentiel_21.csv",
                   col_types = cols(code = "c")) |>
  select(code, dist_dijon_km = dist_km, cout_total_dijon = cout_total)

r <- hyp$taux_credit
facteur_annuite <- r / (1 - (1 + r)^(-hyp$duree_credit))

cout <- communes |>
  left_join(prix,     by = c("code" = "code_commune")) |>
  left_join(mobilite, by = "code") |>
  left_join(ancien,   by = "code") |>
  mutate(cout_logement  = prix_m2_med * hyp$surface_m2 * facteur_annuite,
         cout_transport = 2 * dist_moy_km * hyp$jours_travailles * hyp$cout_km,
         cout_total     = cout_logement + cout_transport,
         part_transport = cout_transport / cout_total)

write_csv(st_drop_geometry(cout) |>
            select(code, nom, actifs, dist_moy_km, part_sur_place, part_vers_dijon,
                   dist_dijon_km, prix_m2_med, cout_logement, cout_transport,
                   cout_total, cout_total_dijon, part_transport),
          "data/processed/cout_residentiel_flux_21.csv")

# --- 6. Cartes ----------------------------------------------------------------------------
carte <- function(var, titre, legende, fichier, pct = FALSE) {
  etiquettes <- if (pct) scales::label_percent() else scales::label_number(big.mark = " ")
  g <- ggplot(cout) +
    geom_sf(aes(fill = .data[[var]]), colour = "white", linewidth = 0.05) +
    scale_fill_viridis_c(name = legende, na.value = "grey85", option = "magma",
                         direction = -1, labels = etiquettes) +
    labs(title = titre,
         caption = "Sources : INSEE (RP 2022, mobilités professionnelles), DVF, geo.api.gouv.fr ; calculs de l'auteur") +
    theme_void() +
    theme(plot.title = element_text(face = "bold"))
  ggsave(fichier, g, width = 8, height = 8, dpi = 200, bg = "white")
  g
}

carte("dist_moy_km", "Distance moyenne domicile-travail des actifs", "km",
      "output/carte_distance_moyenne_21.png")
carte("part_vers_dijon", "Part des actifs qui travaillent à Dijon", "%",
      "output/carte_part_dijon_21.png", pct = TRUE)
g_total <- carte("cout_total", "Coût résidentiel annuel avec les vrais lieux de travail", "€ / an",
                 "output/carte_cout_total_flux_21.png")

# --- 7. Comparaison avec l'hypothèse « tout le monde travaille à Dijon » ------------------
donnees <- st_drop_geometry(cout) |> filter(!is.na(cout_total), !is.na(cout_total_dijon))

g_comp <- ggplot(donnees, aes(x = dist_dijon_km)) +
  geom_smooth(aes(y = cout_total_dijon, colour = "Tous les actifs travaillent à Dijon"),
              se = FALSE, method = "loess") +
  geom_smooth(aes(y = cout_total, colour = "Vrais lieux de travail (INSEE)"),
              se = FALSE, method = "loess") +
  geom_point(aes(y = cout_total, colour = "Vrais lieux de travail (INSEE)"), alpha = 0.3, size = 1) +
  scale_colour_manual(values = c("Tous les actifs travaillent à Dijon" = "grey50",
                                 "Vrais lieux de travail (INSEE)" = "#d95f02"), name = NULL) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(title = "Coût résidentiel total selon la distance à Dijon",
       x = "Distance routière estimée à Dijon (km)", y = "Coût annuel logement + transport (€)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave("output/comparaison_hypotheses_21.png", g_comp, width = 8, height = 5.5,
       dpi = 200, bg = "white")
print(g_comp)

# --- 8. Résumé chiffré ----------------------------------------------------------------------
cat("\nPar tranche de distance à Dijon :\n")
donnees |>
  mutate(tranche = cut(dist_dijon_km, c(0, 10, 20, 30, 40, 60, 80, 200),
                       labels = c("<10", "10-20", "20-30", "30-40", "40-60", "60-80", ">80"))) |>
  group_by(tranche) |>
  summarise(communes          = n(),
            dist_trajet_km    = round(weighted.mean(dist_moy_km, actifs)),
            pct_vers_dijon    = round(100 * weighted.mean(part_vers_dijon, actifs)),
            logement          = round(mean(cout_logement)),
            transport         = round(mean(cout_transport)),
            total             = round(mean(cout_total)),
            total_si_dijon    = round(mean(cout_total_dijon))) |>
  print()
