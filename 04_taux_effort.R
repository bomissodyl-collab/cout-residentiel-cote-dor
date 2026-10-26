# =============================================================================
# 04_taux_effort.R
# Taux d'effort logement + transport par commune et par profil de ménage
# Revenus : INSEE Filosofi 2023, niveau de vie médian par commune (MED_SL)
# Pré-requis : 03_flux_insee.R déjà lancé (data/processed/cout_residentiel_flux_21.csv)
# =============================================================================

if (!"tidyr" %in% rownames(installed.packages())) install.packages("tidyr")
library(dplyr)
library(readr)
library(ggplot2)
library(sf)

# --- 0. Hypothèses -----------------------------------------------------------------
hyp <- list(
  taux_credit      = 0.035,
  duree_credit     = 20,
  jours_travailles = 220,
  cout_km          = 0.30,
  seuil_effort     = 0.45   # seuil de l'indice américain H+T (logement + transport)
)

# Profils de ménages. UC = unités de consommation (échelle OCDE modifiée :
# 1 pour le premier adulte, 0,5 par autre adulte, 0,3 par enfant de moins de 14 ans)
profils <- tibble(
  profil      = c("Personne seule", "Couple biactif, 2 enfants"),
  uc          = c(1, 1 + 0.5 + 2 * 0.3),
  surface_m2  = c(40, 90),
  navetteurs  = c(1, 2)
)

# --- 1. Revenus Filosofi -----------------------------------------------------------------
url_filo <- "https://www.insee.fr/fr/statistiques/fichier/8984752/FILOSOFI_CC_csv.zip"
zip_filo <- "data/raw/filosofi_2023.zip"
if (!file.exists(zip_filo)) download.file(url_filo, zip_filo, mode = "wb")
unzip(zip_filo, exdir = "data/raw/filosofi")

csvs <- list.files("data/raw/filosofi", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
f    <- csvs[which.max(file.size(csvs))]
sep  <- if (grepl(";", readLines(f, n = 1))) ";" else ","
filo <- read_delim(f, delim = sep, col_types = cols(.default = "c"), progress = FALSE)
message("Colonnes du fichier Filosofi : ", paste(names(filo), collapse = ", "))

# Le fichier peut être au format « long » (une ligne par indicateur) ou « large »
if ("FILOSOFI_MEASURE" %in% names(filo)) {
  revenus <- filo |> filter(FILOSOFI_MEASURE == "MED_SL", substr(GEO, 1, 2) == "21")
  if ("GEO_OBJECT" %in% names(revenus))  revenus <- filter(revenus, GEO_OBJECT == "COM")
  if ("TIME_PERIOD" %in% names(revenus)) revenus <- filter(revenus, TIME_PERIOD == max(TIME_PERIOD))
  revenus <- revenus |>
    transmute(code = GEO, niveau_vie_med = as.numeric(gsub(",", ".", OBS_VALUE)))
} else {
  col_code <- intersect(c("GEO", "CODGEO"), names(filo))[1]
  revenus <- filo |>
    filter(substr(.data[[col_code]], 1, 2) == "21") |>
    transmute(code = .data[[col_code]], niveau_vie_med = as.numeric(gsub(",", ".", MED_SL)))
}
revenus <- revenus |> filter(!is.na(niveau_vie_med)) |> distinct(code, .keep_all = TRUE)
message(nrow(revenus), " communes de Côte-d'Or avec un niveau de vie médian.")

# --- 2. Coûts et taux d'effort par profil ---------------------------------------------------
base <- read_csv("data/processed/cout_residentiel_flux_21.csv", col_types = cols(code = "c")) |>
  select(code, nom, dist_moy_km, dist_dijon_km, prix_m2_med) |>
  left_join(revenus, by = "code")

r <- hyp$taux_credit
facteur_annuite <- r / (1 - (1 + r)^(-hyp$duree_credit))

effort <- tidyr::crossing(base, profils) |>
  mutate(revenu_menage  = niveau_vie_med * uc,
         cout_logement  = prix_m2_med * surface_m2 * facteur_annuite,
         cout_transport = navetteurs * 2 * dist_moy_km * hyp$jours_travailles * hyp$cout_km,
         taux_effort    = (cout_logement + cout_transport) / revenu_menage,
         au_dessus_seuil = taux_effort > hyp$seuil_effort)

write_csv(effort, "data/processed/taux_effort_21.csv")

# --- 3. Carte des taux d'effort, un panneau par profil -----------------------------------------
communes <- st_read(
  "https://geo.api.gouv.fr/departements/21/communes?format=geojson&geometry=contour",
  quiet = TRUE
)
carte <- communes |>
  select(code) |>
  left_join(effort, by = "code", relationship = "one-to-many") |>
  filter(!is.na(profil))

g_carte <- ggplot(carte) +
  geom_sf(data = communes, fill = "grey85", colour = "white", linewidth = 0.05) +
  geom_sf(aes(fill = taux_effort), colour = "white", linewidth = 0.05) +
  facet_wrap(~ profil) +
  scale_fill_viridis_c(name = "Taux d'effort\nlogement + transport", option = "magma",
                       direction = -1, labels = scales::label_percent(), na.value = "grey85") +
  labs(title = "Part du revenu consacrée au logement et aux trajets domicile-travail",
       subtitle = "Revenu = niveau de vie médian de la commune × unités de consommation du profil ; gris = données manquantes",
       caption = "Sources : INSEE (Filosofi 2023, RP 2022), DVF, geo.api.gouv.fr ; calculs de l'auteur") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
        strip.text = element_text(face = "bold", size = 11))

ggsave("output/carte_taux_effort_profils_21.png", g_carte, width = 12, height = 7,
       dpi = 200, bg = "white")
print(g_carte)

# --- 4. Résumé chiffré ----------------------------------------------------------------------------
cat("\nTaux d'effort médian et part des communes au-dessus du seuil de",
    100 * hyp$seuil_effort, "% :\n")
effort |>
  filter(!is.na(taux_effort)) |>
  group_by(profil) |>
  summarise(communes            = n(),
            effort_median_pct   = round(100 * median(taux_effort)),
            pct_communes_seuil  = round(100 * mean(au_dessus_seuil))) |>
  print()

cat("\nTaux d'effort médian par tranche de distance à Dijon (%) :\n")
effort |>
  filter(!is.na(taux_effort)) |>
  mutate(tranche = cut(dist_dijon_km, c(0, 10, 20, 30, 40, 60, 80, 200),
                       labels = c("<10", "10-20", "20-30", "30-40", "40-60", "60-80", ">80"))) |>
  group_by(profil, tranche) |>
  summarise(effort = round(100 * median(taux_effort)), .groups = "drop") |>
  tidyr::pivot_wider(names_from = profil, values_from = effort) |>
  print()
