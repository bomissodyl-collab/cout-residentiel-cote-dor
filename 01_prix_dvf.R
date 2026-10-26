# =============================================================================
# 01_prix_dvf.R
# Prix médian au m² des logements par commune, Côte-d'Or (21)
# Source : DVF géolocalisées (data.gouv.fr), contours : geo.api.gouv.fr
# =============================================================================

# --- 0. Packages (installés automatiquement la première fois) ----------------
paquets <- c("dplyr", "readr", "ggplot2", "sf")
a_installer <- paquets[!paquets %in% rownames(installed.packages())]
if (length(a_installer) > 0) install.packages(a_installer)

library(dplyr)
library(readr)
library(ggplot2)
library(sf)

# --- 1. Dossiers du projet -----------------------------------------------------
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# Les données brutes ne vont pas sur GitHub (trop lourdes, re-téléchargeables)
gi <- if (file.exists(".gitignore")) readLines(".gitignore") else character(0)
if (!"data/raw/" %in% gi) cat("\ndata/raw/\n", file = ".gitignore", append = TRUE)

# --- 2. Téléchargement de DVF pour la Côte-d'Or --------------------------------
annees <- 2021:2025
for (a in annees) {
  url  <- sprintf("https://files.data.gouv.fr/geo-dvf/latest/csv/%d/departements/21.csv.gz", a)
  dest <- sprintf("data/raw/dvf_21_%d.csv.gz", a)
  if (!file.exists(dest)) {
    ok <- tryCatch({ download.file(url, dest, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) { unlink(dest); message("Année ", a, " non disponible, ignorée.") }
    else message("Année ", a, " téléchargée.")
  }
}

fichiers <- list.files("data/raw", pattern = "^dvf_21_.*\\.csv\\.gz$", full.names = TRUE)
dvf <- fichiers |>
  lapply(read_csv, col_types = cols(.default = "c"), progress = FALSE) |>
  bind_rows()
message(nrow(dvf), " lignes DVF chargées.")

# --- 3. Nettoyage ---------------------------------------------------------------
# On garde les ventes d'une seule maison ou d'un seul appartement,
# pour que le prix corresponde bien à un logement.
logements <- dvf |>
  mutate(valeur_fonciere     = as.numeric(valeur_fonciere),
         surface_reelle_bati = as.numeric(surface_reelle_bati)) |>
  filter(nature_mutation == "Vente",
         type_local %in% c("Maison", "Appartement"))

mutations_simples <- logements |>
  count(id_mutation) |>
  filter(n == 1) |>
  pull(id_mutation)

logements <- logements |>
  filter(id_mutation %in% mutations_simples,
         !is.na(valeur_fonciere), !is.na(surface_reelle_bati),
         surface_reelle_bati >= 10) |>
  mutate(prix_m2 = valeur_fonciere / surface_reelle_bati) |>
  filter(prix_m2 >= 200, prix_m2 <= 10000)   # retire les valeurs aberrantes

message(nrow(logements), " ventes de logements retenues.")

# --- 4. Prix médian au m² par commune ------------------------------------------
prix_commune <- logements |>
  group_by(code_commune, nom_commune) |>
  summarise(n_ventes   = n(),
            prix_m2_med = median(prix_m2),
            .groups = "drop") |>
  mutate(prix_m2_med = if_else(n_ventes >= 5, prix_m2_med, NA_real_))  # trop peu de ventes = NA

write_csv(prix_commune, "data/processed/prix_m2_communes_21.csv")

# --- 5. Contours des communes et carte -----------------------------------------
communes <- st_read(
  "https://geo.api.gouv.fr/departements/21/communes?format=geojson&geometry=contour",
  quiet = TRUE
)

carte <- communes |>
  left_join(prix_commune, by = c("code" = "code_commune"))

g <- ggplot(carte) +
  geom_sf(aes(fill = prix_m2_med), colour = "white", linewidth = 0.05) +
  scale_fill_viridis_c(name = "€/m²", na.value = "grey85", option = "magma",
                       direction = -1, labels = scales::label_number(big.mark = " ")) +
  labs(title = "Prix médian au m² des logements, Côte-d'Or",
       subtitle = paste0("Ventes DVF ", min(substr(logements$date_mutation, 1, 4)), "–",
                         max(substr(logements$date_mutation, 1, 4)),
                         " ; communes avec au moins 5 ventes (gris = données insuffisantes)"),
       caption = "Sources : DVF (DGFiP, data.gouv.fr), contours geo.api.gouv.fr") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/carte_prix_m2_21.png", g, width = 8, height = 8, dpi = 200, bg = "white")
print(g)

# --- 6. Premier coup d'œil --------------------------------------------------------
cat("\nCommunes couvertes (>= 5 ventes) :",
    sum(!is.na(prix_commune$prix_m2_med)), "sur", nrow(communes), "\n")
cat("\nLes 10 communes les plus chères :\n")
prix_commune |> filter(!is.na(prix_m2_med)) |>
  arrange(desc(prix_m2_med)) |> head(10) |> print()
cat("\nLes 10 communes les moins chères :\n")
prix_commune |> filter(!is.na(prix_m2_med)) |>
  arrange(prix_m2_med) |> head(10) |> print()
