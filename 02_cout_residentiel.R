# =============================================================================
# 02_cout_residentiel.R
# Coût résidentiel annuel = logement + trajets domicile-travail vers Dijon
# Profil : ménage qui achète un logement de 70 m² et travaille à Dijon
# Pré-requis : avoir lancé 01_prix_dvf.R (fichier data/processed/prix_m2_communes_21.csv)
# =============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(sf)

# --- 0. Hypothèses (toutes modifiables ici) -------------------------------------
# Ce sont des valeurs de départ, à documenter et affiner dans la note.
hyp <- list(
  surface_m2      = 70,     # surface du logement acheté
  taux_credit     = 0.035,  # taux d'intérêt annuel du prêt immobilier
  duree_credit    = 20,     # durée du prêt en années
  jours_travailles = 220,   # jours de trajet par an
  cout_km         = 0.30,   # coût d'usage d'une voiture, en € par km
  facteur_route   = 1.3,    # passage de la distance à vol d'oiseau à la distance routière
  dist_min_km     = 3,      # distance minimale (trajets à l'intérieur de Dijon)
  code_dijon      = "21231"
)

# --- 1. Données -------------------------------------------------------------------
prix <- read_csv("data/processed/prix_m2_communes_21.csv",
                 col_types = cols(code_commune = "c", nom_commune = "c"))

communes <- st_read(
  "https://geo.api.gouv.fr/departements/21/communes?format=geojson&geometry=contour",
  quiet = TRUE
) |>
  st_transform(2154)   # Lambert 93 : distances en mètres

# --- 2. Distance de chaque commune à Dijon -----------------------------------------
centres <- st_point_on_surface(st_geometry(communes))
centre_dijon <- centres[communes$code == hyp$code_dijon]

communes$dist_km <- as.numeric(st_distance(centres, centre_dijon)) / 1000 * hyp$facteur_route
communes$dist_km <- pmax(communes$dist_km, hyp$dist_min_km)

# --- 3. Coûts annuels --------------------------------------------------------------
# Annuité constante du prêt : part du prix remboursée chaque année
r <- hyp$taux_credit
facteur_annuite <- r / (1 - (1 + r)^(-hyp$duree_credit))

cout <- communes |>
  left_join(prix, by = c("code" = "code_commune")) |>
  mutate(
    cout_logement  = prix_m2_med * hyp$surface_m2 * facteur_annuite,
    cout_transport = 2 * dist_km * hyp$jours_travailles * hyp$cout_km,
    cout_total     = cout_logement + cout_transport,
    part_transport = cout_transport / cout_total
  )

write_csv(st_drop_geometry(cout) |>
            select(code, nom, dist_km, prix_m2_med, n_ventes,
                   cout_logement, cout_transport, cout_total, part_transport),
          "data/processed/cout_residentiel_21.csv")

# --- 4. Cartes -----------------------------------------------------------------------
carte_euros <- function(var, titre, fichier) {
  g <- ggplot(cout) +
    geom_sf(aes(fill = .data[[var]]), colour = "white", linewidth = 0.05) +
    scale_fill_viridis_c(name = "€ / an", na.value = "grey85", option = "magma",
                         direction = -1, labels = scales::label_number(big.mark = " ")) +
    labs(title = titre,
         subtitle = sprintf("Logement de %d m², prêt %d ans à %.1f %% ; trajets vers Dijon, %d jours/an, %.2f €/km",
                            hyp$surface_m2, hyp$duree_credit, 100 * hyp$taux_credit,
                            hyp$jours_travailles, hyp$cout_km),
         caption = "Sources : DVF (DGFiP), geo.api.gouv.fr ; calculs de l'auteur") +
    theme_void() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8))
  ggsave(fichier, g, width = 8, height = 8, dpi = 200, bg = "white")
  g
}

g_transport <- carte_euros("cout_transport", "Coût annuel des trajets domicile-travail vers Dijon",
                           "output/carte_cout_transport_21.png")
g_total     <- carte_euros("cout_total", "Coût résidentiel annuel : logement + transport",
                           "output/carte_cout_total_21.png")

# --- 5. Le graphique clé : logement moins cher, transport plus cher -----------------
donnees <- st_drop_geometry(cout) |> filter(!is.na(cout_total))

g_arbitrage <- ggplot(donnees, aes(x = dist_km)) +
  geom_point(aes(y = cout_logement,  colour = "Logement"),  alpha = 0.4, size = 1) +
  geom_point(aes(y = cout_total,     colour = "Logement + transport"), alpha = 0.4, size = 1) +
  geom_smooth(aes(y = cout_logement, colour = "Logement"), se = FALSE, method = "loess") +
  geom_smooth(aes(y = cout_total,    colour = "Logement + transport"), se = FALSE, method = "loess") +
  scale_colour_manual(values = c("Logement" = "#1b9e77", "Logement + transport" = "#d95f02"),
                      name = NULL) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(title = "S'éloigner de Dijon : le logement baisse, le coût total beaucoup moins",
       x = "Distance routière estimée à Dijon (km)", y = "Coût annuel (€)",
       caption = "Une commune = un point (communes avec au moins 5 ventes DVF)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

ggsave("output/arbitrage_distance_21.png", g_arbitrage, width = 8, height = 5.5,
       dpi = 200, bg = "white")
print(g_arbitrage)

# --- 6. Résumé chiffré ------------------------------------------------------------------
dijon <- donnees |> filter(code == hyp$code_dijon)
cat("\nDijon : logement", round(dijon$cout_logement), "€/an, transport",
    round(dijon$cout_transport), "€/an, total", round(dijon$cout_total), "€/an\n")

cat("\nCoût total moyen par tranche de distance :\n")
donnees |>
  mutate(tranche = cut(dist_km, c(0, 10, 20, 30, 40, 60, 80, 200),
                       labels = c("<10", "10-20", "20-30", "30-40", "40-60", "60-80", ">80"))) |>
  group_by(tranche) |>
  summarise(communes = n(),
            logement = round(mean(cout_logement)),
            transport = round(mean(cout_transport)),
            total = round(mean(cout_total)),
            part_transport_pct = round(100 * mean(part_transport))) |>
  print()
