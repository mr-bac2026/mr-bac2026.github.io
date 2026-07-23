# =============================================================================
#  Socle commun : données, palette, thème graphique, fonds de carte.
#  Sourcé par chaque page du site. Aucune dépendance système (pas de GDAL).
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(scales)
  library(jsonlite)
  library(ggtext)
  library(glue)
})

# --- Chemins ----------------------------------------------------------------
BAC_ROOT <- tryCatch(rprojroot::find_root(rprojroot::has_file("_quarto.yml")),
                     error = function(e) getwd())
BAC_CSV  <- file.path(BAC_ROOT, "data", "bac2026.csv")
BAC_GEO  <- file.path(BAC_ROOT, "mrt_adm1.geojson")

# --- Typographie ------------------------------------------------------------
# Serif de lecture (Source Serif 4), rendu « longform éditorial ».
# Chargement défensif : si la police distante échoue, on retombe sur "serif".
BAC_FONT_TITLE <- "serif"
BAC_FONT_BODY  <- "serif"
suppressWarnings(suppressMessages(tryCatch({
  library(showtext)
  library(sysfonts)
  sysfonts::font_add_google("Source Serif 4", "sourceserif")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
  BAC_FONT_TITLE <<- "sourceserif"
  BAC_FONT_BODY  <<- "sourceserif"
}, error = function(e) invisible(NULL))))

# =============================================================================
#  PALETTE  — « données modernes » : blanc, encre, un accent cobalt.
#  (catégorielle validée CVD-safe ; séquentielles perceptuelles)
# =============================================================================
BAC_COL <- list(
  encre       = "#0a0a0a",  # texte principal
  encre_douce = "#585d66",  # texte secondaire
  papier      = "#ffffff",  # fond blanc
  papier_alt  = "#f3f4f6",  # aplat clair
  trait       = "#e4e6e9",  # grilles
  terre       = "#1552d0",  # cobalt : accent principal des marques
  or          = "#e0952a",  # ambre : accent secondaire
  vert        = "#127a5b"   # vert profond : accent tertiaire
)

# Catégoriel (séries) — ordre validé (dataviz validator, worst adjacent ΔE 9.1)
BAC_PAL_CAT <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                 "#e87ba4", "#4a3aa7", "#e34948", "#008300")

# Décisions — deux couleurs signifiantes (admis, sessionnaire) + un dégradé de
# gris pour les issues neutres/négatives.
BAC_PAL_DECISION <- c(
  "Admis"                    = "#1552d0",
  "Sessionnaire"             = "#e0952a",
  "Ajourné"                  = "#aab0b8",
  "Absent"                   = "#6b7078",
  "Examen annulé"            = "#33363b"
)

# Séquentielle cobalt (clair -> foncé) pour les cartes et barres de taux.
BAC_SEQ_CHAUD <- c("#e7eefb", "#c2d5f5", "#8fb0ea", "#5585dc", "#2a5fcf", "#1544a3", "#0a2c70")
# Séquentielle graphite (clair -> foncé) pour les effectifs.
BAC_SEQ_FROID <- c("#f2f2f0", "#d6d6d3", "#adadaa", "#7d7d7a", "#4e4e4c", "#242423")

# =============================================================================
#  THÈME
# =============================================================================
theme_bac <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = BAC_FONT_BODY) +
    theme(
      plot.background   = element_rect(fill = BAC_COL$papier, colour = NA),
      panel.background  = element_rect(fill = BAC_COL$papier, colour = NA),
      panel.grid.major  = element_line(colour = BAC_COL$trait, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = BAC_COL$encre_douce, size = rel(0.85)),
      axis.title        = element_text(colour = BAC_COL$encre_douce, size = rel(0.9)),
      plot.title        = element_text(
        family = BAC_FONT_TITLE, colour = BAC_COL$encre, size = rel(1.4),
        face = "bold", lineheight = 1.08, hjust = 0, margin = margin(b = 5)),
      plot.subtitle     = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.95), lineheight = 1.2,
        hjust = 0, margin = margin(b = 12)),
      plot.caption      = element_text(colour = BAC_COL$encre_douce, size = rel(0.72),
                                       hjust = 0, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      legend.title      = element_text(colour = BAC_COL$encre_douce, size = rel(0.82)),
      legend.text       = element_text(colour = BAC_COL$encre_douce, size = rel(0.82)),
      legend.position   = "top",
      legend.justification = "left",
      plot.margin       = margin(16, 18, 12, 16),
      strip.text        = element_text(colour = BAC_COL$encre, face = "bold", size = rel(0.9))
    )
}

# Thème carte (dépouillé)
theme_bac_carte <- function() {
  theme_bac() + theme(
    axis.text = element_blank(), axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right", legend.key.height = unit(24, "pt"),
    legend.key.width = unit(10, "pt")
  )
}

# Fond des figures : on force le canevas des PNG en sable, sinon les cartes
# (coord_quickmap) laissent apparaître du blanc sur les côtés.
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(dev.args = list(bg = BAC_COL$papier))
}

SRC <- "Source : Direction des examens, résultats du baccalauréat 2026 (session normale). Calculs de l'auteur."
# Variante courte (markdown) pour les notes de bas de tableau gt.
SRC_NOTE <- "*Source : Direction des examens. Calculs de l'auteur.*"

# =============================================================================
#  DONNÉES
# =============================================================================
lire_bac <- function() {
  raw <- readr::read_csv(BAC_CSV, show_col_types = FALSE, progress = FALSE)

  norm_ws <- function(x) x |> str_replace_all("\\s+", " ") |> str_trim()

  raw |>
    mutate(
      moyenne   = suppressWarnings(as.numeric(Moy_Bac)),
      wilaya    = norm_ws(Wilaya_FR),
      serie     = norm_ws(Serie_FR),
      centre    = norm_ws(`Centre Examen  FR`),
      etab      = norm_ws(Etablissement_FR),
      naissance = suppressWarnings(as.Date(substr(`Date Naiss`, 1, 10))),
      annee_n   = as.integer(substr(`Date Naiss`, 1, 4)),
      age       = 2026L - annee_n,
      # âge plausible pour un candidat au bac (sinon NA, ligne conservée)
      age_val   = if_else(age >= 15 & age <= 30, age, NA_integer_),
      decision  = case_when(
        Decision == "Admis"                     ~ "Admis",
        Decision == "Sessionnaire"              ~ "Sessionnaire",
        Decision %in% c("Ajourné")              ~ "Ajourné",
        Decision == "Absent"                    ~ "Absent",
        TRUE                                     ~ "Examen annulé"
      ),
      decision  = factor(decision, levels = names(BAC_PAL_DECISION)),
      admis     = as.integer(decision == "Admis"),
      present   = as.integer(decision != "Absent" & decision != "Examen annulé"),
      # regroupement géographique
      pole      = if_else(str_detect(wilaya, "Nouakchott"), "Nouakchott", "Intérieur"),
      # mention (pour les admis)
      mention   = case_when(
        decision != "Admis" ~ NA_character_,
        moyenne >= 16       ~ "Très bien",
        moyenne >= 14       ~ "Bien",
        moyenne >= 12       ~ "Assez bien",
        TRUE                ~ "Passable"
      ),
      mention   = factor(mention, levels = c("Passable","Assez bien","Bien","Très bien"))
    )
}

# Correspondance wilaya (données) -> shapeName (GeoJSON geoBoundaries)
BAC_CROSSWALK <- c(
  "Nouakchott Sud"    = "Nouakchott",
  "Nouakchott Nord"   = "Nouakchott",
  "Nouakchott Ouest"  = "Nouakchott",
  "Trarza"            = "Trarza",
  "Hod Gharby"        = "Hodh el Gharbi",
  "Assaba"            = "Assaba",
  "Gorgol"            = "Gorgol",
  "Dakhlet NDB"       = "Dakhlet Nouadhibou",
  "Brakna"            = "Brakna",
  "Hod Charghy"       = "Hodh ech Chargui",
  "Tagant"            = "Tagant",
  "Guidimagha"        = "Guidimaka",
  "Adrar"             = "Adrar",
  "Tiris Zemour"      = "Tiris Zemmour",
  "Inchiri"           = "Inchiri"
)

# =============================================================================
#  OUTILS D'ANALYSE
# =============================================================================

# Intervalle de confiance de Wilson pour une proportion (plus honnête que
# l'approximation normale quand l'effectif est faible).
ic_wilson <- function(succes, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- succes / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  demi   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  tibble::tibble(p = p, bas = pmax(0, centre - demi), haut = pmin(1, centre + demi))
}

# Tableau de synthèse par groupe : effectif, admis, taux + IC de Wilson.
synth_taux <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(n = dplyr::n(), admis = sum(admis),
              moyenne = mean(moyenne, na.rm = TRUE), .groups = "drop") |>
    mutate(ic_wilson(admis, n)) |>
    rename(taux = p)
}

# Format pourcentage à la française.
pct_fr <- function(x, acc = 0.1) scales::percent(x, accuracy = acc, decimal.mark = ",", suffix = " %")
num_fr <- function(x, acc = 0.01) scales::number(x, accuracy = acc, decimal.mark = ",", big.mark = " ")

# Population par région — RGPH 2023 (5e recensement, ANSADE).
# Clés = shapeName du fond de carte geoBoundaries. Total = 4 927 532 habitants.
BAC_POP_2023 <- c(
  "Adrar"              =   71623,
  "Assaba"             =  451804,
  "Brakna"             =  391310,
  "Dakhlet Nouadhibou" =  184459,
  "Gorgol"             =  442490,
  "Guidimaka"          =  363075,
  "Hodh ech Chargui"   =  625643,
  "Hodh el Gharbi"     =  403089,
  "Inchiri"            =   29484,
  "Nouakchott"         = 1446761,
  "Tagant"             =  114760,
  "Tiris Zemmour"      =   79129,
  "Trarza"             =  323903
)
SRC_POP <- "Population : RGPH 2023 (ANSADE), 5e recensement général de la population et de l'habitat."

# Noms d'affichage harmonisés : on aligne les abréviations du fichier de résultats
# sur l'orthographe du recensement / du fond de carte, pour une graphie unique.
BAC_NOM <- c(
  "Hod Gharby"   = "Hodh el Gharbi",
  "Hod Charghy"  = "Hodh ech Chargui",
  "Dakhlet NDB"  = "Dakhlet Nouadhibou",
  "Guidimagha"   = "Guidimaka",
  "Tiris Zemour" = "Tiris Zemmour"
)
joli_nom <- function(x) { r <- unname(BAC_NOM[x]); ifelse(is.na(r), x, r) }

# GeoJSON -> data.frame (long,lat,group) sans sf. Gère Polygon & MultiPolygon.
lire_carte <- function() {
  gj <- jsonlite::fromJSON(BAC_GEO, simplifyVector = FALSE)
  out <- list(); gid <- 0L
  for (feat in gj$features) {
    nom  <- feat$properties$shapeName
    geom <- feat$geometry
    polys <- if (geom$type == "Polygon") list(geom$coordinates) else geom$coordinates
    for (poly in polys) {
      ring <- poly[[1]]                       # anneau extérieur
      gid <- gid + 1L
      m <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
      out[[length(out) + 1L]] <- data.frame(
        shapeName = nom, group = gid,
        long = m[, 1], lat = m[, 2]
      )
    }
  }
  bind_rows(out)
}
