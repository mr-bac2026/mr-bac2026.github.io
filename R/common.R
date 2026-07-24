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

BAC_ROOT <- tryCatch(rprojroot::find_root(rprojroot::has_file("_quarto.yml")),
                     error = function(e) getwd())
BAC_CSV  <- file.path(BAC_ROOT, "data", "bac2026.csv")
BAC_GEO  <- file.path(BAC_ROOT, "mrt_adm1.geojson")

BAC_FONT_TITLE <- "serif"
BAC_FONT_BODY  <- "serif"
BAC_FONT_AR    <- "sans"
suppressWarnings(suppressMessages(tryCatch({
  f <- function(...) file.path(BAC_ROOT, "fonts", ...)
  systemfonts::register_font(
    name   = "bacserif",
    plain  = f("SourceSerif4-Regular.ttf"),
    bold   = f("SourceSerif4-Semibold.ttf"),
    italic = f("SourceSerif4-It.ttf")
  )
  systemfonts::register_font(
    name  = "bacarabe",
    plain = f("Amiri-Regular.ttf"),
    bold  = f("Amiri-Bold.ttf")
  )
  BAC_FONT_TITLE <<- "bacserif"
  BAC_FONT_BODY  <<- "bacserif"
  BAC_FONT_AR    <<- "bacarabe"
}, error = function(e) invisible(NULL))))

bac_font <- function(lang = "fr") {
  if (identical(lang, "ar")) BAC_FONT_AR else BAC_FONT_BODY
}
bac_font_title <- function(lang = "fr") {
  if (identical(lang, "ar")) BAC_FONT_AR else BAC_FONT_TITLE
}

bac_rtl <- function(lang = "fr") identical(lang, "ar")

bac_pos_y <- function(lang = "fr") if (bac_rtl(lang)) "right" else "left"

bac_h <- function(lang, h) if (bac_rtl(lang)) 1 - h else h

bac_x_num <- function(lang, ...) {
  a <- list(...)
  if (!bac_rtl(lang)) return(do.call(scale_x_continuous, a))
  if (!is.null(a$expand) && length(a$expand) == 4) {
    a$expand <- a$expand[c(3, 4, 1, 2)]
  }
  do.call(scale_x_reverse, a)
}

bac_x_log <- function(lang, ...) {
  a <- list(...)
  if (!bac_rtl(lang)) return(do.call(scale_x_log10, a))
  if (!is.null(a$expand) && length(a$expand) == 4) {
    a$expand <- a$expand[c(3, 4, 1, 2)]
  }
  a$transform <- scales::transform_compose("log10", "reverse")
  do.call(scale_x_continuous, a)
}

bac_x_disc <- function(lang, ...) {
  a <- list(...)
  if (bac_rtl(lang)) a$limits <- rev
  do.call(scale_x_discrete, a)
}

bac_y_num <- function(lang, ...) {
  scale_y_continuous(..., position = bac_pos_y(lang))
}

bac_y_disc <- function(lang, ...) {
  scale_y_discrete(..., position = bac_pos_y(lang))
}

BAC_SOUS_TITRE_PT <- 11.4
BAC_LARGEUR_UTILE <- 7.5

bac_replier <- function(txt, lang = "fr", largeur = BAC_LARGEUR_UTILE,
                        taille = BAC_SOUS_TITRE_PT) {
  if (!nzchar(txt)) return(txt)
  cap <- ragg::agg_capture(width = 200, height = 200, units = "px",
                           res = 72, background = NA)
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- graphics::par(family = bac_font(lang), ps = taille)
  on.exit(graphics::par(op), add = TRUE, after = FALSE)
  mots <- strsplit(txt, " ", fixed = TRUE)[[1]]
  lignes <- character(0)
  cur <- ""
  for (m in mots) {
    essai <- if (nzchar(cur)) paste(cur, m) else m
    trop <- graphics::strwidth(essai, units = "inches") > largeur
    if (trop && nzchar(cur)) {
      lignes <- c(lignes, cur)
      cur <- m
    } else {
      cur <- essai
    }
  }
  paste(c(lignes, cur), collapse = "\n")
}

bac_sous_titre <- function(cle, lang = "fr") {
  bac_replier(tr(cle, lang), lang)
}

BAC_ETIQ_RES <- 300

bac_etiquette_pivotee <- function(label, lang, size = 3,
                                  colour = "#585d66") {
  res <- BAC_ETIQ_RES
  cap <- ragg::agg_capture(width = 2400, height = 400, units = "px",
                           res = res, background = NA)
  op <- graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  graphics::text(0.005, 0.5, label, adj = c(0, 0.5),
                 family = bac_font(lang),
                 cex = size * .pt / graphics::par("ps"),
                 col = colour, xpd = NA)
  m <- cap(native = FALSE)
  graphics::par(op)
  grDevices::dev.off()
  plein <- m != "transparent"
  kr <- range(which(apply(plein, 1, any)))
  kc <- range(which(apply(plein, 2, any)))
  m <- m[kr[1]:kr[2], kc[1]:kc[2], drop = FALSE]
  tm <- t(m)
  tm[rev(seq_len(nrow(tm))), , drop = FALSE]
}

bac_seuil <- function(label, x, lang, size = 3, vjust = 1.6, dy = 3) {
  if (!identical(lang, "ar")) {
    return(annotate("text", x = x, y = Inf, label = label,
                    family = bac_font(lang), hjust = 1.03, vjust = vjust,
                    size = size, colour = BAC_COL$encre_douce, angle = 90))
  }
  m <- bac_etiquette_pivotee(label, lang, size, BAC_COL$encre_douce)
  gr <- grid::rasterGrob(
    m,
    x = grid::unit(0, "npc") + grid::unit(2, "pt"),
    y = grid::unit(1, "npc") - grid::unit(dy, "pt"),
    hjust = 0, vjust = 1,
    width = grid::unit(ncol(m) / BAC_ETIQ_RES, "in"),
    height = grid::unit(nrow(m) / BAC_ETIQ_RES, "in"),
    interpolate = TRUE
  )
  annotation_custom(gr, xmin = x, xmax = x, ymin = -Inf, ymax = Inf)
}

BAC_COL <- list(
  encre       = "#0a0a0a",
  encre_douce = "#585d66",
  papier      = "#ffffff",
  papier_alt  = "#f3f4f6",
  trait       = "#e4e6e9",
  terre       = "#1552d0",
  or          = "#e0952a",
  vert        = "#127a5b"
)

BAC_PAL_CAT <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                 "#e87ba4", "#4a3aa7", "#e34948", "#008300")

BAC_PAL_DECISION <- c(
  "Admis"                    = "#1552d0",
  "Sessionnaire"             = "#e0952a",
  "Ajourné"                  = "#aab0b8",
  "Absent"                   = "#6b7078",
  "Examen annulé"            = "#33363b"
)

BAC_SEQ_CHAUD <- c("#e7eefb", "#c2d5f5", "#8fb0ea", "#5585dc",
                   "#2a5fcf", "#1544a3", "#0a2c70")
BAC_SEQ_FROID <- c("#f2f2f0", "#d6d6d3", "#adadaa",
                   "#7d7d7a", "#4e4e4c", "#242423")

theme_bac <- function(base_size = 12, lang = "fr") {
  rtl  <- identical(lang, "ar")
  bord <- if (rtl) 1 else 0
  cote <- if (rtl) "right" else "left"
  titre_y <- if (rtl) {
    element_text(colour = BAC_COL$encre_douce, size = rel(0.9), angle = 0,
                 hjust = 1, vjust = 1, margin = margin(l = 6))
  } else {
    element_text(colour = BAC_COL$encre_douce, size = rel(0.9))
  }
  theme_minimal(base_size = base_size, base_family = bac_font(lang)) +
    theme(
      plot.background = element_rect(fill = BAC_COL$papier, colour = NA),
      panel.background = element_rect(fill = BAC_COL$papier, colour = NA),
      panel.grid.major = element_line(
        colour = BAC_COL$trait, linewidth = 0.3
      ),
      panel.grid.minor = element_blank(),
      axis.text = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.85)
      ),
      axis.title = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.9)
      ),
      axis.title.y = titre_y,
      axis.title.y.right = titre_y,
      plot.title = element_text(
        family = bac_font_title(lang), colour = BAC_COL$encre,
        size = rel(1.4), face = "bold", lineheight = 1.08,
        hjust = bord, margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.95), lineheight = 1.2,
        hjust = bord, margin = margin(b = 12)
      ),
      plot.caption = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.72),
        hjust = bord, margin = margin(t = 14)
      ),
      plot.caption.position = "plot",
      plot.title.position = "plot",
      legend.title = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.82)
      ),
      legend.text = element_text(
        colour = BAC_COL$encre_douce, size = rel(0.82)
      ),
      legend.position = "top",
      legend.justification = cote,
      plot.margin = margin(16, 18, 12, 16),
      strip.text = element_text(
        colour = BAC_COL$encre, face = "bold", size = rel(0.9)
      )
    )
}

theme_bac_carte <- function(lang = "fr") {
  theme_bac(lang = lang) + theme(
    axis.text = element_blank(), axis.title = element_blank(),
    axis.title.y = element_blank(), axis.title.y.right = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right", legend.key.height = unit(24, "pt"),
    legend.key.width = unit(10, "pt")
  )
}

if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(dev.args = list(bg = BAC_COL$papier))
  if (requireNamespace("ragg", quietly = TRUE)) {
    knitr::opts_chunk$set(dev = "ragg_png")
  }
}

SRC <- paste("Source : Direction des examens, résultats du baccalauréat 2026",
             "(session normale). Calculs de l'auteur.")
SRC_NOTE <- "*Source : Direction des examens. Calculs de l'auteur.*"

lire_bac <- function() {
  raw <- readr::read_csv(BAC_CSV, show_col_types = FALSE, progress = FALSE)

  norm_ws <- function(x) x |> str_replace_all("\\s+", " ") |> str_trim()

  raw |>
    mutate(
      moyenne   = suppressWarnings(as.numeric(Moy_Bac)),
      wilaya    = norm_ws(Wilaya_FR),
      wilaya_ar = norm_ws(Wilaya_AR),
      serie     = norm_ws(Serie_FR),
      centre    = norm_ws(`Centre Examen  FR`),
      etab      = norm_ws(Etablissement_FR),
      etab_ar   = norm_ws(Etablissement_AR),
      naissance = suppressWarnings(as.Date(substr(`Date Naiss`, 1, 10))),
      annee_n   = as.integer(substr(`Date Naiss`, 1, 4)),
      age       = 2026L - annee_n,
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
      present   = as.integer(decision != "Absent" &
                               decision != "Examen annulé"),
      pole      = if_else(str_detect(wilaya, "Nouakchott"),
                          "Nouakchott", "Intérieur"),
      mention   = case_when(
        decision != "Admis" ~ NA_character_,
        moyenne >= 16       ~ "Très bien",
        moyenne >= 14       ~ "Bien",
        moyenne >= 12       ~ "Assez bien",
        TRUE                ~ "Passable"
      ),
      mention   = factor(mention, levels = c("Passable", "Assez bien",
                                             "Bien", "Très bien"))
    )
}

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

ic_wilson <- function(succes, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- succes / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  demi   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  tibble::tibble(p = p, bas = pmax(0, centre - demi),
                 haut = pmin(1, centre + demi))
}

synth_taux <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(n = dplyr::n(), admis = sum(admis),
              moyenne = mean(moyenne, na.rm = TRUE), .groups = "drop") |>
    mutate(ic_wilson(admis, n)) |>
    rename(taux = p)
}

pct_fr <- function(x, acc = 0.1) {
  scales::percent(x, accuracy = acc, decimal.mark = ",", suffix = " %")
}
num_fr <- function(x, acc = 0.01) {
  scales::number(x, accuracy = acc, decimal.mark = ",", big.mark = " ")
}

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
SRC_POP <- paste("Population : RGPH 2023 (ANSADE), 5e recensement général",
                 "de la population et de l'habitat.")

BAC_NOM <- c(
  "Hod Gharby"   = "Hodh el Gharbi",
  "Hod Charghy"  = "Hodh ech Chargui",
  "Dakhlet NDB"  = "Dakhlet Nouadhibou",
  "Guidimagha"   = "Guidimaka",
  "Tiris Zemour" = "Tiris Zemmour"
)
joli_nom <- function(x) {
  r <- unname(BAC_NOM[x])
  ifelse(is.na(r), x, r)
}

BAC_NOM_AR <- c(
  "Adrar"            = "ادرار",
  "Assaba"           = "لعصابه",
  "Brakna"           = "لبراكنه",
  "Dakhlet NDB"      = "داخلت انواذيب",
  "Gorgol"           = "كوركل",
  "Guidimagha"       = "كيدي ماغه",
  "Hod Charghy"      = "الحوض الشرقي",
  "Hod Gharby"       = "الحوض الغربي",
  "Inchiri"          = "اينشيري",
  "Nouakchott Nord"  = "انواكشوط الشمالية",
  "Nouakchott Ouest" = "انواكشوط الغربية",
  "Nouakchott Sud"   = "انواكشوط الجنوبية",
  "Tagant"           = "تكانت",
  "Tiris Zemour"     = "تيرس ازمور",
  "Trarza"           = "اترارزه"
)

nom_wilaya <- function(x, lang = "fr") {
  if (!bac_rtl(lang)) return(joli_nom(x))
  r <- unname(BAC_NOM_AR[x])
  ifelse(is.na(r), joli_nom(x), r)
}

lire_carte <- function() {
  gj <- jsonlite::fromJSON(BAC_GEO, simplifyVector = FALSE)
  out <- list()
  gid <- 0L
  for (feat in gj$features) {
    nom  <- feat$properties$shapeName
    geom <- feat$geometry
    polys <- if (geom$type == "Polygon") {
      list(geom$coordinates)
    } else {
      geom$coordinates
    }
    for (poly in polys) {
      ring <- poly[[1]]
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
