# =============================================================================
#  INTERNATIONALISATION
#  Charge translations.csv (colonnes : key, fr, en, ar) et expose tr().
#  Éditez translations.csv (dans un tableur) pour ajouter/corriger un texte.
# =============================================================================
BAC_I18N <- local({
  path <- file.path(BAC_ROOT, "l10n", "l10n.csv")
  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  split(df, df$key)
})

# tr(key, lang) : texte traduit ; repli sur le français si vide/manquant.
# Les "\n" littéraux du CSV deviennent de vrais sauts de ligne (légendes).
tr <- function(key, lang = "fr") {
  row <- BAC_I18N[[key]]
  if (is.null(row)) stop("Clé i18n manquante : ", key)
  val <- row[[lang]]
  if (length(val) == 0 || is.na(val) || !nzchar(val)) val <- row[["fr"]]
  gsub("\\\\n", "\n", val)
}

# Caption « source » complète selon le contexte de la figure.
cap_src      <- function(lang = "fr") tr("src", lang)
cap_src_pop  <- function(lang = "fr") tr("src_pop", lang)
cap_src_geo  <- function(lang = "fr") {
  paste(tr("src", lang), tr("geo_fond", lang), sep = "\n")
}
cap_src_both <- function(lang = "fr") {
  paste(tr("src", lang), tr("src_pop", lang), sep = "\n")
}
