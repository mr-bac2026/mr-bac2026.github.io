BAC_I18N <- jsonlite::fromJSON(
  file.path(BAC_ROOT, "l10n", "l10n.json"),
  simplifyVector = FALSE
)

tr <- function(key, lang = "fr") {
  row <- BAC_I18N[[key]]
  if (is.null(row)) stop("Clé i18n manquante : ", key)
  val <- row[[lang]]
  if (is.null(val) || is.na(val) || !nzchar(val)) val <- row[["fr"]]
  val
}

cap_src      <- function(lang = "fr") tr("src", lang)
cap_src_pop  <- function(lang = "fr") tr("src_pop", lang)
cap_src_geo  <- function(lang = "fr") {
  paste(tr("src", lang), tr("geo_fond", lang), sep = "\n")
}
cap_src_both <- function(lang = "fr") {
  paste(tr("src", lang), tr("src_pop", lang), sep = "\n")
}
