# =============================================================================
#  FIGURES PARTAGÉES — Géographie
#  Chaque fonction prend les données préparées (geo_data()) et une langue,
#  et tire tous ses textes de tr(). Appelée depuis fr/, en/ et ar/.
# =============================================================================

# Prépare une fois toutes les tables utilisées par les figures de la page.
geo_data <- function() {
  d <- lire_bac()
  carte <- lire_carte()

  par_wilaya <- synth_taux(d, wilaya) |>
    arrange(desc(taux)) |>
    mutate(nom = joli_nom(wilaya))

  d <- d |> mutate(shapeName = BAC_CROSSWALK[wilaya])
  par_forme <- synth_taux(d, shapeName)
  carte_j <- carte |> left_join(par_forme, by = "shapeName")

  par_pop <- par_forme |>
    mutate(pop = BAC_POP_2023[shapeName], pm = 1000 * n / pop)
  par_pole <- synth_taux(d, pole)
  nat_pm <- 1000 * sum(par_pop$n) / sum(par_pop$pop)

  list(d = d, carte_j = carte_j, par_wilaya = par_wilaya,
       par_forme = par_forme, par_pop = par_pop, par_pole = par_pole,
       nat_pm = nat_pm)
}

# Carte : effectifs de candidats par région.
fig_geo_effectifs <- function(g, lang = "fr") {
  ggplot(g$carte_j, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = n), colour = BAC_COL$papier, linewidth = 0.3) +
    scale_fill_gradientn(colours = BAC_SEQ_FROID,
                         name = tr("geo_eff_legend", lang),
                         labels = label_number(big.mark = " "),
                         trans = "sqrt") +
    coord_quickmap() +
    labs(title = tr("geo_eff_title", lang),
         subtitle = tr("geo_eff_sub", lang),
         caption = cap_src_geo(lang)) +
    theme_bac_carte(lang)
}

# Barres : population par région (RGPH 2023).
fig_geo_population <- function(g, lang = "fr") {
  ggplot(g$par_pop, aes(x = pop, y = fct_reorder(shapeName, pop))) +
    geom_col(width = 0.68, fill = BAC_COL$vert) +
    geom_text(aes(label = format(pop, big.mark = " ")), hjust = -0.08,
              family = bac_font(lang), size = 2.7, colour = BAC_COL$encre) +
    scale_x_continuous(limits = c(0, max(g$par_pop$pop) * 1.15),
                       expand = expansion(c(0, 0))) +
    labs(title = tr("geo_pop_title", lang),
         subtitle = tr("geo_pop_sub", lang),
         x = NULL, y = NULL, caption = cap_src_pop(lang)) +
    theme_bac(lang = lang) +
    theme(axis.text.x = element_blank(), panel.grid.major = element_blank())
}

# Barres : candidats pour 1 000 habitants, avec moyenne nationale.
fig_geo_par_habitant <- function(g, lang = "fr") {
  ggplot(g$par_pop, aes(x = pm, y = fct_reorder(shapeName, pm))) +
    geom_vline(xintercept = g$nat_pm, colour = BAC_COL$encre_douce,
               linewidth = 0.4, linetype = "dashed") +
    geom_col(aes(fill = pm), width = 0.68) +
    geom_text(aes(label = num_fr(pm, 0.1)), hjust = -0.15,
              family = bac_font(lang), size = 2.8, colour = BAC_COL$encre) +
    annotate("text", x = g$nat_pm, y = 0.7,
             label = paste0(tr("geo_pm_moyenne", lang), num_fr(g$nat_pm, 0.1)),
             hjust = -0.05, family = bac_font(lang), size = 2.7,
             colour = BAC_COL$encre_douce) +
    scale_fill_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    scale_x_continuous(limits = c(0, max(g$par_pop$pm) * 1.12),
                       expand = expansion(c(0, 0))) +
    labs(title = tr("geo_pm_title", lang),
         subtitle = tr("geo_pm_sub", lang),
         x = tr("geo_pm_x", lang), y = NULL, caption = cap_src_both(lang)) +
    theme_bac(lang = lang) +
    theme(axis.text.x = element_blank(), panel.grid.major = element_blank())
}

# Carte : taux d'admission par région.
fig_geo_carte_taux <- function(g, lang = "fr") {
  ggplot(g$carte_j, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = taux), colour = BAC_COL$papier, linewidth = 0.3) +
    scale_fill_gradientn(colours = BAC_SEQ_CHAUD,
                         name = tr("geo_taux_legend", lang),
                         labels = label_percent(accuracy = 1)) +
    coord_quickmap() +
    labs(title = tr("geo_taux_title", lang),
         subtitle = tr("geo_taux_sub", lang),
         caption = cap_src_geo(lang)) +
    theme_bac_carte(lang)
}

# Classement des wilayas par taux d'admission (IC 95 % de Wilson).
fig_geo_classement <- function(g, lang = "fr") {
  ggplot(g$par_wilaya, aes(x = taux, y = fct_reorder(nom, taux))) +
    geom_segment(aes(x = bas, xend = haut, yend = nom),
                 colour = BAC_COL$trait, linewidth = 1.4) +
    geom_point(aes(colour = taux), size = 2.8) +
    geom_text(aes(label = pct_fr(taux)), hjust = -0.35, vjust = -0.4,
              family = bac_font(lang), size = 2.7,
              colour = BAC_COL$encre_douce) +
    scale_colour_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    scale_x_continuous(labels = label_percent(accuracy = 1),
                       limits = c(0, max(g$par_wilaya$haut) * 1.08)) +
    labs(title = tr("geo_rank_title", lang),
         subtitle = tr("geo_rank_sub", lang),
         x = tr("geo_rank_x", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank())
}

# Comparaison Nouakchott / intérieur.
fig_geo_pole <- function(g, lang = "fr") {
  lab_pole <- c("Nouakchott" = tr("pole_nkc", lang),
                "Intérieur"  = tr("pole_int", lang))
  ggplot(g$par_pole, aes(x = taux, y = pole)) +
    geom_segment(aes(x = 0, xend = taux, yend = pole),
                 colour = BAC_COL$trait, linewidth = 5) +
    geom_point(size = 5, colour = BAC_COL$terre) +
    geom_text(aes(label = paste0(pct_fr(taux), "  (",
                                 format(n, big.mark = " "), " ",
                                 tr("word_candidats", lang), ")")),
              hjust = -0.12, family = bac_font(lang), size = 3.1,
              colour = BAC_COL$encre) +
    scale_y_discrete(labels = lab_pole) +
    scale_x_continuous(labels = label_percent(accuracy = 1),
                       limits = c(0, max(g$par_pole$taux) * 1.5)) +
    labs(title = tr("geo_pole_title", lang),
         subtitle = tr("geo_pole_sub", lang),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank())
}

# =============================================================================
#  FIGURES PARTAGÉES — Ouverture (index)
# =============================================================================
index_data <- function() {
  d <- lire_bac()
  pres <- d |> filter(present == 1, !is.na(moyenne))
  list(d = d, pres = pres, n_tot = nrow(d), n_adm = sum(d$admis),
       tx_adm = sum(d$admis) / nrow(d),
       moy_pres = mean(d$moyenne[d$present == 1], na.rm = TRUE))
}

fig_index_hist <- function(g, lang = "fr") {
  lab_dec <- c("Admis" = tr("dec_admis", lang),
               "Sessionnaire" = tr("dec_sess", lang),
               "Ajourné" = tr("dec_ajourne", lang),
               "Absent" = tr("dec_absent", lang),
               "Examen annulé" = tr("dec_annule", lang))
  ggplot(g$pres, aes(x = moyenne, fill = decision)) +
    geom_histogram(binwidth = 0.5, colour = BAC_COL$papier, linewidth = 0.15,
                   boundary = 0, closed = "left") +
    geom_vline(xintercept = c(8, 10), colour = BAC_COL$encre,
               linewidth = 0.4, linetype = "dashed") +
    annotate("text", x = 8, y = Inf, label = tr("idx_seuil_8", lang),
             family = bac_font(lang), hjust = 1.03, vjust = 1.6, size = 3,
             colour = BAC_COL$encre_douce, angle = 90) +
    annotate("text", x = 10, y = Inf, label = tr("idx_seuil_10", lang),
             family = bac_font(lang), hjust = 1.03, vjust = -0.7, size = 3,
             colour = BAC_COL$encre_douce, angle = 90) +
    scale_fill_manual(values = BAC_PAL_DECISION, labels = lab_dec, name = NULL,
                      guide = guide_legend(override.aes =
                                             list(linewidth = 0))) +
    scale_x_continuous(breaks = seq(0, 18, 2), limits = c(0, 18)) +
    scale_y_continuous(labels = label_number(big.mark = " "),
                       expand = expansion(c(0, 0.05))) +
    labs(title = tr("idx_hist_title", lang),
         subtitle = tr("idx_hist_sub", lang),
         x = tr("ax_moyenne", lang), y = tr("idx_hist_y", lang),
         caption = cap_src(lang)) +
    theme_bac(lang = lang)
}

# =============================================================================
#  FIGURES PARTAGÉES — Panorama
# =============================================================================
panorama_data <- function() {
  d <- lire_bac()
  n_tot <- nrow(d)
  rep <- d |> count(decision) |> mutate(part = n / sum(n))
  niv <- c("Inscrits", "Présents à l'épreuve", "Moyenne ≥ 8", "Admis (≥ 10)")
  etapes <- tibble::tibble(
    etape = factor(niv, levels = niv),
    n = c(n_tot, sum(d$present == 1),
          sum(d$present == 1 & d$moyenne >= 8, na.rm = TRUE), sum(d$admis))
  ) |> mutate(part = n / n_tot)
  pres <- d |> filter(present == 1, !is.na(moyenne))
  men <- d |> filter(decision == "Admis") |> count(mention) |>
    mutate(part = n / sum(n))
  list(d = d, n_tot = n_tot, rep = rep, etapes = etapes,
       pres = pres, med = median(pres$moyenne), men = men)
}

fig_pano_decisions <- function(g, lang = "fr") {
  lab_dec <- c("Admis" = tr("dec_admis", lang),
               "Sessionnaire" = tr("dec_sess", lang),
               "Ajourné" = tr("dec_ajourne", lang),
               "Absent" = tr("dec_absent", lang),
               "Examen annulé" = tr("dec_annule", lang))
  ggplot(g$rep, aes(x = n, y = fct_rev(decision), fill = decision)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = paste0(format(n, big.mark = " "), "  ·  ",
                                 pct_fr(part))),
              hjust = -0.05, family = bac_font(lang), size = 3.1,
              colour = BAC_COL$encre) +
    scale_fill_manual(values = BAC_PAL_DECISION, guide = "none") +
    scale_y_discrete(labels = lab_dec) +
    scale_x_continuous(limits = c(0, max(g$rep$n) * 1.22),
                       expand = expansion(c(0, 0))) +
    labs(title = tr("pano_dec_title", lang),
         subtitle = paste0(tr("pano_dec_sub_a", lang),
                           format(g$n_tot, big.mark = " "),
                           tr("pano_dec_sub_b", lang)),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_blank(), panel.grid.major.x = element_blank())
}

fig_pano_entonnoir <- function(g, lang = "fr") {
  lab_step <- c("Inscrits" = tr("step_inscrits", lang),
                "Présents à l'épreuve" = tr("step_presents", lang),
                "Moyenne ≥ 8" = tr("step_moy8", lang),
                "Admis (≥ 10)" = tr("step_admis", lang))
  ggplot(g$etapes, aes(x = etape, y = n)) +
    geom_col(width = 0.6, fill = BAC_COL$terre) +
    geom_text(aes(label = paste0(format(n, big.mark = " "), "\n",
                                 pct_fr(part, 1))),
              vjust = -0.25, family = bac_font(lang), size = 3.1,
              colour = BAC_COL$encre, lineheight = 0.95) +
    scale_x_discrete(labels = lab_step) +
    scale_y_continuous(labels = label_number(big.mark = " "),
                       expand = expansion(c(0, 0.15))) +
    labs(title = tr("pano_ent_title", lang),
         subtitle = tr("pano_ent_sub", lang),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.x = element_blank())
}

fig_pano_densite <- function(g, lang = "fr") {
  ggplot(g$pres, aes(x = moyenne)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, boundary = 0,
                   fill = BAC_COL$papier_alt, colour = BAC_COL$papier,
                   linewidth = 0.15) +
    geom_density(colour = BAC_COL$terre, linewidth = 0.9, adjust = 1.1) +
    geom_vline(xintercept = c(8, 10), linetype = "dashed",
               colour = BAC_COL$encre_douce, linewidth = 0.4) +
    geom_vline(xintercept = g$med, colour = BAC_COL$vert, linewidth = 0.6) +
    annotate("text", x = g$med, y = Inf,
             label = paste0(tr("pano_den_med", lang), num_fr(g$med, 0.1)),
             hjust = -0.08, vjust = 2, family = bac_font(lang), size = 3,
             colour = BAC_COL$vert) +
    scale_x_continuous(breaks = seq(0, 18, 2), limits = c(0, 18)) +
    labs(title = tr("pano_den_title", lang),
         subtitle = tr("pano_den_sub", lang),
         x = tr("ax_moyenne", lang), y = tr("ax_densite", lang),
         caption = cap_src(lang)) +
    theme_bac(lang = lang)
}

fig_pano_mentions <- function(g, lang = "fr") {
  lab_ment <- c("Passable" = tr("ment_passable", lang),
                "Assez bien" = tr("ment_ab", lang),
                "Bien" = tr("ment_bien", lang),
                "Très bien" = tr("ment_tb", lang))
  ggplot(g$men, aes(x = mention, y = n)) +
    geom_col(width = 0.66, fill = BAC_COL$or) +
    geom_text(aes(label = paste0(format(n, big.mark = " "), " · ",
                                 pct_fr(part))),
              vjust = -0.3, family = bac_font(lang), size = 3,
              colour = BAC_COL$encre) +
    scale_x_discrete(labels = lab_ment) +
    scale_y_continuous(labels = label_number(big.mark = " "),
                       expand = expansion(c(0, 0.16))) +
    labs(title = tr("pano_ment_title", lang),
         subtitle = tr("pano_ment_sub", lang),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.x = element_blank())
}

# =============================================================================
#  FIGURES PARTAGÉES — Filières
# =============================================================================
filieres_data <- function() {
  suppressPackageStartupMessages(library(ggridges))
  d <- lire_bac()
  par_serie <- synth_taux(d, serie) |> arrange(desc(n))
  d <- d |> mutate(serie = factor(serie, levels = par_serie$serie))
  ps <- par_serie |> filter(n >= 100)
  dr <- d |> filter(present == 1, !is.na(moyenne), serie %in% ps$serie)
  dd <- d |> filter(present == 1) |>
    mutate(issue = if_else(admis == 1, "Admis", "Non admis"))
  tab <- table(dd$serie, dd$issue)
  khi <- chisq.test(tab)
  v_cramer <- sqrt(as.numeric(khi$statistic) /
                     (sum(tab) * (min(dim(tab)) - 1)))
  kw <- kruskal.test(moyenne ~ serie,
                     data = d |> filter(present == 1, serie %in% ps$serie))
  list(d = d, par_serie = par_serie, ps = ps, dr = dr,
       khi = khi, v_cramer = v_cramer, kw = kw)
}

fig_fil_effectifs <- function(g, lang = "fr") {
  ggplot(g$par_serie, aes(x = n, y = fct_reorder(serie, n))) +
    geom_col(width = 0.68, fill = BAC_COL$encre_douce) +
    geom_text(aes(label = format(n, big.mark = " ")), hjust = -0.12,
              family = bac_font(lang), size = 3, colour = BAC_COL$encre) +
    scale_x_continuous(limits = c(0, max(g$par_serie$n) * 1.15),
                       expand = expansion(c(0, 0))) +
    labs(title = tr("fil_eff_title", lang),
         subtitle = tr("fil_eff_sub", lang),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(axis.text.x = element_blank(), panel.grid.major = element_blank())
}

fig_fil_taux <- function(g, lang = "fr") {
  ggplot(g$ps, aes(x = taux, y = fct_reorder(serie, taux))) +
    geom_segment(aes(x = bas, xend = haut, yend = serie),
                 colour = BAC_COL$trait, linewidth = 1.5) +
    geom_point(aes(colour = taux), size = 3.2) +
    geom_text(aes(label = pct_fr(taux)), hjust = -0.35, vjust = -0.5,
              family = bac_font(lang), size = 2.9,
              colour = BAC_COL$encre_douce) +
    scale_colour_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    scale_x_continuous(labels = label_percent(accuracy = 1),
                       limits = c(0, max(g$ps$haut) * 1.1)) +
    labs(title = tr("fil_taux_title", lang),
         subtitle = tr("fil_taux_sub", lang),
         x = tr("ax_taux", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) + theme(panel.grid.major.y = element_blank())
}

fig_fil_ridgeline <- function(g, lang = "fr") {
  ggplot(g$dr, aes(x = moyenne,
                   y = fct_reorder(serie, moyenne, .fun = median),
                   fill = after_stat(x))) +
    geom_density_ridges_gradient(scale = 2.1, rel_min_height = 0.01,
                                 colour = BAC_COL$papier, linewidth = 0.3) +
    geom_vline(xintercept = 10, linetype = "dashed", colour = BAC_COL$encre,
               linewidth = 0.4) +
    scale_fill_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    scale_x_continuous(breaks = seq(0, 18, 2), limits = c(0, 18)) +
    labs(title = tr("fil_ridge_title", lang),
         subtitle = tr("fil_ridge_sub", lang),
         x = tr("ax_moyenne", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank())
}
