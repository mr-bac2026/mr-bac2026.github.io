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
