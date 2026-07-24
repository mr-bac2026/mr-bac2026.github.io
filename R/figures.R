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

fig_geo_population <- function(g, lang = "fr") {
  ggplot(g$par_pop, aes(x = pop, y = fct_reorder(shapeName, pop))) +
    geom_col(width = 0.68, fill = BAC_COL$vert) +
    geom_text(aes(label = format(pop, big.mark = " ")),
              hjust = bac_h(lang, -0.08),
              family = bac_font(lang), size = 2.7, colour = BAC_COL$encre) +
    bac_x_num(lang, limits = c(0, max(g$par_pop$pop) * 1.15),
              expand = expansion(c(0, 0))) +
    bac_y_disc(lang) +
    labs(title = tr("geo_pop_title", lang),
         subtitle = tr("geo_pop_sub", lang),
         x = NULL, y = NULL, caption = cap_src_pop(lang)) +
    theme_bac(lang = lang) +
    theme(axis.text.x = element_blank(), panel.grid.major = element_blank())
}

fig_geo_par_habitant <- function(g, lang = "fr") {
  ggplot(g$par_pop, aes(x = pm, y = fct_reorder(shapeName, pm))) +
    geom_vline(xintercept = g$nat_pm, colour = BAC_COL$encre_douce,
               linewidth = 0.4, linetype = "dashed") +
    geom_col(aes(fill = pm), width = 0.68) +
    geom_text(aes(label = num_fr(pm, 0.1)), hjust = bac_h(lang, -0.15),
              family = bac_font(lang), size = 2.8, colour = BAC_COL$encre) +
    annotate("text", x = g$nat_pm, y = 0.7,
             label = paste0(tr("geo_pm_moyenne", lang), num_fr(g$nat_pm, 0.1)),
             hjust = bac_h(lang, -0.05), family = bac_font(lang), size = 2.7,
             colour = BAC_COL$encre_douce) +
    scale_fill_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    bac_x_num(lang, limits = c(0, max(g$par_pop$pm) * 1.12),
              expand = expansion(c(0, 0))) +
    bac_y_disc(lang) +
    labs(title = tr("geo_pm_title", lang),
         subtitle = tr("geo_pm_sub", lang),
         x = tr("geo_pm_x", lang), y = NULL, caption = cap_src_both(lang)) +
    theme_bac(lang = lang) +
    theme(axis.text.x = element_blank(), panel.grid.major = element_blank())
}

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

fig_geo_classement <- function(g, lang = "fr") {
  ggplot(g$par_wilaya, aes(x = taux, y = fct_reorder(nom, taux))) +
    geom_segment(aes(x = bas, xend = haut, yend = nom),
                 colour = BAC_COL$trait, linewidth = 1.4) +
    geom_point(aes(colour = taux), size = 2.8) +
    geom_text(aes(label = pct_fr(taux)), hjust = bac_h(lang, -0.35),
              vjust = -0.4,
              family = bac_font(lang), size = 2.7,
              colour = BAC_COL$encre_douce) +
    scale_colour_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    bac_x_num(lang, labels = label_percent(accuracy = 1),
              limits = c(0, max(g$par_wilaya$haut) * 1.08)) +
    bac_y_disc(lang) +
    labs(title = tr("geo_rank_title", lang),
         subtitle = tr("geo_rank_sub", lang),
         x = tr("geo_rank_x", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank())
}

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
              hjust = bac_h(lang, -0.12), family = bac_font(lang), size = 3.1,
              colour = BAC_COL$encre) +
    bac_y_disc(lang, labels = lab_pole) +
    bac_x_num(lang, labels = label_percent(accuracy = 1),
              limits = c(0, max(g$par_pole$taux) * 1.5)) +
    labs(title = tr("geo_pole_title", lang),
         subtitle = tr("geo_pole_sub", lang),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank())
}

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
    bac_seuil(tr("idx_seuil_8", lang), 8, lang, vjust = 1.6, dy = 3) +
    bac_seuil(tr("idx_seuil_10", lang), 10, lang, vjust = -0.7, dy = 3) +
    scale_fill_manual(values = BAC_PAL_DECISION, labels = lab_dec, name = NULL,
                      guide = guide_legend(override.aes =
                                             list(linewidth = 0))) +
    bac_x_num(lang, breaks = seq(0, 18, 2), limits = c(0, 18)) +
    bac_y_num(lang, labels = label_number(big.mark = " "),
              expand = expansion(c(0, 0.05))) +
    labs(title = tr("idx_hist_title", lang),
         subtitle = tr("idx_hist_sub", lang),
         x = tr("ax_moyenne", lang), y = tr("idx_hist_y", lang),
         caption = cap_src(lang)) +
    theme_bac(lang = lang)
}

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
  men <- d |>
    filter(decision == "Admis") |>
    count(mention) |>
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
              hjust = bac_h(lang, -0.05), family = bac_font(lang), size = 3.1,
              colour = BAC_COL$encre) +
    scale_fill_manual(values = BAC_PAL_DECISION, guide = "none") +
    bac_y_disc(lang, labels = lab_dec) +
    bac_x_num(lang, limits = c(0, max(g$rep$n) * 1.22),
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
    bac_x_disc(lang, labels = lab_step) +
    bac_y_num(lang, labels = label_number(big.mark = " "),
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
             hjust = bac_h(lang, -0.08), vjust = 2,
             family = bac_font(lang), size = 3,
             colour = BAC_COL$vert) +
    bac_x_num(lang, breaks = seq(0, 18, 2), limits = c(0, 18)) +
    bac_y_num(lang) +
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
    bac_x_disc(lang, labels = lab_ment) +
    bac_y_num(lang, labels = label_number(big.mark = " "),
              expand = expansion(c(0, 0.16))) +
    labs(title = tr("pano_ment_title", lang),
         subtitle = tr("pano_ment_sub", lang),
         x = NULL, y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.x = element_blank())
}

filieres_data <- function() {
  suppressPackageStartupMessages(library(ggridges))
  d <- lire_bac()
  par_serie <- synth_taux(d, serie) |> arrange(desc(n))
  d <- d |> mutate(serie = factor(serie, levels = par_serie$serie))
  ps <- par_serie |> filter(n >= 100)
  dr <- d |> filter(present == 1, !is.na(moyenne), serie %in% ps$serie)
  dd <- d |>
    filter(present == 1) |>
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
    geom_text(aes(label = format(n, big.mark = " ")),
              hjust = bac_h(lang, -0.12),
              family = bac_font(lang), size = 3, colour = BAC_COL$encre) +
    bac_x_num(lang, limits = c(0, max(g$par_serie$n) * 1.15),
              expand = expansion(c(0, 0))) +
    bac_y_disc(lang) +
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
    geom_text(aes(label = pct_fr(taux)), hjust = bac_h(lang, -0.35),
              vjust = -0.5,
              family = bac_font(lang), size = 2.9,
              colour = BAC_COL$encre_douce) +
    scale_colour_gradientn(colours = BAC_SEQ_CHAUD, guide = "none") +
    bac_x_num(lang, labels = label_percent(accuracy = 1),
              limits = c(0, max(g$ps$haut) * 1.1)) +
    bac_y_disc(lang) +
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
    bac_x_num(lang, breaks = seq(0, 18, 2), limits = c(0, 18)) +
    bac_y_disc(lang) +
    labs(title = tr("fil_ridge_title", lang),
         subtitle = tr("fil_ridge_sub", lang),
         x = tr("ax_moyenne", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) +
    theme(panel.grid.major.y = element_blank())
}

reussite_data <- function() {
  d <- lire_bac()
  dm <- d |>
    filter(present == 1, !is.na(moyenne), !is.na(age_val)) |>
    mutate(
      serie  = fct_lump_min(serie, min = 200),
      serie  = fct_relevel(factor(serie), "Sciences Naturelles"),
      wilaya = fct_relevel(factor(wilaya), "Nouakchott Ouest")
    )
  mod <- glm(admis ~ serie + wilaya + age_val, data = dm, family = binomial())
  co <- summary(mod)$coefficients
  ors <- data.frame(
    terme = rownames(co),
    or    = exp(co[, "Estimate"]),
    bas   = exp(co[, "Estimate"] - 1.96 * co[, "Std. Error"]),
    haut  = exp(co[, "Estimate"] + 1.96 * co[, "Std. Error"]),
    p     = co[, "Pr(>|z|)"],
    row.names = NULL
  )
  os <- ors |>
    filter(grepl("^serie", terme)) |>
    mutate(lab = gsub("^serie", "", terme))
  ow <- ors |>
    filter(grepl("^wilaya", terme)) |>
    mutate(lab = joli_nom(gsub("^wilaya", "", terme)),
           ns  = p >= 0.05, lab = fct_reorder(lab, or))
  pa <- dm |>
    group_by(age_val) |>
    summarise(n = n(), taux = mean(admis), .groups = "drop") |>
    filter(n >= 50)
  or_age <- ors$or[ors$terme == "age_val"]
  mod0 <- glm(admis ~ 1, data = dm, family = binomial())
  mcfadden <- 1 - as.numeric(logLik(mod)) / as.numeric(logLik(mod0))
  pr <- predict(mod, type = "response")
  r  <- rank(pr)
  n1 <- sum(dm$admis == 1)
  n0 <- sum(dm$admis == 0)
  auc <- (sum(r[dm$admis == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  list(dm = dm, ors = ors, os = os, ow = ow, pa = pa,
       or_age = or_age, mcfadden = mcfadden, auc = auc)
}

fig_reu_serie <- function(g, lang = "fr") {
  ggplot(g$os, aes(x = or, y = fct_reorder(lab, or))) +
    geom_vline(xintercept = 1, colour = BAC_COL$encre_douce, linewidth = 0.4) +
    geom_segment(aes(x = bas, xend = haut, yend = lab),
                 colour = BAC_COL$trait, linewidth = 1.5) +
    geom_point(colour = BAC_COL$terre, size = 3) +
    bac_x_log(lang) +
    bac_y_disc(lang) +
    labs(title = tr("reu_serie_title", lang),
         subtitle = tr("reu_serie_sub", lang),
         x = tr("reu_or_x", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) + theme(panel.grid.major.y = element_blank())
}

fig_reu_wilaya <- function(g, lang = "fr") {
  ggplot(g$ow, aes(x = or, y = lab)) +
    geom_vline(xintercept = 1, colour = BAC_COL$encre, linewidth = 0.5) +
    geom_segment(aes(x = bas, xend = haut, yend = lab),
                 colour = BAC_COL$trait, linewidth = 1.4) +
    geom_point(aes(fill = or), shape = 21, colour = BAC_COL$papier,
               size = 3.4, stroke = 0.6) +
    geom_point(data = ~ subset(.x, ns), shape = 21, fill = NA,
               colour = BAC_COL$encre, size = 5, stroke = 0.7) +
    scale_fill_gradientn(colours = rev(BAC_SEQ_CHAUD), guide = "none") +
    bac_x_log(lang, expand = expansion(c(0.05, 0.08))) +
    bac_y_disc(lang) +
    labs(title = tr("reu_wil_title", lang),
         subtitle = tr("reu_wil_sub", lang),
         x = tr("reu_wil_x", lang), y = NULL, caption = cap_src(lang)) +
    theme_bac(lang = lang) + theme(panel.grid.major.y = element_blank())
}

fig_reu_age <- function(g, lang = "fr") {
  ggplot(g$pa, aes(x = age_val, y = taux)) +
    geom_line(colour = BAC_COL$trait, linewidth = 0.8) +
    geom_point(aes(size = n), colour = BAC_COL$terre) +
    scale_size_area(max_size = 6, guide = "none") +
    bac_y_num(lang, labels = label_percent(accuracy = 1)) +
    bac_x_num(lang, breaks = seq(15, 30, 1)) +
    labs(title = tr("reu_age_title", lang),
         subtitle = tr("reu_age_sub", lang),
         x = tr("reu_age_x", lang), y = tr("ax_taux", lang),
         caption = cap_src(lang)) +
    theme_bac(lang = lang)
}

etab_data <- function() {
  suppressPackageStartupMessages(library(gt))
  d <- lire_bac()
  tx_global <- mean(d$admis)
  par_etab <- d |>
    group_by(etab, wilaya) |>
    summarise(n = n(), admis = sum(admis),
              moyenne = mean(moyenne, na.rm = TRUE), .groups = "drop") |>
    mutate(ic_wilson(admis, n)) |>
    rename(taux = p)
  top <- par_etab |> filter(n >= 30) |> arrange(desc(taux)) |> head(12)
  solides <- par_etab |>
    filter(n >= 50, bas > tx_global) |>
    arrange(desc(taux)) |>
    head(12)
  grille <- tibble::tibble(n = 10^seq(log10(10), log10(max(par_etab$n)),
                                      length.out = 250)) |>
    mutate(se  = sqrt(tx_global * (1 - tx_global) / n),
           h95 = pmin(1, tx_global + 1.96 * se),
           b95 = pmax(0, tx_global - 1.96 * se),
           h99 = pmin(1, tx_global + 2.58 * se),
           b99 = pmax(0, tx_global - 2.58 * se))
  list(tx_global = tx_global, par_etab = par_etab, top = top,
       solides = solides, grille = grille)
}

tbl_etab_top <- function(g, lang = "fr") {
  g$top |>
    transmute(etablissement = etab, wilaya = wilaya, candidats = n,
              taux = taux, moyenne = moyenne) |>
    gt() |>
    cols_label(etablissement = tr("col_etab", lang),
               wilaya = tr("col_wilaya", lang),
               candidats = tr("col_candidats", lang),
               taux = tr("col_taux", lang),
               moyenne = tr("col_moyenne", lang)) |>
    fmt_percent(columns = taux, decimals = 1, dec_mark = ",",
                sep_mark = " ") |>
    fmt_number(columns = moyenne, decimals = 2, dec_mark = ",") |>
    data_color(columns = taux,
               fn = scales::col_numeric(BAC_SEQ_CHAUD, domain = NULL)) |>
    cols_align("left", columns = c(etablissement, wilaya)) |>
    tab_header(title = md(tr("etab_top_title", lang)),
               subtitle = tr("etab_top_sub", lang)) |>
    tab_source_note(md(tr("src_note", lang))) |>
    tab_options(table.font.size = px(13), table.width = pct(100),
                heading.title.font.size = px(16))
}

tbl_etab_solides <- function(g, lang = "fr") {
  g$solides |>
    transmute(etablissement = etab, wilaya = wilaya, candidats = n,
              taux = taux, ic_bas = bas) |>
    gt() |>
    cols_label(etablissement = tr("col_etab", lang),
               wilaya = tr("col_wilaya", lang),
               candidats = tr("col_candidats", lang),
               taux = tr("col_taux_court", lang),
               ic_bas = tr("col_ic_bas", lang)) |>
    fmt_percent(columns = c(taux, ic_bas), decimals = 1, dec_mark = ",",
                sep_mark = " ") |>
    data_color(columns = taux,
               fn = scales::col_numeric(BAC_SEQ_CHAUD, domain = NULL)) |>
    cols_align("left", columns = c(etablissement, wilaya)) |>
    tab_header(title = md(tr("etab_sol_title", lang)),
               subtitle = tr("etab_sol_sub", lang)) |>
    tab_source_note(md(tr("src_note", lang))) |>
    tab_options(table.font.size = px(13), table.width = pct(100),
                heading.title.font.size = px(16))
}

fig_etab_funnel <- function(g, lang = "fr") {
  n_lab  <- 12
  se_lab <- sqrt(g$tx_global * (1 - g$tx_global) / n_lab)
  ggplot() +
    geom_ribbon(data = g$grille, aes(n, ymin = b99, ymax = h99),
                fill = "#eef1f4") +
    geom_ribbon(data = g$grille, aes(n, ymin = b95, ymax = h95),
                fill = "#d3e0f7") +
    geom_hline(yintercept = g$tx_global, colour = BAC_COL$terre,
               linewidth = 0.7) +
    geom_point(data = g$par_etab |> filter(n >= 10), aes(n, taux, size = n),
               shape = 21, fill = scales::alpha(BAC_COL$encre_douce, 0.35),
               colour = BAC_COL$papier, stroke = 0.2) +
    annotate("text", x = n_lab, y = g$tx_global + 2.58 * se_lab,
             label = "99 %", hjust = bac_h(lang, 0), vjust = -0.35,
             family = bac_font(lang), size = 2.9,
             colour = BAC_COL$encre_douce) +
    annotate("text", x = n_lab, y = g$tx_global + 1.96 * se_lab,
             label = "95 %", hjust = bac_h(lang, 0), vjust = -0.35,
             family = bac_font(lang), size = 2.9,
             colour = BAC_COL$encre_douce) +
    annotate("label", x = 10, y = g$tx_global,
             label = paste0(tr("etab_fun_moyenne", lang),
                            pct_fr(g$tx_global)),
             hjust = bac_h(lang, 0), vjust = 0.5,
             family = bac_font(lang), size = 2.9,
             colour = BAC_COL$terre, fill = BAC_COL$papier,
             label.size = 0, label.padding = unit(1.6, "pt")) +
    scale_size_area(max_size = 6, guide = "none") +
    bac_x_log(lang, labels = label_number(big.mark = " "),
              expand = expansion(c(0.02, 0.03))) +
    bac_y_num(lang, labels = label_percent(accuracy = 1),
              limits = c(0, NA)) +
    labs(title = tr("etab_fun_title", lang),
         subtitle = tr("etab_fun_sub", lang),
         x = tr("etab_fun_x", lang), y = tr("ax_taux", lang),
         caption = cap_src(lang)) +
    theme_bac(lang = lang)
}
