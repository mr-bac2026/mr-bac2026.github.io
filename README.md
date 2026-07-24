# Baccalauréat 2026

Analyse statistique des résultats du baccalauréat mauritanien (session normale 2026), publiée en trois langues : français, anglais et arabe. Le français est la version de référence, complète ; l'anglais et l'arabe sont en cours de traduction. Construit avec [Quarto](https://quarto.org) et R.

Site en ligne : <https://mr-bac2026.github.io>

## Contenu

Sept pages d'analyse, déclinées par langue : ouverture, panorama général, géographie (cartes par wilaya), filières, facteurs de réussite (régression logistique), établissements (graphique en entonnoir) et méthode. La version arabe est rendue de droite à gauche, figures comprises.

## Structure

| Chemin | Rôle |
| --- | --- |
| `pages/{fr,en,ar}/` | Les sept pages d'analyse (`.qmd`), une version par langue |
| `R/common.R` | Données, nettoyage, palette, thème, fonds de carte |
| `R/figures.R` | Fonctions de figures partagées par les trois langues |
| `R/i18n.R` | Chargement des traductions et fonction `tr()` |
| `l10n/l10n.json` | Libellés traduits (fr / en / ar) |
| `assets/` | Feuille de style et favicon |
| `fonts/` | Polices embarquées (Source Serif 4, Amiri pour l'arabe) |
| `data/` | Résultats bruts, un candidat par ligne (`bac2026.csv`, `bac2025.csv`) |
| `mrt_adm1.geojson` | Fond de carte des wilayas (geoBoundaries) |
| `_quarto.yml` + `_quarto-{fr,en,ar}.yml` | Configuration commune et profils par langue |
| `render.sh` | Rend les trois langues et assemble `docs/` |
| `docs/` | Site rendu, publié par GitHub Pages |

## Développement

Prérequis : R (≥ 4.5), Quarto (≥ 1.9), et les bibliothèques système HarfBuzz et FriBidi, dont le paquet `ragg` se sert pour la mise en forme de l'arabe (liaison des lettres et sens droite-à-gauche).

```bash
Rscript install_pkgs.R        # installe les paquets R
./render.sh                   # rend fr, en, ar puis assemble docs/
quarto preview --profile fr   # aperçu local d'une langue
```

Chaque langue est un profil Quarto rendu dans `_build/<lang>/`, puis fusionné dans `docs/` par `render.sh` (ressources partagées, index de recherche et sitemap réunis). Un `quarto render` sans profil écrit dans un bac à sable et ne met pas à jour le site.

## Déploiement

Chaque poussée sur `main` déclenche le workflow [`.github/workflows/deploy.yml`](.github/workflows/deploy.yml), qui publie le dossier `docs/` sur GitHub Pages. Le site étant pré-rendu, lancez `./render.sh` avant de pousser.

## Sources

- Données : Direction en charge des examens (sessions normales 2026 et 2025).
- Fond de carte : [geoBoundaries](https://www.geoboundaries.org), niveau ADM1.
- Population : recensement RGPH 2023 (ANSADE).

Méthode, traitements et limites sont détaillés dans la page Méthode du site.
