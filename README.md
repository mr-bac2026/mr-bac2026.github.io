# Baccalauréat 2026

Analyse statistique des résultats du baccalauréat mauritanien (session normale
2026), en français. Construit avec [Quarto](https://quarto.org) et R.

Site en ligne : https://mr-bac2026.github.io

## Contenu

Sept pages d'analyse : panorama général, géographie (cartes par wilaya),
filières, facteurs de réussite (régression logistique), établissements
(graphique en entonnoir) et méthode.

## Structure

| Chemin | Rôle |
|---|---|
| `pages/` | Les sept pages d'analyse (`.qmd`) |
| `R/common.R` | Données, nettoyage, palette, thème, fond de carte |
| `assets/` | Feuille de style et favicon |
| `data/` | Résultats bruts (un candidat par ligne) |
| `mrt_adm1.geojson` | Fond de carte des wilayas (geoBoundaries) |
| `docs/` | Site rendu, publié par GitHub Pages |
| `_quarto.yml` | Configuration du site |

## Développement

Prérequis : R (≥ 4.5) et Quarto (≥ 1.9).

```bash
Rscript install_pkgs.R   # installe les paquets R
quarto render            # régénère docs/
quarto preview           # aperçu local
```

## Déploiement

Chaque poussée sur `main` déclenche le workflow
[`.github/workflows/deploy.yml`](.github/workflows/deploy.yml), qui publie le
dossier `docs/` sur GitHub Pages. Le site étant pré-rendu, pensez à lancer
`quarto render` avant de pousser.

## Sources

- Données : Direction en charge des examens (session normale 2026).
- Fond de carte : [geoBoundaries](https://www.geoboundaries.org), niveau ADM1.
- Population : recensement RGPH 2023 (ANSADE).

Méthode, traitements et limites sont détaillés dans la page Méthode du site.
