#!/usr/bin/env bash
set -euo pipefail

# Site multilingue (fr / en / ar). Chaque langue est un profil Quarto avec sa
# propre navbar et son dossier de préparation _build/<lang>/ (un render de
# projet nettoie son output-dir, d'où le rendu séparé puis la fusion).
for lang in fr en ar; do
  echo ">> profil $lang"
  # Réchauffe le cache (freeze) page par page dans _build/<lang> : contourne un
  # bug de Quarto au render projet « à froid » sans jamais toucher docs/.
  for f in pages/"$lang"/*.qmd; do
    quarto render "$f" --profile "$lang" >/dev/null
  done
  quarto render --profile "$lang"
done

# Fusionne dans docs/ : ressources partagées + pages de chaque langue.
rm -rf docs
mkdir -p docs/pages
cp -r _build/fr/site_libs docs/
[ -d _build/fr/assets ] && cp -r _build/fr/assets docs/
[ -f _build/fr/index.html ] && cp _build/fr/index.html docs/
[ -f _build/fr/robots.txt ] && cp _build/fr/robots.txt docs/
[ -f _build/fr/.nojekyll ] && cp _build/fr/.nojekyll docs/ || touch docs/.nojekyll
for lang in fr en ar; do
  cp -r "_build/$lang/pages/$lang" docs/pages/
done

echo ">> docs/ assemblé (fr + en + ar)"
