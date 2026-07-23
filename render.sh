#!/usr/bin/env bash
set -euo pipefail

for f in pages/*/*.qmd; do
  quarto render "$f" >/dev/null
done

for lang in fr en ar; do
  echo ">> profil $lang"
  quarto render --profile "$lang"
done

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
