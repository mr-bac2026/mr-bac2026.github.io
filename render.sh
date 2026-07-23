#!/usr/bin/env bash
# Build fiable du site multilingue.
#
# Quarto peut échouer sur un `quarto render` de projet « à froid » (cache
# freeze vide) avec une erreur knitr « cannot open file '<page>.qmd' ».
# Le rendu fichier par fichier ne déclenche pas ce bug : on réchauffe donc
# le cache page par page, puis on assemble le site (recherche, plan, etc.).
set -euo pipefail

for f in pages/*/*.qmd; do
  echo ">> $f"
  quarto render "$f"
done

echo ">> assemblage du site"
quarto render
