#!/usr/bin/env bash
set -euo pipefail

for f in pages/*/*.qmd; do
  echo ">> $f"
  quarto render "$f"
done

echo ">> assemblage du site"
quarto render
