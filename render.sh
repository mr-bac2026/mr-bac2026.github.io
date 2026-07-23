#!/usr/bin/env bash
set -euo pipefail

# Site multilingue (fr / en / ar). Chaque langue est un profil Quarto avec sa
# propre navbar et son dossier de préparation _build/<lang>/ (un render de
# projet nettoie son output-dir, d'où le rendu séparé puis la fusion).
#
# On rend le PROJET, jamais les pages une par une : le cache « freeze » n'est
# consulté que lors d'un rendu de projet, si bien qu'une boucle page par page
# ré-exécutait tout le code R à chaque build.

LANGS=(fr en ar)

for lang in "${LANGS[@]}"; do
  echo ">> profil $lang"
  quarto render --profile "$lang"
done

# --- Fusion dans docs/ ------------------------------------------------------
# Ressources communes (identiques d'un profil à l'autre) prises côté fr, puis
# les pages de chaque langue.
echo ">> fusion dans docs/"
rm -rf docs
mkdir -p docs/pages
cp -r _build/fr/site_libs docs/
cp -r _build/fr/assets docs/
cp _build/fr/index.html docs/
cp _build/fr/robots.txt docs/
touch docs/.nojekyll

for lang in "${LANGS[@]}"; do
  cp -r "_build/$lang/pages/$lang" docs/pages/
done

# L'index de recherche et le sitemap sont produits par profil, mais le site
# fusionné n'en sert qu'un seul à la racine : on concatène les trois (les
# entrées portent des chemins pages/<lang>/..., donc sans collision).
python3 - "${LANGS[@]}" <<'PY'
import json, sys, re, pathlib

langs = sys.argv[1:]

index = []
for lang in langs:
    p = pathlib.Path(f"_build/{lang}/search.json")
    if p.exists():
        index += json.loads(p.read_text(encoding="utf-8"))
pathlib.Path("docs/search.json").write_text(
    json.dumps(index, ensure_ascii=False), encoding="utf-8")

urls = []
for lang in langs:
    p = pathlib.Path(f"_build/{lang}/sitemap.xml")
    if p.exists():
        urls += re.findall(r"  <url>.*?</url>", p.read_text(encoding="utf-8"),
                           re.S)
pathlib.Path("docs/sitemap.xml").write_text(
    '<?xml version="1.0" encoding="UTF-8"?>\n'
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n'
    + "\n".join(urls) + "\n</urlset>\n", encoding="utf-8")

print(f">> search.json : {len(index)} entrées — sitemap.xml : {len(urls)} URL")
PY

echo ">> docs/ assemblé (${LANGS[*]})"
