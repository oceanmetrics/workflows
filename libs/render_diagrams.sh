#!/usr/bin/env bash
# Pre-render every diagrams/*.mmd to diagrams/*.svg with mermaid-cli, so the notebook
# embeds SVGs (zoomable through the diagram viewer, libs/diagram.js) and no browser
# runs during `quarto render`. Run after editing a .mmd; commit the .svg beside it.
# Adapted from CalCOFI/docs/libs/render_diagrams.sh.
#
#   npm install            # optional: installs the mermaid-cli pinned in package.json
#   libs/render_diagrams.sh [name ...]   # all, or the named diagrams
#
# uses node_modules/.bin/mmdc when present, else mmdc on the PATH (brew install mermaid-cli)
set -euo pipefail
cd "$(dirname "$0")/.."
if [ -x node_modules/.bin/mmdc ]; then
  MMDC=node_modules/.bin/mmdc
elif command -v mmdc >/dev/null 2>&1; then
  MMDC=$(command -v mmdc)
else
  echo "mmdc not found: run npm install (pins @mermaid-js/mermaid-cli in package.json) or install mermaid-cli" >&2
  exit 1
fi
names=("$@")
[ ${#names[@]} -eq 0 ] && names=($(ls diagrams/*.mmd | xargs -n1 basename | sed 's/\.mmd$//'))
for n in "${names[@]}"; do
  "$MMDC" -q -i "diagrams/$n.mmd" -o "diagrams/$n.svg" -b transparent -c libs/mermaid.config.json
  # mmdc writes an <svg> with a max-width style; strip it so the figure fills its column
  # and the viewer scales it. mermaid emits each word as a <tspan> with a leading space,
  # which librsvg (the pdf path) strips under the default whitespace mode; xml:space=
  # "preserve" on the root keeps the spaces in every renderer (the file has no indentation)
  # (sed -i differs between BSD and GNU, so write through a temp file)
  sed -E -e 's/ style="max-width: [0-9.]+px;"//' -e 's/^<svg /<svg xml:space="preserve" /' \
    "diagrams/$n.svg" > "diagrams/$n.svg.tmp" && mv "diagrams/$n.svg.tmp" "diagrams/$n.svg"
  echo "diagrams/$n.svg  $(wc -c < "diagrams/$n.svg") bytes"
done
