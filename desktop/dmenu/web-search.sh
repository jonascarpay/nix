#!/usr/bin/env bash

set -e

declare -A URLS

URLS=(
  ["Duckduckgo"]="https://www.duckduckgo.com/?q="
  ["Google"]="https://www.google.com/search?q="
  ["Github-nixpkgs"]="https://github.com/NixOS/nixpkgs/search?q="
  ["Github-repo"]="https://github.com/"
  ["Hoogle-local"]="http://localhost:8080/?hoogle="
  ["Hoogle"]="https://hoogle.haskell.org/?hoogle="
  ["Hackage"]="https://hackage.haskell.org/package/"
  ["Hackage-search"]="https://hackage.haskell.org/packages/search?terms="
  ["Wolfram-alpha"]="https://www.wolframalpha.com/input?i="
  ["URL"]="https://"
)

gen_list() {
  for i in "${!URLS[@]}"; do
    echo "$i"
  done
}

ENGINE_HISTORY=~/.local/share/frecently/search-engine-history

ENGINE=$(gen_list | frecently view $ENGINE_HISTORY -ar | dmenu -i -sr -p "爵 ")

if [[ -n "$ENGINE" ]]; then

  HISTORY=~/.local/share/frecently/search-$ENGINE-history

  QUERY=$(frecently view $HISTORY | dmenu -p "$ENGINE")

  if [[ -n "$QUERY" ]]; then
    frecently bump "$ENGINE_HISTORY" "$ENGINE"
    frecently bump "$HISTORY" "$QUERY"
    URL=${URLS[$ENGINE]}$QUERY
    xdg-open "$URL"
  fi
fi
