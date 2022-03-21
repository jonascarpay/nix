#!/usr/bin/env bash

declare -A URLS

URLS=(
  ["Duckduckgo"]="https://www.duckduckgo.com/?q="
  ["Google"]="https://www.google.com/search?q="
  ["Hoogle-local"]="http://localhost:8080/?hoogle="
  ["Hoogle"]="https://hoogle.haskell.org/?hoogle="
  ["Hackage"]="https://hackage.haskell.org/packages/search?terms="
  ["Wolfram-alpha"]="https://www.wolframalpha.com/input?i="
  ["URL"]="https://"
)

gen_list() {
  for i in "${!URLS[@]}"; do
    echo "$i"
  done
}

ENGINE_HISTORY=~/.local/share/rofi/rofi-engine-history

ENGINE=$( (gen_list) | xargs frecently $ENGINE_HISTORY view | rofi -dmenu -i -matching fuzzy -no-custom -p "Engine")

if [[ -n "$ENGINE" ]]; then

  HISTORY=~/.local/share/rofi/rofi-$ENGINE-search-history

  QUERY=$(frecently $HISTORY view | rofi -dmenu -i -matching fuzzy -p "$ENGINE search")

  if [[ -n "$QUERY" ]]; then
    URL=${URLS[$ENGINE]}$QUERY
    xdg-open "$URL"
    frecently $ENGINE_HISTORY bump "$ENGINE"
    frecently "$HISTORY" bump "$QUERY"
  fi
fi
