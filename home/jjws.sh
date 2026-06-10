#!/usr/bin/env bash
# Start a shell in a throwaway jujutsu workspace, cleaning it up on exit.
set -euo pipefail

origin=$(pwd)
origin_name=$(basename "$origin")
timestamp=$(date +%Y%m%d_%H%M%S)
workspaces="${TMPDIR:-/tmp}/workspaces"
mkdir -p "$workspaces"
ws="${workspaces}/${origin_name}_${timestamp}"
name=$(basename "$ws")

cleanup() {
	# `workspace forget` does not snapshot, so we force snapshot before forgetting
	jj util snapshot --repository "$ws" 2>/dev/null || true
	jj workspace forget --repository "$origin" "$name" 2>/dev/null || true
	rm -rf "$ws"
}
trap cleanup EXIT

jj workspace add --quiet --name "$name" "$ws"
jj workspace list
cd "$ws"
"${SHELL:-bash}" || true
