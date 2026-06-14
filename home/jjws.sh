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

# `.envrc`/`.direnv` are git-ignored, so they don't come along with the
# workspace. Copy them over and re-allow so direnv works in the workspace too.
if [ -f "${origin}/.envrc" ] && command -v direnv >/dev/null 2>&1; then
	cp "${origin}/.envrc" "${ws}/.envrc"
	[ -d "${origin}/.direnv" ] && cp -r "${origin}/.direnv" "${ws}/.direnv"
	direnv allow "$ws"
fi

cd "$ws"
"${SHELL:-bash}" || true
