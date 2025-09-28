#!/bin/sh
# Usage: ./grove-clone.sh <url> [name] [extra-clone-args...]

set -e

if [ -z "$1" ]; then
	echo "Usage: $0 <git-url> [name] [extra-clone-args...]" >&2
	exit 1
fi

GIT_URL="$1"
shift

if [ -n "$1" ] && ! expr "$1" : '-' >/dev/null; then
	DIR_NAME="$1"
	shift
else
	DIR_NAME=$(basename "$GIT_URL" .git)
fi

TMP_DIR=$(mktemp -d -t git-clone-bare-XXXXXX)

trap 'echo "Cleaning up temporary directory..."; rm -rf "$TMP_DIR"' EXIT

echo "Setting up grove '$DIR_NAME'..."
git clone "$@" "$GIT_URL" "$TMP_DIR"
git --git-dir="$TMP_DIR/.git" config --bool core.bare true

echo "Clone successful, moving into place..."
mkdir "$DIR_NAME"
mv "$TMP_DIR/.git" "$DIR_NAME/.git"

echo "Sucesss!"
