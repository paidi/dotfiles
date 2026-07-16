#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
source ./lib.sh

if ! command -v brew >/dev/null 2>&1; then
    echo "Installing Homebrew..."
    NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    brew update
fi

echo "Installing Homebrew packages from packages/brew.txt..."
# Read the package list on fd 3, not stdin - `brew install` occasionally
# reads stdin itself (e.g. for prompts), and if it shared stdin with this
# loop's `read`, it would silently eat package names off the list.
while IFS= read -r pkg <&3; do
    brew install "$pkg"
done 3< <(read_package_list packages/brew.txt)
