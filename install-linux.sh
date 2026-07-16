#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
source ./lib.sh

echo "Installing base apt packages from packages/apt.txt..."
maybe_sudo apt-get update
while IFS= read -r pkg <&3; do
    maybe_sudo apt-get install -y --no-install-recommends "$pkg"
done 3< <(read_package_list packages/apt.txt)

if ! command -v brew >/dev/null 2>&1; then
    echo "Installing Homebrew (Linuxbrew)..."
    NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
else
    brew update
fi

echo "Installing Homebrew packages from packages/brew.txt..."
# Read the package list on fd 3, not stdin - see the comment in
# install-macos.sh for why.
while IFS= read -r pkg <&3; do
    brew install "$pkg"
done 3< <(read_package_list packages/brew.txt)
