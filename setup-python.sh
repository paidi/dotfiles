#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
source ./lib.sh

if ! command -v uv >/dev/null 2>&1; then
    echo "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
    export PATH="$HOME/.local/bin:$PATH"
fi

echo "Installing Python interpreters from packages/python-versions.txt..."
first=1
while IFS= read -r version <&3; do
    if [ "$first" -eq 1 ]; then
        uv python install --default "$version"
        first=0
    else
        uv python install "$version"
    fi
done 3< <(read_package_list packages/python-versions.txt)

echo "Installing Python CLI tools from packages/uv-tools.txt..."
while IFS= read -r tool <&3; do
    # shellcheck disable=SC2086
    uv tool install $tool
done 3< <(read_package_list packages/uv-tools.txt)
