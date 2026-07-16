#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

DOTFILES_DIR="$(pwd)"

link() {
    local src="$1" dst="$2"
    if [ -e "$dst" ] || [ -L "$dst" ]; then
        if [ "$(readlink "$dst" 2>/dev/null)" = "$src" ]; then
            echo "  [=] $dst already linked"
            return
        fi
        echo "  [!] $dst exists, backing up to $dst.bak"
        mv "$dst" "$dst.bak"
    fi
    ln -s "$src" "$dst"
    echo "  [+] $dst -> $src"
}

while IFS= read -r -d '' entry; do
    name="$(basename "$entry")"
    link "$entry" "$HOME/$name"
done < <(find "$DOTFILES_DIR/user" -mindepth 1 -maxdepth 1 -name '.*' -print0)
