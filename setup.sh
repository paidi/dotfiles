#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

case "$(uname -s)" in
    Darwin) ./install-macos.sh ;;
    Linux)  ./install-linux.sh ;;
    *) echo "Unsupported OS: $(uname -s)" >&2; exit 1 ;;
esac

./setup-symlinks.sh
./setup-python.sh

echo
echo "Done. Start a new shell (or 'exec zsh') to pick up the new dotfiles."
