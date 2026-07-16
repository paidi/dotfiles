# Runs once for login shells, before .zshrc. Keep this to PATH/env setup
# that every shell (including non-interactive ones) should inherit.
# .zshrc also sources this file directly, since not every interactive
# shell is a login shell (tmux panes, etc.) - guard against running twice.
[ -n "$DOTFILES_ZPROFILE_LOADED" ] && return
export DOTFILES_ZPROFILE_LOADED=1

# Homebrew (works for /opt/homebrew, /usr/local and Linuxbrew alike)
if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -x /usr/local/bin/brew ]; then
    eval "$(/usr/local/bin/brew shellenv)"
elif [ -x /home/linuxbrew/.linuxbrew/bin/brew ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# uv-managed tools (`uv tool install`) and the uv/uvx binaries themselves
export PATH="$HOME/.local/bin:$PATH"
