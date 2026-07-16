# .zshrc -- interactive shell config: history, completion, keybindings and
# prompt/tooling integration.

# Ensure we start in the home directory
cd "$HOME" || true

# .zprofile only runs for login shells. Not every interactive zsh is one
# (tmux panes, `zsh` run from another shell, ...), so source it here too
# to guarantee Homebrew/uv end up on PATH everywhere. Safe to run twice.
[ -f ~/.zprofile ] && source ~/.zprofile

[ -f ~/.exports ] && source ~/.exports

### History ###
HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000
SAVEHIST=100000
setopt inc_append_history   # write history as commands run, not on shell exit
setopt share_history        # share history across concurrent sessions
setopt hist_ignore_dups
setopt hist_ignore_space     # don't record lines starting with a space

### Completion ###
autoload -Uz compinit
compinit
autoload -Uz bashcompinit
bashcompinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

### Emacs-style navigation ###
bindkey -e

# Treat path segments as word boundaries, so M-f/M-b/M-d stop at "/"
WORDCHARS=${WORDCHARS//[\/]}

# Arrow keys: substring history search (needs zsh-history-substring-search,
# sourced below)
zmodload zsh/terminfo
if [ -n "$terminfo[kcuu1]" ]; then
    bindkey "$terminfo[kcuu1]" history-substring-search-up
    bindkey "$terminfo[kcud1]" history-substring-search-down
fi

# Home/End
[ -n "$terminfo[khome]" ] && bindkey "$terminfo[khome]" beginning-of-line
[ -n "$terminfo[kend]" ] && bindkey "$terminfo[kend]" end-of-line

# Option/Alt + Left/Right: move by word (Terminal.app / iTerm2 send these
# escape sequences for option-arrow when "use Option as Meta" is off)
bindkey '^[[1;3D' backward-word
bindkey '^[[1;3C' forward-word

### Automatically list directory contents on `cd` ###
auto-ls() {
    emulate -L zsh
    if command -v eza >/dev/null 2>&1; then
        eza -a --group-directories-first
    else
        ls -A
    fi
}
chpwd_functions=(auto-ls $chpwd_functions)

### Tool integrations (all optional; skipped if not installed) ###
if command -v brew >/dev/null 2>&1; then
    BREW_PREFIX="$(brew --prefix)"
    source "$BREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh" 2>/dev/null
    source "$BREW_PREFIX/share/zsh-history-substring-search/zsh-history-substring-search.zsh" 2>/dev/null
    # zsh-syntax-highlighting must be sourced last of the zsh plugins
    source "$BREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" 2>/dev/null
fi

command -v zoxide >/dev/null 2>&1 && eval "$(zoxide init zsh)"
command -v fzf >/dev/null 2>&1 && eval "$(fzf --zsh)"
command -v starship >/dev/null 2>&1 && eval "$(starship init zsh)"

# uv: Python version/venv manager - shell completion
command -v uv >/dev/null 2>&1 && eval "$(uv generate-shell-completion zsh)"
command -v uvx >/dev/null 2>&1 && eval "$(uvx --generate-shell-completion zsh)"

# generic colouriser for gcc/ping/etc, if installed
if [ "$TERM" != dumb ] && command -v grc >/dev/null 2>&1; then
    alias colourify="grc -es --colour=auto"
    alias configure='colourify ./configure'
    for app in gcc g++ ping traceroute; do
        alias "$app"="colourify $app"
    done
fi

### Modern CLI tool aliases ###
# Note: fd/rg/ag are left un-aliased over find/grep - their flags aren't
# compatible, so aliasing over the originals just breaks muscle memory.
if command -v eza >/dev/null 2>&1; then
    alias ls='eza --group-directories-first'
    alias ll='eza -alh --group-directories-first'
    alias lstree='eza --tree'
fi
command -v bat >/dev/null 2>&1 && alias cat='bat --paging=never'
