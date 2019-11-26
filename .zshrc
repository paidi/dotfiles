# history
SAVEHIST=100000

# Load Antigen
source /usr/local/share/antigen/antigen.zsh

# Load Antigen configurations
antigen init .antigenrc


# Tell antigen that you're done.
#antigen apply

###
#################################################################################################

# bind UP and DOWN arrow keys for history search
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

export PURE_GIT_UNTRACKED_DIRTY=0

# Automatically list directory contents on `cd`.
auto-ls () {
        emulate -L zsh;
        # explicit sexy ls'ing as aliases arent honored in here.
        hash gls >/dev/null 2>&1 && CLICOLOR_FORCE=1 gls -aFh --color --group-directories-first || ls
}
chpwd_functions=( auto-ls $chpwd_functions )


# Enable autosuggestions automatically
#zle-line-init() {
#    zle autosuggest-start
#}

#zle -N zle-line-init


# history mgmt
# http://www.refining-linux.org/archives/49/ZSH-Gem-15-Shared-history/
setopt inc_append_history
setopt share_history


zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'


# uncomment to finish profiling
# zprof



# Load default dotfiles
source ~/.bash_profile


export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
