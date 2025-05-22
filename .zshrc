#
# ZSH
#

#bindkey -v

zstyle :compinstall filename '/home/ozerova/.zshrc'
autoload -Uz compinit
compinit

#
# Exports
#

export LANG=en_US.UTF-8
export XDG_CONFIG_HOME=~/.config
export COLORTERM=truecolor

# Paths
PATH=$PATH:~/.local/bin 
PATH=$PATH:~/builds/ols
PATH=$PATH:~/.cargo/bin

#
# Execution
#

source .alias

#
# Tmux Autoload
#

# if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#   exec tmux -u
# fi

#
# Configure Tooling
#

eval "$(starship init zsh)"
eval "$(atuin init zsh --disable-up-arrow)"
eval "$(zoxide init zsh)"
eval "$(direnv hook zsh)"
eval "$(keychain --agents ssh --eval ./.ssh/github.alexei-ozerov)"

# Set Theme
wal -i downloads/wal.jpg
