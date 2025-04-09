#
# ZSH
#

bindkey -v

zstyle :compinstall filename '/home/ozerova/.zshrc'

autoload -Uz compinit
compinit

#
# Locale
#

export LANG=en_US.UTF-8

#
# Execution
#

cd
source .alias
source .wsl2

#
# Tmux Autoload
#

if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux -u
fi

#
# Configure Tooling
#

eval "$(starship init zsh)"
eval "$(atuin init zsh --disable-up-arrow)"
eval "$(zoxide init zsh)"
