#
# ZSH
#

bindkey -v

zstyle :compinstall filename '/home/ozerova/.zshrc'
autoload -Uz compinit
compinit

#
# Exports
#

export LANG=en_US.UTF-8
export XDG_CONFIG_HOME=~/.config
export COLORTERM=truecolor
export EDITOR=nvim

# Paths
PATH=$PATH:~/.local/bin 
PATH=$PATH:~/builds/ols
PATH=$PATH:~/builds/umka-lang/umka_linux
PATH=$PATH:~/.cargo/bin

#
# Loads
#

source ~/.alias
source ~/.procedures

#
# Configure Tooling
#

eval "$(starship init zsh)"
eval "$(atuin init zsh --disable-up-arrow)"
eval "$(zoxide init zsh)"
eval "$(direnv hook zsh)"
eval "$(keychain --eval ./.ssh/github.alexei-ozerov --eval ./.ssh/gitlab.ozerova)"
