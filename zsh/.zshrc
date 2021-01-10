# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/insanya/.oh-my-zsh"

# Theme
ZSH_THEME="robbyrussell"

# Plugins
plugins=(git nvm npm)

source $ZSH/oh-my-zsh.sh

# User configuration

alias l='ls --group-directories-first -hlX'
alias ll='ls --group-directories-first -hlAX'

# MonetDB and .local/bin
export MONETDB="$HOME/Desktop/local-bin/MonetDB"
export PATH=$MONETDB/bin:$HOME/.local/bin:$PATH
