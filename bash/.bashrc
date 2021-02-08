#!/usr/bin/env bash

# If not running interactively, don't do anything
case $- in
  *i*) ;;
    *) return;;
esac

# Path to the bash it configuration
export BASH_IT="/home/insanya/.bash_it"

# Theme
export BASH_IT_THEME='minimal'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true

# Load Bash It
source "$BASH_IT"/bash_it.sh

# Personal config
alias l='ls --group-directories-first -hlX'
alias ll='ls --group-directories-first -hlAX'
alias em="emacsclient -c &"
alias volu="amixer -q set Master 10%+"
alias vold="amixer -q set Master 10%-"

# Node Version Manager
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# MonetDB Bins
export MONETDB="/home/insanya/Desktop/local-installs/monetdb"
export PATH=$MONETDB/bin:$PATH

# Rust Cargo Env
source "$HOME/.cargo/env"
