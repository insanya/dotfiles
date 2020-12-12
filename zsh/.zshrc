# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/insanya/.oh-my-zsh"

# Theme
ZSH_THEME="robbyrussell"

# Plugins
plugins=(git nvm npm emacs)

source $ZSH/oh-my-zsh.sh

# User configuration

alias l='ls --group-directories-first -hlX'
alias ll='ls --group-directories-first -hlAX'

export MonetDB_rel=Oct2020-SP1
export MonetDB_ver=11.39.7
export MonetDB_base_dir=$HOME/Desktop/bin/MonetDB-$MonetDB_rel
export MonetDB_source_dir=$MonetDB_base_dir/MonetDB-$MonetDB_ver
export MonetDB_build_dir=$MonetDB_base_dir/BUILD
export MonetDB_prefix_dir=$MonetDB_base_dir
