# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/insanya/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
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
