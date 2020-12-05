export PATH=$HOME/bin:/usr/local/bin:$PATH

export MonetDB_rel=Oct2020-SP1
export MonetDB_ver=11.39.7
export MonetDB_base_dir=$HOME/Desktop/bin/MonetDB-$MonetDB_rel
export MonetDB_source_dir=$MonetDB_base_dir/MonetDB-$MonetDB_ver
export MonetDB_build_dir=$MonetDB_base_dir/BUILD
export MonetDB_prefix_dir=$MonetDB_base_dir

# Path to your oh-my-zsh installation.
export ZSH="/home/insanya/.oh-my-zsh"

ZSH_THEME="robbyrussell"

COMPLETION_WAITING_DOTS="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Personal Aliases
alias l='ls --group-directories-first -hlX'
alias ll='ls --group-directories-first -hlAX'
alias firefox-dev='~/.local/share/umake/web/firefox-dev/firefox'

# NVM Setup
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc

export LD_LIBRARY_PATH=/home/insanya/Desktop/bin/openssl/lib:$LD_LIBRARY_PATH
export PATH=/home/insanya/Desktop/bin/openssl/bin:$PATH
