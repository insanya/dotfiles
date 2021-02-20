

add-apt-repository -y ppa:aslatter/ppa
add-apt-repository -y ppa:kelleyk/emacs

apt update

apt install -y git xmonad libghc-xmonad-contrib-dev xmobar alacritty emacs27 curl llvm clang clangd texlive-base rofi i3lock trayer volumeicon-alsa fonts-font-awesome feg fdpowermon

git clone https://github.com/insanya/dotfiles.git ~/dotfiles
git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash

cargo install --git https://github.com/latex-lsp/texlab.git
nvm install node
