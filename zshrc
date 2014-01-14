# -*- mode: sh; -*-
ZSH=$HOME/.oh-my-zsh
DISABLE_AUTO_UPDATE="true"
DISABLE_CORRECTION="true"

ZSH_THEME="space"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=/usr/local/bin:$HOME/.cask/bin:$HOME/.cabal/bin:$PATH
[ -f $HOME/.zshrc.local ] && source $HOME/.zshrc.local
