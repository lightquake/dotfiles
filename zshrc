# -*- mode: sh; -*-
ZSH=$HOME/.oh-my-zsh
DISABLE_AUTO_UPDATE="true"
DISABLE_CORRECTION="true"

ZSH_THEME="space"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=$HOME/.cabal/bin:$PATH
