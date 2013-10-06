#!/bin/sh
set -e
SRC=$(dirname $0)
DST=$HOME
LN='echo ln'
if [ "$1" = "-i" ]; then LN='ln -nv'; move_aside=1; mkdir "$SRC/old"; fi
if [ "$1" = "-d" ]; then LN='ln -nv'; fi
if [ "$1" = "-f" ]; then LN='ln -fnv'; fi
install() {
    dest="$DST/.$(basename $1 | sed 's:^\.::')"
    if [ "$move_aside" -a -e "$dest" ]; then
        mv -v "$dest" "$SRC/old"
    fi
    $LN -s "$SRC/$1" "$dest"
}

install emacs.d
install gitconfig
install gitignore
install zshrc
install tmux.conf
$LN -s "$PWD/$SRC/space.zsh-theme" "$HOME/.oh-my-zsh/themes/space.zsh-theme"
