#!/bin/bash

# Set up symbolic links
DOTFILESPATH="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
DOTFILES=(vimrc spacemacs bash_profile bashrc gitconfig)

for dotfile in "${DOTFILES[@]}" ; do
  if [ -L ~/.$dotfile ] ; then
    rm -fv ~/.$dotfile
  elif [ -e ~/.$dotfile ] ; then
    mv -fv ~/.$dotfile ~/.$dotfile.bak
  fi

  ln -s $DOTFILESPATH/$dotfile ~/.$dotfile
done
