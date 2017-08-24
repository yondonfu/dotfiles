"Set up Vundle
let hasVundle=1
let vundleReadme=expand('~/.vim/bundle/Vundle.vim/README.md')
if !filereadable(vundleReadme)
  echo "Installing Vundle..."
  echo ""
  silent !mkdir -p ~/.vim/bundle
  silent !mkdir -p ~/.vim/backup
  silent !mkdir -p ~/.vim/swp
  silent !git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
  let hasVundle=0
endif

set nocompatible

set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

"Colorschemes
Plugin 'altercation/vim-colors-solarized'

call vundle#end()

if hasVundle == 0
  echo "Installing plugins."
  echo ""
  :PluginInstall
endif

set t_Co=256

filetype plugin indent on
syntax on

syntax enable

let g:solarized_termcolors=256
set background=light
colorscheme solarized

"Backups and swap files in different dir
set backupdir=~/.vim/backup
set directory=~/.vim/swp

"Enable mouse if the terminal emulator supports it
if has('mouse')
  set mouse=a
endif

"Search settings
set incsearch
set hlsearch
set ignorecase

"Editor
set number
set scrolljump=5
set scrolloff=3
set autoindent
set shiftwidth=2
set expandtab
set tabstop=2
set softtabstop=2
set eol
