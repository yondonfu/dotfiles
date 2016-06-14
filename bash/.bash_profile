export PATH=/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin:$PATH

alias emacs="/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs -nw"

alias current-branch='git rev-parse --abbrev-ref HEAD'
alias make-pr='git push -u origin $(current-branch)'
