[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi

export PATH=/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin:$PATH
export PATH=/Library/TeX/texbin:$PATH

alias current-branch='git rev-parse --abbrev-ref HEAD'
alias make-pr='git push -u origin $(current-branch)'
alias fpush='git push --force origin $(current-branch)'

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
