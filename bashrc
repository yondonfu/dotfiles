[ -f ~/.fzf.bash ] && source ~/.fzf.bash

if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi

function start_emacs ()
{
  # Checks if there is a frame open
  emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" 2> /dev/null | grep t &> /dev/null

  if [ "$?" -eq "1" ]; then
     emacsclient -a '' -nqc "$@" &> /dev/null
  else
      emacsclient -nq "$@" &> /dev/null
  fi
}

export PATH=/Applications/Sublime\ Text\ 2.app/Contents/SharedSupport/bin:$PATH
export PATH=/Library/TeX/texbin:$PATH

alias em=start_emacs
alias current-branch='git rev-parse --abbrev-ref HEAD'
alias make-pr='git push -u origin $(current-branch)'
alias fpush='git push --force origin $(current-branch)'

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
