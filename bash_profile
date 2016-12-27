if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

# Setting PATH for Python 3.5
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"
export PATH

# Setting PATH for Go
export GOPATH="$HOME/gocode"
export PATH="$PATH:$GOPATH/bin"
