#
# show git project branch
function parse_git_branch_and_add_brackets {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\ \[\1\]/'
}
PS1="\h\[\033[0;32m\]\$(parse_git_branch_and_add_brackets) \[\033[0m\]\$ "

#
# change to the prompt to show just the host.  the default was '\h:\w \u $'
#export PS1='\h $ '

#
# look for the apps that the local admin has installed
# and where I might place a script or two during the day
#export PATH=/usr/local/git/bin:/usr/local/bin:/Users/me:/Users/me/bin:/opt/local/bin:/opt/local/sbin:$PATH

#
# color anyone?
export CLICOLOR=1
export LSCOLORS=dxexfxcxbxgxegedabagacad

#
# bash history
# - limit history to 500
# - ignore matching history entries
# - don't record some commands
export HISTFILESIZE=500
export HISTCONTROL=ignoreboth
export HISTIGNORE="&:ls:cd:clear:e:emacs:exit"

#
# emacs key bindings in shell
set -o emacs

#
# aliases that I want to use in all shells
# - .bash_profile is executed only for interactive login shells.  the order of
#   execution is as follows: /etc/profile, ~/.bash_profile, ~/.bash_login,
#   ~/.profile
# - .bashrc is executed for interactive non-login shells and remote shell daemon
if [ -f .bashrc ]; then
    source .bashrc
fi

#
# colored su,ssh,rlogin,slogin
# I usually store colorwrap in ~/bin
#if [ "${TERM}" != "dumb" ]; then
#  alias su='colorwrap.sh \su'
#  alias ssh='colorwrap.sh \ssh'
#  alias rlogin='colorwrap.sh \rlogin'
#  alias slogin='colorwrap.sh \slogin'
#fi

#
# git tab complete
source ~/.git-completion.bash

# Setting PATH for Python 3.5
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"
export PATH
