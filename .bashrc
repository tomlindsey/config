#
# ~/.bashrc
# contains aliases to simple functions and common commands
#

#
# common options for built-in commands
alias bye='clear ; logout'       # used to exit shells
alias du='du -hkc'               # show total in kb
alias ls='ls -la'                # list all files
alias lss='ls -lS'               # list all files by size DESC
alias lst='ls -lt'               # list all files by time DESC
alias mkdir='mkdir -p'           # make the full path
alias ping='ping -c 10'          # no sense pinging forever
alias ps='ps -axf'               # process list
alias rr='fc -s'                 # repeat last command pattern:  rr ch
alias 644='chmod 644'            # owner +rw, others +r
alias 755='chmod 755'            # owner +rwe, others, +re
#
# other stuff
alias e='emacs'                  # execute emacs
#
# bash history
alias hist='history | grep $1'   # show commands by regexp

#
# simple functions
#

#
# cdls dir
# change directory, list contents
cdls() { cd ${1}; echo $PWD; ls; }
alias cd='cdls'                  # after changing directories list contents
alias cdd='cdls ~/scratch'       # after changing directories list contents
alias cd..='cd ..'               # common typo for cd ..

#
# common directories
alias c='cd "$CODE"'
alias s='cd ~/scratch'
