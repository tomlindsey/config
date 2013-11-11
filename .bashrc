#
# ~/.bashrc
# contains aliases to simple functions and common commands
#

#
# common options for built-in commands
alias bye='clear ; logout'       # used to exit shells
alias du='du -kc'                # show total in kb
alias ls='ls -la'                 # list all files
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
# source control
alias co='p4 edit'               # checkout
alias ci='p4 submit'             # checkin
alias di='p4 diff'               # diff
alias up='p4 sync'               # update
alias pull='find . -name ".git" -type d -prune -print -execdir git pull \;'

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
# prompt for remove
rmls() { 
    ls $1;
    echo continue?;
    select yn in "Yes" "No"; do
        case $yn in
            Yes) echo $1 ;; 
            No ) exit ;;
        esac
    done
}


#
# switch to a foundation source project
readonly CODE=~/code
readonly CODE_FS="$CODE"/fs/ny
fscode() {
    cd "$CODE_FS"
    [ -n "$1" ] && cd $1
}

#
# common directories
alias c='cd "$CODE"'
alias fs='fscode'
alias s='cd ~/scratch'
