#!/bin/sh

# ------------------------------------------------------------------------
# colorwrap 
#
# This script wraps a command in a different background and foreground 
# color.  Depending on what the command is, it sets the color, runs the
# command, then sets the color back to what it was before
#
# To display the current colors being used:
#   colorwrap.sh getcolor
#
# Here's an example of how to configure (t)csh to use it:
#   alias ssh '${HOME}/opt/bin/colorwrap.sh \ssh \!*'
#
# Here's how you would use it configure bash:
#   alias ssh='${HOME}/opt/bin/colorwrap.sh \ssh $*'
#
# JD Smith and Eric Kow
#
# This script is released to the public domain.  
# Do whatever you want with it.
# ------------------------------------------------------------------------

# edit this function to change to the commands/servers you want 
select_fg_color() {
  case "${1}" in
    # if the command contains a server name, we set accordingly
    *eagle)        NEW_FG_COLOR='yellow';;
    *wrangler)     NEW_FG_COLOR='orange';;
    *.fs.ut.local) NEW_FG_COLOR='cyan';;
    # You can get a fancy color by using the Apple's color-
    # selection GUI and then doing a colorwrap.sh getcolor 
    *cis.upenn.edu)    NEW_FG_COLOR='{17911, 7822, -1}';; 
    # black on red (see below)
    su)                NEW_FG_COLOR='black';;
    # this is a default value in case we don't specify otherwise
    *)                 NEW_FG_COLOR='green';;
  esac
  echo ${NEW_FG_COLOR}
}

select_bg_color() {
  case "${1}" in
    # su is red to make us alert 
    su)    NEW_BG_COLOR='red';;
    *)     NEW_BG_COLOR='black';;
  esac
  echo ${NEW_BG_COLOR}
}

# ----------------------------------------------------------------------
# command line arguments 
# ----------------------------------------------------------------------

if [ $# -lt 1 ]; then
  echo "usage: $0 cmd [args...]"
  exit 1
fi

COMMAND=$*

# ----------------------------------------------------------------------
# colors
# ----------------------------------------------------------------------

# Set the title bar name to ensure that the correct window
# is modified by the (slow) Applescripts even if you switch 
# terminals
WINDOW_NAME="${COMMAND}_$$"
set_title() {
  echo -n -e "\e]0;${WINDOW_NAME}\a"
}
unset_title() {
  echo -n -e "\e]0;\a"
}

tell_prefix() {
  echo 'tell application "Terminal" to tell'\
       '(first window whose name contains "'${WINDOW_NAME}'")'
}

get_fg_color() {
  TELL=`tell_prefix` 
  echo `osascript -s s -e "${TELL} to get its normal text color"`
}

get_bg_color() {
  TELL=`tell_prefix`
  echo `osascript -s s -e "${TELL} to get its background color"`
}

quote_color() {
  THE_COLOR=$*
  # quote the NEW_COLOR, if neccesary
  case ${THE_COLOR} in 
    \"*\") ;;
    '{'*'}') ;;
    *) THE_COLOR='"'${THE_COLOR}'"'
    ;;
  esac
  echo ${THE_COLOR}
}

set_colors() {
  bg_color=`quote_color ${1}`
  fg_color=`quote_color ${2}`
  TELL=`tell_prefix`
  osascript -s s\
   -e "${TELL} to set its background color to ${bg_color}"\
   -e "${TELL} to set its normal text color to ${fg_color}"
}

# get the original colors
set_title 
OLD_BG_COLOR=`get_bg_color`
OLD_FG_COLOR=`get_fg_color`

# if the user just wants to know what the color is...
if [ "${COMMAND}" == "getcolor" ]; then
  echo "bg: ${OLD_BG_COLOR}"
  echo "fg: ${OLD_FG_COLOR}"
  exit 0
fi

# select the new colors
NEW_BG_COLOR=`select_bg_color "${COMMAND}"`
NEW_FG_COLOR=`select_fg_color "${COMMAND}"`


# ----------------------------------------------------------------------
# running the command 
# ----------------------------------------------------------------------

decolor_err() {
  # reset the color to its old value
  set_title
  set_colors "${OLD_BG_COLOR}" "${OLD_FG_COLOR}"
  unset_title
  exit 1;
}

# we trap these signals in case the following
# quit/kill/abort/errors signals are sent to the command
# prematurely
trap decolor_err 1 2 3 6

# set the color to the requested NEW_COLOR
set_colors "${NEW_BG_COLOR}" "${NEW_FG_COLOR}"

# run the script and save its exit status
${COMMAND}
CMDEXIT=$?

# reset the color to its old value
set_title 
set_colors "${OLD_BG_COLOR}" "${OLD_FG_COLOR}"
unset_title

# exit with the exit status of the command we ran
exit ${CMDEXIT}
