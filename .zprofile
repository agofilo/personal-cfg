
export EDITOR=emacs

export CLICOLOR=1

PROMPT=%~'>'

PATH=/Users/luke/bin:$PATH

source ~/relevance/etc/bash/git_autocompletion.sh
source ~/relevance/etc/bash/git_prompt.sh

PROMPT=$PS1

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

