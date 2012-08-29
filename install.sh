#!/usr/bin/env bash

set -e

cd `dirname $0`
export CONFIGDIR=`pwd`

source $CONFIGDIR/install_functions.sh

update_submodules

link_with_backup .zsh
link_with_backup .zshrc
link_with_backup .zprofile

link_with_backup .gitconfig
link_with_backup .rvmrc
link_with_backup .tmux.conf

link_with_backup .emacs.d
build_latest_orgmode
link_with_backup .emacs
link_with_backup .emacs-custom.el

install_relevance_etc
