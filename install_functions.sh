
function backup {
    local FILE=$1
    if [ -L $FILE ]; then
        rm $FILE
    elif [ -e $FILE ]; then
        mv $FILE $FILE.bak
    fi
}

function link_with_backup {
    local FILENAME=$1
    local SOURCE=$CONFIGDIR/$FILENAME
    local TARGET=$HOME/$FILENAME
    backup $TARGET
    ln -s $SOURCE $TARGET
}

function update_submodules {
    git submodule init
    git submodule update
}

function install_relevance_etc {
    backup ~/.relevance-etc
    ln -s $CONFIGDIR/submodules/relevance/etc $HOME/.relevance-etc
}

function build_latest_orgmode {
    cd $CONFIGDIR/.emacs.d/local
    if [ ! -d org-mode ]; then
        git clone --depth 1 git@github.com:stuartsierra/org-mode.git
    fi
    cd org-mode 
    make
}
