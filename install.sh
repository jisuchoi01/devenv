#!/bin/bash
function copy_config() {
    cp ./.vimrc ~/.vimrc
    cp ./.emacs ~/.emacs
    cp ./.bashrc ~/.bashrc
    cp ./.gitconfig ~/.gitconfig
    cp ./.gitattributes ~/.gitattributes
    cp -arp ./.git-templates ~/
}

function setting_git() {
   git config --global init.templateDir ~/.git-templates
   source /usr/share/git/completion/git-completion.bash
   source /usr/share/git/completion/git-prompt.sh
}

function pgp_key() {
    gpg --import ~/.gnupg/public.pgp
    gpg --import ~/.gnupg/private.pgp

    git config --global user.signKey `gpg --list-key | awk 'FNR == 4 {gsub(/ /,"");print}'`
}

copy_config
setting_git
pgp_key
source ~/.bashrc
