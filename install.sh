#!/bin/bash
function copy_config() {
    cp ./.vimrc ~/.vimrc
    cp ./.emacs ~/.emacs
    cp ./.bashrc ~/.bashrc
    cp ./.gitconfig ~/.gitconfig
    cp -arp ./.git-templates ~/
}

function setting_git() {
   git config --global init.templateDir ~/.git-templates
}

copy_config
setting_git
source ~/.bashrc
