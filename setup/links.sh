#!/bin/bash

###################################################################
# Create symbolic links
###################################################################

echo "Linking profile to Linux system"

###################################################################
# Setup .emacs and .emacs.d files
###################################################################
# Get full paths to emacs files
DOT_EMACS=$(readlink -f ../emacs/.emacs)
EMACS_DIR=$(readlink -f ../emacs/.emacs.d)

if [ -f ~/.emacs ];
then
    echo ".emacs exists."
    echo -n "Erase file and create symbolic link? (Y/n):"
    read SURE

    if [[ $SURE = "Y" || $SURE = "y" || $SURE = "" ]]; 
    then
        rm ~/.emacs
    fi
fi

if [ -d ~/.emacs.d ];
then
    echo ".emacs.d exists."
    echo -n "Erase directory and create symbolic link? (Y/n):"
    read SURE

    if [[ $SURE = "Y" || $SURE = "y" || $SURE = "" ]]; 
    then
        rm -rf ~/.emacs.d
    fi
fi

# link the files / directories
ln -s ${DOT_EMACS} ~/.emacs
ln -s ${EMACS_DIR} ~/.emacs.d

###################################################################
# .bashrc installation
###################################################################
if [ -f ~/.bashrc ];
then
    echo ".bashrc exists."
    echo -n "Erase file and create symbolic link? (Y/n):"
    read SURE

    if [[ $SURE = "Y" || $SURE = "y" || $SURE = "" ]]; 
    then
        rm ~/.bashrc
    fi
fi

BASHRC=$(readlink -f ../.bashrc)
ln -s ${BASHRC} ~/.bashrc

##########################################################
# link pianobar config file to ~/.config/pianobar/config
##########################################################
mkdir -p ~/.config/pianobar

PIANOBAR_CONFIG=$(readlink -f ../pianobar/config)
ln -s ${PIANOBAR_CONFIG} ~/.config/pianobar/config
