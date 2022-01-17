#/usr/bin/env bash

ln -s site-lisp/init.el .
sudo apt install -qq -y ttf-bitstream-vera
emacs -Q --batch -l site-lisp/install.el
