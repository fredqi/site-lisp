#/usr/bin/env bash

sudo apt install -qq -y ttf-bitstream-vera

PKGS="yaml-mode htmlize yasnippet company company-tabnine cython-mode auctex android-mode"

emacs -Q --batch --eval "(progn (require 'package)\
  (setq package--init-file-ensured t)\
  (package-initialize)\
  (setq package-archives\
        '((\"gnu\" . \"https://elpa.gnu.org/packages/\")\
          (\"melpa\" . \"https://melpa.org/packages/\")
          (\"marmalade\" . \"https://marmalade-repo.org/packages/\")))\
  (package-refresh-contents)\
  (dolist (pkg '($PKGS))\
          (package-install pkg)))"

ln -s site-lisp/init.el .
