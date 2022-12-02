;;; install.el ---
;;
;; Filename: install.el
;; Author: Fred Qi
;; Created: 2022-01-17 23:15:18(+0800)
;;
;; Last-Updated: 2022-11-04 13:52:54(+0800) [by Fred Qi]
;;     Update #: 112
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ----------------------------------------------------------------------
;; Setting up the package manager.
;; ----------------------------------------------------------------------
(require 'package)

(package-initialize)
(setq package-archives
      '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	;; ("elpy" . "http://jorgenschaefer.github.io/packages/")
	;; ("orgmode" . "http://orgmode.org/elpa/")
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-selected-packages
      '(use-package
	htmlize
	yasnippet
	flycheck
	company company-tabnine company-reftex
	auctex cdlatex
	docker docker-compose-mode dockerfile-mode
	cmake-mode cmake-ide
	go-mode
	cython-mode django-mode toml-mode conda
	markdown-mode
	csv-mode yaml-mode
	ox-hugo ox-reveal citeproc gnuplot
	elfeed))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; install.el ends here
