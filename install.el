;;; install.el ---
;;
;; Filename: install.el
;; Author: Fred Qi
;; Created: 2022-01-17 23:15:18(+0800)
;;
;; Last-Updated: 2023-06-07 13:40:17(+0800) [by Fred Qi]
;;     Update #: 120
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
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
      ;; ("marmalade" . "http://marmalade-repo.org/packages/")
      ;; ("elpy" . "http://jorgenschaefer.github.io/packages/")
      ;; ("orgmode" . "http://orgmode.org/elpa/"))
      )

(setq package-selected-packages
      '(use-package
	cnfonts
	htmlize
	yasnippet
	flycheck flycheck-grammarly
	company company-tabnine company-reftex
	auctex cdlatex
	docker docker-compose-mode dockerfile-mode
	cmake-mode cmake-ide
	go-mode
	cython-mode django-mode toml-mode anaconda-mode numpydoc sphinx-doc
	markdown-mode
	csv-mode yaml-mode
	ox-hugo ox-reveal citeproc gnuplot
	elfeed elfeed-score))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; install.el ends here
