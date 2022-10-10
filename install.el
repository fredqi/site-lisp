;;; install.el ---
;;
;; Filename: install.el
;; Author: Fred Qi
;; Created: 2022-01-17 23:15:18(+0800)
;;
;; Last-Updated: 2022-10-10 23:39:17(+0800) [by Fred Qi]
;;     Update #: 111
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
      '(("gnu" . "http://elpa.gnu.org/packages/")
	;; ("marmalade" . "http://marmalade-repo.org/packages/")
	;; ("elpy" . "http://jorgenschaefer.github.io/packages/")
	;; ("orgmode" . "http://orgmode.org/elpa/")
	("melpa" . "http://melpa.org/packages/")))

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
	org-pomodoro
	ox-hugo ox-reveal gnuplot
	elfeed))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; install.el ends here
