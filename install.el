;;; install.el ---
;;
;; Filename: install.el
;; Author: Fred Qi
;; Created: 2022-01-17 23:15:18(+0800)
;;
;; Last-Updated: 2022-01-19 14:11:03(+0800) [by Fred Qi]
;;     Update #: 93
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
	company company-tabnine
	;; lsp-mode lsp-treemacs lsp-jedi
	;; which-key
	;; dap-mode
	auctex cdlatex
	docker docker-compose-mode dockerfile-mode
	yaml-mode
	go-mode
	cython-mode
	csv-mode
	markdown-mode
	django-mode
	matlab-mode
	julia-mode
	android-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; install.el ends here
