;;; install.el ---
;;
;; Filename: install.el
;; Author: Fred Qi
;; Created: 2022-01-17 23:15:18(+0800)
;;
;; Last-Updated: 2022-01-18 00:24:00(+0800) [by Fred Qi]
;;     Update #: 83
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

(require 'package)
(setq pkgs '(htmlize
	     yasnippet
	     company company-tabnine
	     auctex cdlatex
	     docker docker-compose-mode dockerfile-mode
	     yaml-mode
	     cython-mode
	     csv-mode
	     markdown-mode
	     django-mode
	     matlab-mode
	     julia-mode
	     android-mode))

(defun install-packages (packages)
  (setq package--init-file-ensured t)
  (setq modepkg "%s-mode")
  (package-initialize)
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-refresh-contents)
  (dolist (pkg pkgs t)
    (package-install pkg)))

(install-packages pkgs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; install.el ends here
