;; -*- encoding: utf-8 -*------------------------------------------------
;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: init.el
;; Author: Fred Qi
;; Created: 2005-02-01 16:10:07(+0800)
;; 
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;; 2006-03-23 23:06:51(+0800)    Fred Qi@mobile  
;;    Added the local settings for laptop, lab, and dorm of Windows NT.
;; ----------------------------------------------------------------------
;; Last-Updated: 2024-10-24 19:47:21(+0800) [by Fred Qi]
;;     Update #: 638
;; ----------------------------------------------------------------------

;;; Code:
(defconst winnt-p
  (eq system-type 'windows-nt)
  "Are we running on a Microsoft Windows system?")

(defconst linux-p
  (or (eq system-type 'gnu/linux)
	  (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst darwin-p
  (eq system-type 'darwin)
  "Are we running on a Microsoft Windows system?")

(defconst local-user-name "Fred Qi")
(defconst local-user-email "fred dot qi at ieee dot org")

(defconst fred-chn-sentence-end
  "\\([，。；\\|[.!?][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
  "Teach emacs the chinese sentence ends.")

;; ----------------------------------------------------------------------
;; Custom system/machin related environment variables
;; ----------------------------------------------------------------------

(server-start)
(add-hook 'kill-emacs-hook
	  (lambda()
	    (if (file-exists-p "~/.emacs.d/server/server")
                (delete-file "~/.emacs.d/server/server"))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(defconst local-ispell-pathexe 
  (cond 
   (winnt-p "C:/emacs/ispell/ispell.exe")
   ((or linux-p darwin-p) "aspell"))
  "The executable file name with path of ISpell.")

(defconst local-ispell-dict
  (cond
   (winnt-p "C:/emacs/ispell/English/american.hash")
   (darwin-p "en_US"))
  "The dictionary used by ispell.")

;; (defconst local-matlab-root 
;;   (cond
;;    (winnt-p  "C:/MATLAB")
;;    (linux-p  "/usr/local/matlab"))
;;   "The path of the MATLAB lisp package.")

;; (defconst local-asymptote-path 
;;   (cond
;;    (winnt-p		"C:/LaTeX/Asymptote/"))
;;   "The path of the asymptote program.")

;; (defconst local-tex-dirs
;;   (cond
;;    (winnt-p '("C:/LaTeX/MiKTeX27")))
;;   "The directories containing tex macros.")

;; (defconst local-inno-path
;;   (cond
;;    (winnt-p "C:/Develop/Inno/"))
;;   "The directory inno setup installed to.")

(defconst local-python-path
  (cond
   (winnt-p "C:/Python25/python.exe"))
  "The directory python installed to.")

(defconst local-screen-height 60
  "Number of rows of the Emacs frame window.")
(defconst local-screen-width 120
  "Number of column of the Emacs frame window.")

;; (defconst local-encoding
;;   (cond
;;    (winnt-p "Chinese-GB18030")
;;    (linux-p "UTF-8")
;;    (darwin-p "UTF-8"))
;;   "Character encoding according to the system running on.")

;; (defconst local-font-eng
;;   (if linux-p
;; 	  "-*-Bitstream Vera Sans Mono-medium-r-*-*-28-*-*-*-*-*-fontset-gbk"
;; 	"-outline-Consolas-normal-r-normal-normal-20-*-120-120-c-*-fontset-gbk")
;;   "The string specifying the font used for displaying English characters.")

;; (defconst local-font-eng-bold
;;   (if linux-p
;; 	  "-*-Bitstream Vera Sans Mono-bold-r-*-*-28-*-*-*-*-*-fontset-gbkbold"
;; 	"-outline-Consolas-bold-r-normal-normal-20-*-120-120-c-*-fontset-gbkbold")
;;   "The string specifying the font used for displaying English characters.")

;; (defconst local-font-eng-italic
;;   (if linux-p
;; 	  "-*-Bitstream Vera Sans Mono-medium-o-*-*-28-*-*-*-*-*-fontset-gbkitalic"
;; 	"-outline-Consolas-normal-i-normal-normal-20-*-120-120-c-*-fontset-gbkitalic")
;;   "The string specifying the font used for displaying English characters.")

;; (defconst local-font-eng-bold-italic
;;   (if linux-p
;; 	  "-*-Bitstream Vera Sans Mono-bold-o-*-*-28-*-*-*-*-*-fontset-gbkboldit"
;; 	"-outline-Consolas-bold-i-normal-normal-20-*-120-120-c-*-fontset-gbkboldit")
;;   "The string specifying the font used for displaying English characters.")

;; (defconst local-font-chn
;;   (if linux-p
;; 	  "-*-Noto Sans CJK SC-medium-r-normal-*-28-*-*-*-c-*-iso10646-1"
;; 	"-outline-Noto Serif CJK SC-normal-r-normal-*-20-*-120-120-c-*-iso10646-1")
;;   "The string specifying the font used for displaying Chinese simplified characters.")

(setq safe-local-variable-values
      (append '((TeX-master . t)
		(encoding . us-ascii)
		(encoding . us-ascii-unix)
		(encoding . us-ascii-dos)
		(encoding . us-ascii-mac)
		(encoding . utf-8)
		(encoding . utf-8-unix)
		(encoding . utf-8-dos)
		(encoding . utf-8-mac)
		(encoding . chinese-gbk)
		(encoding . chinese-gbk-unix)
		(encoding . chinese-gbk-dos)
		(encoding . chinese-gbk-mac)
		(default-fill-column . 68 ))
	      safe-local-variable-values ))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (add-to-list 'default-frame-alist '(height . local-screen-height))
;; (add-to-list 'default-frame-alist '(width . local-screen-width))
(add-to-list 'initial-frame-alist '(height . local-screen-height))
(add-to-list 'initial-frame-alist '(width . local-screen-width))

(load "install.el" nil t t)     ;; install packages
(load "emacsinit.el" nil t t)	;; emacs miscellaneous settings
(load "texinit.el" nil t t)	;; tex related settings
(load "cppinit.el" nil t t)	;; C/C++ related settings
(load "pyinit.el" nil t t)	;; Python related settings
;; (load "mathinit.el" nil t t)    ;; Math software related settings
(load "orginit.el" nil t t)     ;; Org mode settings
(load "feedinit.el" nil t t)
;; (load "museinit.el" nil t t)	;; load muse packages

;; ----------------------------------------------------------------------
;; SOME GOOD LINKS FOR EMACS SOURCES
;; ----------------------------------------------------------------------
;;
;; http://relativity.yi.org/Emacs/index.shtml
;; http://tiny-tools.sourceforge.net/
;; http://www.anc.ed.ac.uk/~stephen/emacs/ell.html
;; http://www.dotemacs.de/
;; http://www.gnusoftware.com/WebRing/
;; http://www.splode.com/users/friedman/software/emacs-lisp/
;;
;; Reference:
;; https://medium.com/helpshift-engineering/configuring-emacs-from-scratch-use-package-c30382297877
;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
