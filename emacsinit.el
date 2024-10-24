;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: emacsinit.el
;; Author: Fred Qi
;; Created: 2005-02-01 16:10:07(+0800)
;; 
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;; 2006-03-23 23:07:51(+0800)    Fred Qi@mobile
;;    Move the ess package to mathinit.el.
;;    Setting user-full-name with respect to machine and OS platform.
;; ----------------------------------------------------------------------
;; Last-Updated: 2023-08-05 18:52:24(+0800) [by Fred Qi]
;;     Update #: 1112
;; ----------------------------------------------------------------------
;;
;;
;;
;; REQUIRED FILES / PACKAGES NOT INCLUDED WITH GNU EMACS
;; ----------------------------------------------------------------------
;;
;; FILE             SOURCE
;; color-theme.el   http://www.emacswiki.org/cgi-bin/wiki?ColorTheme
;; htmlize.el       http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
;; header2.el       http://www.emacswiki.org/cgi-bin/wiki/header2.el
;; psvn.el          http://www.xsteve.at/prg/emacs/psvn.el
;; psgml            http://www.lysator.liu.se/projects/about_psgml.html
;; doscmd-mode.el   local, mode for windows/dos console batch files
;; 
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Load required packages 
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Setting up Chinese language / GBK enviroment
;; ----------------------------------------------------------------------

;;; Code:
(require 'cnfonts)
(setq cnfonts-profiles '("fred"))
(setq cnfonts-default-fontsize 20.0)
(setq cnfonts-use-face-font-rescale t)
(cnfonts-mode 1)

;; (if (>= emacs-major-version 23)
;;     (progn 
;;       (if (or linux-p darwin-p)
;; 		  (set-language-environment 'utf-8))
;; 	  (if winnt-p
;; 		  (progn
;; 			(set-language-environment 'chinese-gbk)
;; 			(setq w32-charset-info-alist
;; 				  (cons '("gbk" w32-charset-gb2312 . 936) 
;; 						w32-charset-info-alist))))
;;       ;; create fontsets from specification strings
;;       (create-fontset-from-fontset-spec local-font-eng)
;;       (create-fontset-from-fontset-spec local-font-eng-bold)
;;       (create-fontset-from-fontset-spec local-font-eng-italic)
;;       (create-fontset-from-fontset-spec local-font-eng-bold-italic)
;;       ;; set fonts in corresponding fontsets
;;       (set-fontset-font "fontset-default" nil local-font-chn nil 'prepend)
;;       (set-fontset-font "fontset-gbk" 'kana local-font-chn nil 'prepend)
;;       (set-fontset-font "fontset-gbk" 'han local-font-chn nil 'prepend)
;;       (set-fontset-font "fontset-gbk" 'cjk-misc local-font-chn nil 'prepend)
;;       (set-fontset-font "fontset-gbk" 'symbol local-font-chn nil 'prepend)
;;       ;; select fontsets for displaying specific face/style font
;;       (set-frame-font "fontset-gbk")
;; 	  (setq default-frame-alist
;; 	  		(append '((font . "fontset-gbk")) default-frame-alist))
;; 	  (set-face-font 'bold "fontset-gbkbold")
;; 	  (set-face-font 'italic "fontset-gbkitalic")
;; 	  (set-face-font 'bold-italic "fontset-gbkboldit")))

;; (if linux-p
;; 	(progn
;; 	  (register-input-method "chinese-wubi"
;; 							 local-encoding
;; 							 'quail-use-package
;; 							 "WuBi" "WuBi" "wubi")
;; 	  (setq default-input-method "chinese-wubi")))

;; included in GNU EMACS(v23)
(require 'ido)          ; Interactively do things with buffers and files.
(require 'ibuffer)      ; Operate on buffers like dired.
; Rename files editing their names in dired buffers.
(autoload 'wdired-change-to-wdired-mode "wdired")

(require 'htmlize)      ; Convert buffer text and decorations to HTML.
(require 'header2)      ; Support for creation and update of file headers.

(require 'use-package)

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)
  ;; Number candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  (global-company-mode t))

(use-package company-tabnine :ensure t)

;; Company configurations
(add-to-list 'company-backends #'company-tabnine)

;; yasnippet configurations
(use-package yasnippet
  :defer 3		       ; takes a while to load, so do it async
  :demand t
  :mode ("\\.yasnippet\\'" . snippet-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode)
  :custom
  (yas-prompt-functions '(yas-completing-prompt)))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package toml-mode)

;; ----------------------------------------------------------------------
;; Custom the basic functionalities of EMACS
;; ----------------------------------------------------------------------
(setq display-time-24hr-format t)		; set time format shown in mode-line
(setq frame-title-format "%n%b")		; show buffer name in the title bar
(setq inhibit-splash-screen t)			; no startup message
(setq column-number-mode t)				; display column number
(setq line-number-mode t)				; display line number 
(setq visible-bell t)					; flash the frame to represent a bell
(setq ring-bell-function				; inihibit the motherboard ring bell
      (lambda ()  t))
(if (fboundp 'menu-bar-mode)
	(menu-bar-mode -1))					; hide the menu
(if (fboundp 'tool-bar-mode)			
	(tool-bar-mode -1))					; hide the tool bar
(if (fboundp 'scroll-bar-mode)
	(scroll-bar-mode -1))				; hide the scroll bar

(setq color-theme-libraries 
	  '("color-theme-library.el"))
(require 'color-theme)
(color-theme-initialize)				; load the color theme

(defvar fred-theme-list
  '((color-theme-dark-laptop)
	(color-theme-arjen)
	(color-theme-sitaramv-nt)
	(color-theme-jb-simple)
	(color-theme-billw)))

(funcall
 (car (nth 
       (mod (random t) 
			(length fred-theme-list))
       fred-theme-list)))

;; Backup policies 
(setq make-backup-files t) 
(setq version-control t) 
(setq kept-old-versions 10) 
(setq kept-new-versions 10) 
(setq delete-old-versions t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq auto-save-list-file-prefix "~/.emacs.d/autosaves/")

;; Setup the desktop save mode
;; (setq desktop-path '("" "~/.emacs.d"))
(setq desktop-base-file-name ".desktop.el")
(setq desktop-save t)
(desktop-save-mode t)					; save desktop

;; custom edit mode
(display-time)							; enable display of time in mode lines
(auto-save-mode t)						; enable the auto save mode
(show-paren-mode t)						; hilight paired parens
(transient-mark-mode t)					; hilight current marked region
(global-font-lock-mode t)				; syntax hilight
(auto-image-file-mode t)				; show images (png,jpeg,xpm,tiff)
(mouse-avoidance-mode 'animate)			; move mouse cursor when editing to

;; (setq abbrev-mode t)                    ; turn on abbrev-mode
;; (setq save-abbrevs nil)
(setq auto-fill-mode nil)		; turn on auto-fill-mode
(setq default-fill-column 80)           ; set column width 
(setq default-tab-width 4)              ; default tab width
(setq kill-ring-max 200)
(setq default-major-mode 'text-mode)    ; set text-mode as the default major mode
(setq default-buffer-file-coding-system
	  (cond
	   (linux-p 'utf-8-unix)
	   (darwin-p 'utf-8-mac)
	   (winnt-p 'utf-8-dos)))

(setq tramp-default-method "ssh")		; faster than default scp
;; (setq url-using-proxy t)
;; (setq url-proxy-services  '(("http" . "127.0.0.1:1080")))

;; teach eamcs Chinese punctuations 
(setq sentence-end fred-chn-sentence-end)
(setq sentence-end-double-space nil)

;; set user information
(setq user-full-name local-user-name)
(setq user-mail-address local-user-email)

;; Setup the calendar
(setq calendar-latitude 39.90)      
(setq calendar-longitude 116.30) 
(setq calendar-location-name "Xi'an")
;; (setq calendar-week-start-day 1)		; use default value
(setq calendar-mark-holidays-flag nil)

(if (or linux-p darwin-p)
	(setq x-select-enable-clipboard t))

;; ----------------------------------------------------------------------
;; SETTING LOADED PACKAGES
;; ----------------------------------------------------------------------

;; Settings for ispell
(setq ispell-program-name local-ispell-pathexe)
(if linux-p (ispell-change-dictionary "american" t)
  (setenv "DICTIONARY" local-ispell-dict))

;; Setup ido mode
(ido-mode 'buffer)
(setq ido-enable-flex-matching t)

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(setq vc-handled-backends '(svn git))
(setq svn-status-hide-unknown t)
(setq svn-status-hide-unmodified t)

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/projects"))

(setq header-date-format "%Y-%m-%d %T(%z)")
;; (setq header-copyright-notice "Copyright (C) 2008, all rights reserved.\n")
(setq file-header-update-alist nil)

;; Adding hooks
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'text-mode-hook 'fred-text-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'fred-auto-header-hook)
(add-hook 'doscmd-mode-hook 'fred-auto-header-hook)
(add-hook 'sh-mode-hook 'fred-sh-mode-hook)

(register-file-header-action "Filename[ \t]*:" 'fred-update-file-name)
(register-file-header-action "Last-Updated[ \t]*: " 'fred-update-modified-date)
(register-file-header-action "Update #[ \t]*: " 'fred-update-version-info)

(defun fred-strim-right (str)
  "Remove white spaces in beginning and ending of STRING."
  (replace-regexp-in-string "[ \t\n]*\\'" "" str))

(defsubst fred-header-blank ()
  (insert (fred-strim-right header-prefix-string) "\n"))

(defsubst fred-header-title ()
  (insert (concat comment-start
				  (and (= 1 (length comment-start)) header-prefix-string)
                  (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))
                  " ---" "\n"))
  (setq return-to  (1- (point))))

(defsubst section-comment-start ()
  (if (= (length comment-start) 1)      ; e.g. Lisp: ";; \n;;;"
      (concat (fred-strim-right header-prefix-string)
			  "\n" comment-start header-prefix-string)
    (concat "\n" comment-start)))       ; e.g. C: "\n/*"

(defsubst fred-header-commentary ()
  (insert (concat (section-comment-start) "Commentary:\n")))

(defsubst fred-header-history ()
  (insert (concat (section-comment-start) header-history-label "\n")))

(defun fred-text-mode-hook ()
  "Fred's text mode hook."
  ;; (turn-on-auto-fill)
  (turn-on-visual-line-mode)
  (outline-minor-mode 1))

(defun fred-sh-mode-hook ()
  "Fred's hook for shell script mode."
  (setq comment-start "#")
  (fred-auto-header-hook))

(defun fred-auto-header-hook ()
  "Fred's hook to bind header hooks with specific major mode."
  ;; (make-local-hook 'make-header-hook)
  (make-local-variable 'make-header-hook)
  (setq make-header-hook 
        '(fred-header-title
          fred-header-blank
          header-file-name
          ;; header-status
          header-author
          ;; header-maintainer
          header-copyright
          header-creation-date
          ;; header-sccs
          fred-header-blank
          header-modification-date
          header-update-count
          header-end-line
          fred-header-commentary
		  fred-header-blank
		  fred-header-blank
		  header-end-line
		  fred-header-history
		  fred-header-blank
		  fred-header-blank
		  header-end-line
		  ;; header-code
          header-eof))
  (auto-make-header)
  (add-hook 'write-file-hooks 'auto-update-file-header))

(defun fred-update-file-name ()
  "Update the line that indicates the file name."  
  (delete-and-forget-line)
  (insert " " (file-name-nondirectory (buffer-file-name))))

(defun fred-update-modified-date()
  "Update the line that indicates the last-modified date."
  (delete-and-forget-line)
  (insert (header-date-string)
          " \[by " user-full-name "\]"))

(defun fred-update-version-info()
  "Increment the update number."
  (let ((num)
        (str (delete-and-forget-line)))
    (setq num (car (read-from-string str)))
    (if (numberp num)
        (insert (format "%s" (1+ num)))
      (insert str)
      (error "Invalid number for update count `%s'" str))))

(defun count-words-region (start end) 
  (interactive "r")
  (save-excursion 
    (let ((n 0)) 
      (goto-char start) 
      (while (< (point) end) 
        (if (forward-word 1) 
            (setq n (1+ n)))) 
      (message "Region has %d words" n) 
      n)))

(use-package markdown-mode
  :bind
  (("C-c C-s a" . markdown-table-align))
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md$" . gfm-mode))

;; (pdf-tools-install)  ; Standard activation command
;; (pdf-loader-install) ; On demand loading, leads to faster startup time

;; ----------------------------------------------------------------------
;; MY PREFERRED KEY BINDINGS
;; ----------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c r") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c b") 'make-box-comment)

(global-set-key [f1]   'help)
(global-set-key [f2]   'shell)
(global-set-key [f3]   'query-replace)
(global-set-key [S-f3] 'query-replace-regexp)
(global-set-key [S-f4] 'kill-buffer)
(global-set-key [f5]   'revert-buffer)
(global-set-key [f6]   'outline-show-entry)
(global-set-key [S-f6] 'outline-show-all)
(global-set-key [f7]   'outline-hide-entry)
(global-set-key [S-f7] 'outline-hide-body)
(global-set-key [f9]   'calendar)
(global-set-key [f12]  'auto-fill-mode)

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
