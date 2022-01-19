;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: texinit.el
;; Author: Fred Qi
;; Created: 2005-02-01 16:10:07(+0800)
;; 
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;; ----------------------------------------------------------------------
;; Last-Updated: 2022-01-19 18:48:37(+0800) [by Fred Qi]
;;     Update #: 298
;; ----------------------------------------------------------------------
;;; COMMENTARY
;; ----------------------------------------------------------------------
;; Settings for using TeX/LaTeX
;; - AUCTeX for LaTeX and XeLaTeX
;; - RefTeX for BibTeX
;; 
;; ----------------------------------------------------------------------

(defun fred-latex-hook ()
  (use-package cdlatex :ensure t)
  (use-package company-reftex :ensure t)
  (fred-auto-header-hook)
  (TeX-PDF-mode t)
  (TeX-fold-mode)
  (outline-minor-mode)
  (flyspell-mode)
  (turn-on-reftex)
  (turn-on-visual-line-mode)
  (LaTeX-math-mode)
  (turn-on-cdlatex)
  (TeX-source-correlate-mode)
  (make-local-variable 'company-backends)
  (setq company-backends
	'((company-capf
	   company-tabnine
	   company-reftex-labels
           company-reftex-citations
           company-yasnippet))))

;; Enable LaTeX
(use-package tex-site
  :ensure auctex
  :mode
  ("\\.tex\\'" . LaTeX-mode)
  ("\\.ltx\\'" . LaTeX-mode)
  :config
  (setq reftex-plug-into-AUCTeX t
	TeX-parse-self t
	TeX-auto-save t
	TeX-master t
	TeX-electric-sub-and-superscript t
	TeX-engine `xetex
	bib-cite-use-reftex-view-crossref t)
  (dolist (hook '(latex-mode-hook LaTeX-mode-hook))
    (add-hook hook #'fred-latex-hook)))

(autoload 'bst-mode "bst-mode" "BibTeX-style major mode." t)
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
(add-hook 'asy-mode-hook
	  (lambda ()
	    (c-set-style "standard")))

;; ;; preview-latex settings
;; (setq preview-image-type 'pnm)
;; (setq preview-auto-cache-preamble t)
;; ;;(setq preview-transparent-color "blue")

;; ;; Biblatex command
;; (setq reftex-bibliography-commands
;; 	  '("bibliography" "nobibliography"
;; 		"addbibresource" "addglobalbib" "addsectionbib"))

;; (require 'zotelo)
;; (add-hook 'TeX-mode-hook 'zotelo-minor-mode)

;; SyncTeX support with Evince
(use-package dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

;; MetaPost mode settings
(defun fred-metapost-hook()
  (fred-auto-header-hook)
  (set (make-local-variable 'compile-command)
	   (concat "mpost -quiet -aux-directory="
			   (file-name-directory buffer-file-name) " "
			   (file-name-nondirectory buffer-file-name)))
  (local-set-key (kbd "C-c C-c") 'compile))

(add-hook 'metapost-mode-hook 'fred-metapost-hook)

(defun sds-word-count (start end)
  "Count lines/words/characters from START to END.
   Replacement for count-lines-region."
  (interactive "r")
  (let ((ost (syntax-table)) (nst (copy-syntax-table)))
    (modify-syntax-entry ?_ "w" nst)
    (modify-syntax-entry ?- "w" nst)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (min start end))
	(unwind-protect
	     (progn (set-syntax-table nst)
		    (message "Region (%d to %d) has: lines: %d; \
                              words: %d; characters: %d."
			     start end (count-lines start end)
			     (string-to-number (how-many "\\<"))
			     (- end start)))
	  (set-syntax-table ost))))))
(define-key esc-map "=" 'sds-word-count) ; was count-line-region

(setq asy-command-location local-asymptote-path)
(add-hook 'asy-mode-hook 'fred-auto-header-hook)

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
