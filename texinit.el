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
;; Last-Updated: 2021-08-01 11:32:16(+0800) [by Fred Qi]
;;     Update #: 251
;; ----------------------------------------------------------------------
;;; COMMENTARY
;; ----------------------------------------------------------------------
;; Settings for using TeX/LaTeX
;; - AUCTeX for LaTeX and XeLaTeX
;; - RefTeX for BibTeX
;; 
;; ----------------------------------------------------------------------

;; Load the AUCTeX package
(load "auctex.el" nil t t)
(load "preview.el" nil t t)

(require 'tex-site)

(autoload 'bst-mode "bst-mode" "BibTeX-style major mode." t)

(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
(add-hook 'asy-mode-hook
	  (lambda ()
	    (c-set-style "standard")))

;; (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
;; (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

;; AUCTeX Configurations
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq TeX-master t)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-engine `xetex)

;; preview-latex settings
(setq preview-image-type 'pnm)
(setq preview-auto-cache-preamble t)
;;(setq preview-transparent-color "blue")

;; RefTeX custom
(setq reftex-plug-into-AUCTeX t)
(setq bib-cite-use-reftex-view-crossref t)
(setq reftex-bibpath-environment-variables
	  '("~/texmf/bibtex/bib/fred/"))
;; ;; Biblatex command
;; (setq reftex-bibliography-commands
;; 	  '("bibliography" "nobibliography"
;; 		"addbibresource" "addglobalbib" "addsectionbib"))

;; (require 'zotelo)
;; (add-hook 'TeX-mode-hook 'zotelo-minor-mode)

(defun fred-LaTeX-hook()
  (fred-auto-header-hook)
  (turn-on-visual-line-mode)
  (setq TeX-engine `xetex)
  (TeX-PDF-mode)
  (TeX-fold-mode)
  (outline-minor-mode)
  (LaTeX-math-mode)
  (flyspell-mode)
  (turn-on-reftex)
  (TeX-source-correlate-mode))

;; load minor modes when opening tex files
(add-hook 'LaTeX-mode-hook 'fred-LaTeX-hook)

;; SyncTeX support with Evince
(require 'dbus)

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

;;setup key-map
(global-set-key [f10]	'tex-source-specials-mode)
(global-set-key [f11]	'latex-math-mode)

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
