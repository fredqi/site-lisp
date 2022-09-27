;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: orginit.el
;; Author: Fred Qi
;; Created: 2010-03-02 17:13:39(+0800)
;; 
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;;; References:
;;  - https://blog.sumtypeofway.com/posts/emacs-config.html
;;  - https://jamiecollinson.com/blog/my-emacs-config/
;;  - https://ianyepan.github.io/posts/setting-up-use-package/
;; ----------------------------------------------------------------------
;; Last-Updated: 2022-09-26 19:34:14(+0800) [by Fred Qi]
;;     Update #: 917
;; ----------------------------------------------------------------------

;;; Code:

;; Org-mode settings
(require 'org)
(require 'org-pomodoro)
(require 'ox-beamer)
;; (use-package ox-hugo :ensure t :pin melpa :after ox)
(require 'ox-hugo)

(use-package markdown-mode
  :bind
  (("C-c C-s a" . markdown-table-align))
  :mode
  ("\\.markdown\\'" . markdown-mode)
  ("\\.md\\'" . markdown-mode)
  ("README\\.md$" . gfm-mode))

(eval-after-load "org"
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((python . t)
	 (shell . t)
	 (emacs-lisp . t)
	 (gnuplot . t)
	 (dot . t)))
      (setq org-confirm-babel-evaluate nil)
      (conda-env-activate "base")
      (setq org-babel-python-command "python3")))

(setq org-agenda-custom-commands
      '(("r" "Research tasks"
         ((agenda "") (tags "read") (tags "write") (tags "code") (tags "review")))
	("l" "Life matters"
         ((todo "REFR")))))

;; GTD and agenda
(setq org-log-done 'time)
(setq org-agenda-start-on-weekday 0)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-agenda-dim-blocked-tasks t)	; Dim blocked tasks
(setq org-agenda-compact-blocks t)	; Compact the block agenda view

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)"
		  "|" "DONE(d!/!)")
	(sequence "REFR(Q!)" "DELEG(q!)" "SDAY(S!)"
		  "|" "DONE(d!/!)" "EXPIRED(E@)" "REJECTED(R@)" "CANCELLED(c@/!)")))
;; (sequence "WAIT(w@/!)" "SDAY(S!)"
;; 		"OPEN(O@)" "|" "CANCELLED(c@/!)" "PHONE")
;; (sequence "DELEG(q!)" "|"
;; 		"APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("STARTED" :foreground "cyan" :weight bold)
 	("DONE" :foreground "forest green" :weight bold)
 	;; ("WAIT" :foreground "orange" :weight bold)
	;; ("SDAY" :foreground "magenta" :weight bold)
	;; ("QUOTE" :foreground "red" :weight bold)
	;; ("PHONE" :foreground "forest green" :weight bold)
	;; ("APPROVED" :foreground "forest green" :weight bold)
	("REFR" :foreground "magenta" :weight bold)
	("DELEG" :foreground "cyan" :weight bold)
 	("CANCELLED" :foreground "forest green" :weight bold)
	("EXPIRED" :foreground "forest green" :weight bold)
	("REJECTED" :foreground "forest green" :weight bold)))
 
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAIT" ("WAIT" . t) ("NEXT"))
        ("SDAY" ("WAIT" . t))
        (done ("NEXT") ("WAIT"))
        ("TODO" ("WAIT") ("CANCELLED") ("NEXT"))
        ("STARTED" ("WAIT"))
        ("DONE" ("WAIT") ("CANCELLED") ("NEXT"))))

(setq org-capture-templates
      '(("r" "Reading task" entry
	 (file+olp "research.org" "General Research" "Reading list")
	 "*** TODO %^{Description}%? :read:"
	 :immediate-finish t :jump-to-captured t)
	("w" "Writing task" entry
	 (file+olp "research.org" "General Research" "Writing list")
	 "*** TODO %^{Description}%? :write:"
	 :immediate-finish t :jump-to-captured t)
	("R" "Review invitation" entry
	 (file+olp "research.org" "Review" "Invitations")
	 "*** TODO %^{Description} :review:\nDEADLINE: %^t\n**** Comments to Authors\n%?\n**** Comments to Editor\n"
	 :jump-to-captured t)
	("i" "Checkable subtask" checkitem (clock)
	 "[ ] %^{Description}%?"
	 :immediate-finish t :jump-to-captured t)
	("n" "Note" entry (file+headline "notes.org" "Notes")
	 "* %^{Description} :note:\n%?" :jump-to-captured t)))

(setq org-directory "~/github/personal/orgs")
(setq org-agenda-files (list "research.org" "teaching.org" "life.org"))
(setq org-default-notes-file
      (concat org-directory "/notes.org"))

;; Custom agenda command definitions
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cr" 'org-capture)

;; ----------------------------------------------------------------------
;; Org export options
;; ----------------------------------------------------------------------

;; LaTeX and beamer
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.5))
(add-to-list 'org-latex-classes
	     '("IEEEtran"
	       "\\documentclass{IEEEtran}
               [PACKAGES]
               [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")))

(add-to-list 'org-latex-classes
	     '("wmsnrpt"
	       "\\documentclass[work]{wmsnrpt}
               [NO-DEFAULT-PACKAGES]
               [PACKAGES]
               [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ;; Beamer related options
;; (setq org-beamer-outline-frame-options "")

;; LaTeX and beamer export
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; ;; Org-ref
;; (setq bibtex-completion-bibliography
;;       '("~/cloud/OneDrive/references/bbt-camerart.bib"))
;; (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
;; (define-key org-mode-map (kbd "C-c (") org-ref-insert-ref-function)
;; (define-key org-mode-map (kbd "C-c )") org-ref-insert-label-function)


;; Zotero rst mode still unsatisfactory.
;; (autoload 'zotero-rst-mode "zotero-rst" "" t)
;; (autoload 'org-zotero-mode "org-zotero" "" t)

;; (defun ieee-parse-rss-entry (entry)
;;   "Parse the `:item-full-text' field for xml tags and create new properties."
;;   (require 'xml)
;;   (with-temp-buffer
;;     (insert (plist-get entry :item-full-text))
;;     (goto-char (point-min))
;;     (while (re-search-forward "<\\([a-zA-Z]+\\>\\).*?><!\\[CDATA\\[\\([^\000]*?\\)\\]\\]></\\1>"
;;                               nil t)
;;       (setq entry (plist-put entry
;;                              (intern (concat ":" (match-string 1)))
;;                              (xml-substitute-special (match-string 2)))))
;;     (goto-char (point-min))
;;     (unless (re-search-forward "isPermaLink[ \t]*=[ \t]*\"false\"" nil t)
;;       (setq entry (plist-put entry :guid-permalink t))))
;;   entry)

;; (defvar ieee-filtered-entries)
;; (setq ieee-filtered-entries 
;;       '((:author . "")
;; 	(:title . "IEEE Foundation")
;; 	(:title . "[Table of Contents]")
;; 	(:title . "Table of Contents")
;; 	(:title . "IEEE Xplore Digital Library [advertisement]")
;; 	(:title . "IEEE Robotics and Automation Society Information")
;; 	(:title . "IEEE Transactions on Robotics information for authors")
;; 	(:title . "IEEE Transactions on Robotics publication information")))

;; (defun ieee-filter-entry (entry)
;;   (if (member t (loop for (key . val) in ieee-filtered-entries
;;                       collect (string= (plist-get entry key) val)))
;;       nil
;;     entry))

;; ;; Feeds

;; (setq org-feed-alist
;;       '(("PR"
;; 	 "http://feeds.sciencedirect.com/publication/science/00313203"
;; 	 "~/github/personal/orgs/feeds.org" "Pattern Recognition")
;; 	;; ("TPAMI"
;; 	;;  "http://csdl.computer.org/rss/tpami.xml"
;; 	;;  "~/github/personal/orgs/feeds.org" "PAMI")
;; 	("TCSVT"
;; 	 "http://ieeexplore.ieee.org/rss/TOC76.XML"
;; 	 "~/github/personal/orgs/feeds.org" "TCSVT")))
;; 	 ;; :parse-entry ieee-parse-rss-entry
;; 	 ;; :filter ieee-filter-entry)))
;; ----------------------------------------------------------------------
;; Refile Setup
;; ----------------------------------------------------------------------

;; Targets include this file and any file contributing to the agenda
;;  - up to 5 levels deep
(setq org-refile-targets 
      (quote ((nil :maxlevel . 5)
	      ("journal.org" :maxlevel . 5))))

;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

;; Targets complete in steps so we start with filename, 
;; TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; ----------------------------------------------------------------------
;; Make RefTeX mode work with Org mode
;; ----------------------------------------------------------------------
(defun org-mode-myhook ()
  (progn
    (delete '("\\.pdf\\'" . default) org-file-apps)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))
;; ;; Make RefTeX mode work with org mode
;; (load-library "reftex")
;; (and (buffer-file-name)
;; 	   (file-exists-p (buffer-file-name))
;; 	   (reftex-parse-all))
;; (define-key org-mode-map (kbd "C-c [") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-myhook)

;; ----------------------------------------------------------------------
;; Capture templates for: TODO tasks, Notes, appointments, phone calls
;; ----------------------------------------------------------------------

;; ;; ----------------------------------------------------------------------
;; ;; Make RefTeX mode work with rst mode
;; ;; ----------------------------------------------------------------------

;; (defun rst-locate-citation-files (master-dir &optional files)
;;   ;; Scan buffer for citation macro and return file list.
;;   ;; (print "ORG INIT:")					; DEBUG Info
;;   ;; (print major-mode)					; DEBUG Info
;;   (if (string= major-mode "latex-mode")
;; 	  (progn
;; 		(setq bibre
;; 			  (concat "\\(^\\)[^%\n\r]*\\\\\\("
;; 					  (mapconcat 'identity reftex-bibliography-commands "\\|")
;; 					  "\\){[ \t]*\\([^}]+\\)"))
;; 		(setq splitre "[ \t\n\r]*,[ \t\n\r]*")))

;;   (if (string= major-mode "rst-mode")
;; 	(progn
;; 	  (setq bibre
;; 			(concat "\\(^\\)\\.\\.[ \t]*\\("
;; 					"citation\\|bibliography" "\\)::[ \t]*\\(.+\\)"))
;; 	  (setq splitre "[ \t\n\r]+")))
  
;;   (unless files
;;     (save-excursion
;;       (goto-char (point-min))
;;       (if (re-search-forward bibre nil t)
;;           (setq files (split-string 
;; 					   (when (match-beginning 3)
;; 						 (buffer-substring-no-properties
;; 						  (match-beginning 3) (match-end 3)))
;; 					   splitre)))))
;;   ;; (print files)							; DEBUG Info
;;   (when files
;;     (setq files 
;;           (mapcar
;;            (lambda (x)
;;              (if (or (member x reftex-bibfile-ignore-list)
;;                      (delq nil (mapcar (lambda (re) (string-match re x))
;;                                        reftex-bibfile-ignore-regexps)))
;;                  ;; excluded file
;;                  nil
;;                ;; find the file
;;                (reftex-locate-file x "bib" master-dir)))
;;            files))
;; 	;; (print files)						; DEBUG Info
;;     (delq nil files)))

;; (defun fred-rst-mode-hook ()
;;   (make-local-variable 'reftex-cite-format)
;;   (setq reftex-cite-format ":cite:`%l`")
;;   (fset 'reftex-locate-bibliography-files
;; 		'rst-locate-citation-files)
;;   (turn-on-reftex)
;;   (reftex-parse-all)
;;   (define-key rst-mode-map (kbd "C-c [") 'reftex-citation))

;; ;; (add-hook 'rst-mode-hook 'fred-rst-mode-hook)

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
