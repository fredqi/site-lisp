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
;; ----------------------------------------------------------------------
;; Last-Updated: 2017-04-26 19:53:49(+0800) [by Fred Qi]
;;     Update #: 562
;; ----------------------------------------------------------------------

;; ;; el-pocket
;; (require 'el-pocket)
;; (el-pocket-load-auth)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; Org-mode settings
;; (setq debug-on-error t)
(require 'org)
;; (require 'org-exp)
(require 'ox-latex)
(require 'ox-beamer)
;; (require 'ox-reveal)
;; (require 'ox-gfm)

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-latex-listings t)

;; disable the pomodoro
;; (require 'org-pomodoro)
;; (setq request-curl "curl-735")
;; (require 'org-toodledo)
;; (setq org-toodledo-folder-support-mode 
;; 	  (quote heading))
;; (setq org-toodledo-sync-on-save "yes")
;; (setq org-toodledo-userid "td5361f42a355de")
;; (setq org-toodledo-password "qsgct1s")
;; (setq org-toodledo-file "~/personal/orgs/toodledo.org")

;; Zotero rst mode still unsatisfactory.
;; (autoload 'zotero-rst-mode "zotero-rst" "" t)
;; (autoload 'org-zotero-mode "org-zotero" "" t)

;; (setq org-toodledo-status-to-org-map
;;   '(("Active" . "TODO")
;;     ("None" . "TODO")
;;     ("Next Action" . "TODO")
;;     ("Planning" . "TODO")
;;     ("Delegated" . "DELEG")
;;     ("Waiting" . "WAIT")
;;     ("Someday" . "SDAY")
;;     ("Hold" . "SDAY")
;;     ("Postponed" . "SDAY")
;;     ("Canceled" . "CANCELED")
;;     ("Reference" . "REFR")))

(setq org-log-done 'time)
(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-keywords
	  (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
			  (sequence "REFR(Q!)" "|" "DONE(d!/!)")
			  (sequence "WAIT(w@/!)" "SDAY(S!)" 
						"OPEN(O@)" "|" "CANCELLED(c@/!)" "PHONE")
			  (sequence "DELEG(q!)" "|" 
						"APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))))

(setq org-todo-keyword-faces
 	  (quote (("TODO" :foreground "red" :weight bold)
			  ("STARTED" :foreground "blue" :weight bold)
 			  ("DONE" :foreground "forest green" :weight bold)
 			  ("WAIT" :foreground "orange" :weight bold)
			  ("SDAY" :foreground "magenta" :weight bold)
 			  ("CANCELLED" :foreground "forest green" :weight bold)
			  ("QUOTE" :foreground "red" :weight bold)
			  ("REFR" :foreground "magenta" :weight bold)
			  ("APPROVED" :foreground "forest green" :weight bold)
			  ("EXPIRED" :foreground "forest green" :weight bold)
			  ("REJECTED" :foreground "forest green" :weight bold)
			  ("DELEG" :foreground "blue" :weight bold)
			  ("PHONE" :foreground "forest green" :weight bold))))
 
(setq org-todo-state-tags-triggers
	  (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAIT" ("WAIT" . t) ("NEXT"))
              ("SDAY" ("WAIT" . t))
              (done ("NEXT") ("WAIT"))
              ("TODO" ("WAIT") ("CANCELLED") ("NEXT"))
              ("STARTED" ("WAIT"))
              ("DONE" ("WAIT") ("CANCELLED") ("NEXT")))))

(setq org-directory "~/personal/orgs/")

(setq org-agenda-files (list "toodledo.org"))
(setq org-default-notes-file "~/personal/orgs/notes.org")


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
;; 	  '((:author . "")
;; 		(:title . "IEEE Foundation")
;; 		(:title . "[Table of Contents]")
;; 		(:title . "Table of Contents")
;; 		(:title . "IEEE Xplore Digital Library [advertisement]")
;; 		(:title . "IEEE Robotics and Automation Society Information")
;; 		(:title . "IEEE Transactions on Robotics information for authors")
;; 		(:title . "IEEE Transactions on Robotics publication information")))

;; (defun ieee-filter-entry (entry)
;;   (if (member t (loop for (key . val) in ieee-filtered-entries
;;                       collect (string= (plist-get entry key) val)))
;;       nil
;;     entry))

;; (setq org-feed-alist
;; 	  '(("CVIU"
;; 		 "http://feeds.sciencedirect.com/publication/science/10773142"
;; 		 "~/personal/orgs/feeds.org" "Computer Vision and Image Understanding")
;; 		("TPAMI"
;; 		 "http://csdl.computer.org/rss/tpami.xml"
;; 		 "~/personal/orgs/feeds.org" "PAMI")
;; 		("TCSVT"
;; 		 "http://ieeexplore.ieee.org/rss/TOC76.XML"
;; 		 "~/personal/orgs/feeds.org" "CSVT"
;; 		 :parse-entry ieee-parse-rss-entry
;; 		 :filter ieee-filter-entry)))

;; these custom agenda views will be displayed in the org-mobile app
(setq org-agenda-custom-commands
      '(("x" "MobileOrg TODO List" todo "TODO|STARTED|WAIT" nil
		 (org-agenda-sorting-strategy '(priority-up effort-down)))
		("y" alltodo "")
		("z" "MobileOrg Agenda" agenda "" ((org-agenda-ndays 3)))))

(setq org-agenda-start-on-weekday 0)

;; ("G" "GTD Block Agenda"
;;          ((tags-todo "office")
;;           (tags-todo "reading")
;;           (tags-todo "phone")
;;           (tags-todo "writing")))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; ----------------------------------------------------------------------
;; Refile Setup
;; ----------------------------------------------------------------------

;; Use IDO for target completion
(setq org-completion-use-ido t)
;; Targets include this file and any file contributing to the agenda
;;  - up to 5 levels deep
(setq org-refile-targets 
	  (quote ((nil :maxlevel . 5)
			  ("journal.org" :maxlevel . 5)
			  ("reading.org" :maxlevel . 5))))

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
  ;; Open PDFs files with Evince
  (if linux-p
	  (progn
		(delete '("\\.pdf\\'" . default) org-file-apps)
		(add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))
  ;; Make RefTeX mode work with org mode
  (load-library "reftex")
  (and (buffer-file-name)
	   (file-exists-p (buffer-file-name))
	   (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-myhook)

;; ----------------------------------------------------------------------
;; Org export options
;; ----------------------------------------------------------------------

(add-to-list 'org-latex-classes
			 '("wmsnrpt"
			   "\\documentclass[work]{wmsnrpt}"
			   ("\n\\section{%s}" . "\n\\section*{%s}")
			   ("\n\\subsection{%s}" . "\n\\subsection*{%s}")
			   ("\n\\subsubsection{%s}" . "\n\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
			 '("wmsnlect"
			   "\\documentclass{wmsnlect}"
			   ("\n\\section{%s}" . "\n\\section*{%s}")
			   ("\n\\subsection{%s}" . "\n\\subsection*{%s}")
			   ("\n\\subsubsection{%s}" . "\n\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
			 '("course"
			   "\\documentclass{course}"
			   ("\n\\section{%s}" . "\n\\section*{%s}")
			   ("\n\\subsection{%s}" . "\n\\subsection*{%s}")
			   ("\n\\subsubsection{%s}" . "\n\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")
			   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
			 '("IEEEtran"
			   "\\documentclass{IEEEtran}"
			   ("\n\\section{%s}" . "\n\\section*{%s}")
			   ("\n\\subsection{%s}" . "\n\\subsection*{%s}")
			   ("\n\\subsubsection{%s}" . "\n\\subsubsection*{%s}")
			   ("\\paragraph{%s}" . "\\paragraph*{%s}")))

;; Beamer related options
(setq org-beamer-outline-frame-options "")

(setq org-export-latex-packages-alist
	  (quote (("" "graphicx" t)
			  ("" "amsmath" t)
			  ("" "amssymb" t)
			  ("" "subfig" t)
			  ("" "hyperref" nil))))

;; Dim blocked tasks
(setq org-agenda-dim-blocked-tasks t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cr" 'org-capture)

;; ----------------------------------------------------------------------
;; Capture templates for: TODO tasks, Notes, appointments, phone calls
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Settings for syncing with MobileOrg via Dropbox
;; ----------------------------------------------------------------------
(setq org-mobile-directory "~/cloud/dropbox-work/Apps/orgs")
(setq org-mobile-inbox-for-pull "~/cloud/dropbox-work/Apps/orgs/inbox.org")

;; Enable org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)
							 (emacs-lisp . t)))

;; ----------------------------------------------------------------------
;; Make RefTeX mode work with rst mode
;; ----------------------------------------------------------------------

(defun rst-locate-citation-files (master-dir &optional files)
  ;; Scan buffer for citation macro and return file list.
  ;; (print "ORG INIT:")					; DEBUG Info
  ;; (print major-mode)					; DEBUG Info
  (if (string= major-mode "latex-mode")
	  (progn
		(setq bibre
			  (concat "\\(^\\)[^%\n\r]*\\\\\\("
					  (mapconcat 'identity reftex-bibliography-commands "\\|")
					  "\\){[ \t]*\\([^}]+\\)"))
		(setq splitre "[ \t\n\r]*,[ \t\n\r]*")))

  (if (string= major-mode "rst-mode")
	(progn
	  (setq bibre
			(concat "\\(^\\)\\.\\.[ \t]*\\("
					"citation\\|bibliography" "\\)::[ \t]*\\(.+\\)"))
	  (setq splitre "[ \t\n\r]+")))
  
  (unless files
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward bibre nil t)
          (setq files (split-string 
					   (when (match-beginning 3)
						 (buffer-substring-no-properties
						  (match-beginning 3) (match-end 3)))
					   splitre)))))
  ;; (print files)							; DEBUG Info
  (when files
    (setq files 
          (mapcar
           (lambda (x)
             (if (or (member x reftex-bibfile-ignore-list)
                     (delq nil (mapcar (lambda (re) (string-match re x))
                                       reftex-bibfile-ignore-regexps)))
                 ;; excluded file
                 nil
               ;; find the file
               (reftex-locate-file x "bib" master-dir)))
           files))
	;; (print files)						; DEBUG Info
    (delq nil files)))

(defun fred-rst-mode-hook ()
  (make-local-variable 'reftex-cite-format)
  (setq reftex-cite-format ":cite:`%l`")
  (fset 'reftex-locate-bibliography-files
		'rst-locate-citation-files)
  (turn-on-reftex)
  (reftex-parse-all)
  (define-key rst-mode-map (kbd "C-c [") 'reftex-citation))

;; (add-hook 'rst-mode-hook 'fred-rst-mode-hook)

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
