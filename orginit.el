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
;; Last-Updated: 2024-10-24 11:54:58(+0800) [by Fred Qi]
;;     Update #: 1163
;; ----------------------------------------------------------------------

;;; Code:

(require 'org)

;; (require 'org-pomodoro)
(require 'ox-beamer)
(require 'ox-hugo)
(require 'oc-csl)

(eval-after-load "org"
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (emacs-lisp . t)
       ;; (python . t)
       (gnuplot . t)
       (dot . t)))
    (setq org-agenda-span 10)
    (setq org-agenda-start-on-weekday nil)
    (setq org-duration-format (quote h:mm))
    (setq org-confirm-babel-evaluate nil)
    (setq org-latex-listings t)
    (setq org-agenda-time-grid
	  '((daily today require-timed)
	    (830 1130 1245 1545 1845)
	    "......" "----------------"))
    ;; (conda-env-activate "base")
    ;; (setq org-babel-python-command "python3")
    (setq org-cite-csl-styles-dir "~/cloud/zotero/styles")))

;; (setq todoist-token "7999636165f29b30242db38de5c65c8c8978c691"
;;       todoist-backing-buffer "todoist.org"
;;       todoist-show-all t)

(require 'org-arbeitszeit)

(with-eval-after-load 'ox-hugo
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References"))

(setq org-agenda-custom-commands
      '(("r" "Research tasks"
         ((agenda "")
	  (tags "read") (tags "write") (tags "code") (tags "review")))
	("l" "Life matters"
         ((todo "REFR")))))

;; GTD and agenda
(setq org-log-done 'time)
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

;; Capture templates
(setq org-capture-templates
      '(("r" "Reading task" entry
	 (file+olp "reading.org" "General Research" "Reading list")
	 "*** TODO %^{Description}%? :read:"
	 :immediate-finish t :jump-to-captured t)
	("l" "Read an elfeed entry" entry
	 (file+olp "reading.org" "General Research" "Reading list")
	 "*** TODO %a :read:\n%(oc-get-elfeed-entry)\n")
	("w" "Writing task" entry
	 (file+olp "writing.org" "General Research" "Writing list")
	 "*** TODO %^{Description}%? :write:"
	 :immediate-finish t :jump-to-captured t)
	("R" "Review invitation" entry
	 (file+olp "reading.org" "Review" "Invitations")
	 "*** TODO %^{Description} :review:\nDEADLINE: %^t\n**** Comments to Authors\n%?\n**** Comments to Editor\n"
	 :jump-to-captured t)
	("i" "Checkable subtask" checkitem (clock)
	 "[ ] %^{Description}%?"
	 :immediate-finish t :jump-to-captured t)
	("n" "Note" entry (file+headline "notes.org" "Notes")
	 "* %^{Description} :note:\n%?" :jump-to-captured t)))

(setq org-directory "~/cloud/OneDrive/personal/pim/orgs"
      org-default-notes-file (concat org-directory "/notes.org")
      org-archive-location "archive.org::")
(setq org-agenda-files
      '("writing.org" "reading.org" "teaching.org"
	"management.org" "life.org"))

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
	     '("ctexart"
	       "\\documentclass{ctexart}
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

;; LaTeX and beamer export
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

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

;; Refile Setup
;; ----------------------------------------------------------------------

(setq org-refile-use-outline-path (quote file))
(setq org-refile-targets
      (quote ((org-agenda-files :maxlevel . 3)
	      ("archive.org" :maxlevel . 3))))

;; Targets complete in steps so we start with filename,
;; TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; (defun org-dblock-write:weekly (params)
;;   (cl-flet ((fmttm (tm) (format-time-string (org-time-stamp-format t t) tm)))
;;     (let ((file (or (plist-get params :file) (buffer-file-name)))
;;           (start (seconds-to-time
;;                   (org-matcher-time (plist-get params :tstart))))
;;           (end (seconds-to-time (org-matcher-time (plist-get params :tend)))))
;;       (while (time-less-p start end)
;;         (let ((next-week (time-add start
;;                                    (date-to-time "1970-01-08T00:00Z")))
;;               (week-begin (line-beginning-position))
;;               (week-minutes 0))
;;           (insert "Weekly Table from " (fmttm start) "\n")
;;           (insert "| Day | Time |\n|-\n")
;;           (while (time-less-p start next-week)
;;             (let* ((next-day (time-add start (date-to-time "1970-01-02T00:00Z")))
;;                    (minutes
;;                     (with-current-buffer (find-file-noselect file)
;;                       (cadr (org-clock-get-table-data
;;                              file
;;                              (list :maxlevel 0
;;                                    :tstart (fmttm start)
;;                                    :tend (fmttm next-day))))))
;; 		   (hours (/ minutes 60))
;; 		   (mins (- minutes (* hours 60))))
;;               (insert "|" (format-time-string "%a" start)
;;                       "|" (format "%d:%02d" hours mins)
;;                       "|\n")
;;               (org-table-align)
;;               (incf week-minutes minutes)
;;               (setq start next-day)))
;; 	  (insert "|-\n| |" (format "%5.2f" (/ week-minutes 60.0)) "\n|-\n")
;; 	  (org-table-align)
;;           (when (equal week-minutes 0)
;;             (delete-region week-begin (line-beginning-position))))))))

(defun org-mode-myhook ()
  (progn
    (delete '("\\.pdf\\'" . default) org-file-apps)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

(add-hook 'org-mode-hook 'org-mode-myhook)

;; ----------------------------------------------------------------------
;;; END OF FILE
;; ----------------------------------------------------------------------
