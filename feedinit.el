;;; feedinit.el ---
;;
;; Filename: feedinit.el
;; Author: Fred Qi
;; Created: 2022-10-08 18:26:37(+0800)
;;
;; Last-Updated: 2022-10-10 23:36:06(+0800) [by Fred Qi]
;;     Update #: 270
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
;; https://cundy.me/post/elfeed/
;; https://www.astro.umd.edu/~chongchong/emacs-dotfile/#org1791dd2

;;; Code:

(defun compose-arxiv-rss-search (category tag length)
  "Composing arXiv RSS search."
  (let* ((arxiv-api-prefix "https://export.arxiv.org/api/query?")
	 (arxiv-api-option-cat (concat "cat:" category))
	 (arxiv-api-option-length (format "max_results=%d" length))
	 (arxiv-api-options
	  (string-join (list arxiv-api-option-cat
			     arxiv-api-option-length
			     "sortBy=lastUpdatedDate"
			     "sortOrder=descending") "&"))
	 (arxiv-rss (concat arxiv-api-prefix "search_query=" arxiv-api-options)))
    (list arxiv-rss tag)))

(use-package elfeed
  :ensure t
  :defer t
  :commands (elfeed)
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
	(list (compose-arxiv-rss-search "cs.CV" 'CVPR 128)
	      (compose-arxiv-rss-search "cs.MM" 'MM 128)
	      (compose-arxiv-rss-search "cs.LG" 'ML 128))))

;; (require 'elfeed-score)
;; (elfeed-score-enable)

(defun concatenate-authors (authors-list &optional fullname)
  "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
  (mapconcat
   (lambda (author)
     (if fullname
	 (plist-get author :name)
       (car (last (split-string (plist-get author :name))))))
   authors-list ", "))

(defun my-search-print-fn (entry)
  "Print ENTRY to the buffer."
  (let* ((elfeed-search-date-format '("%y%m%d" 6 :left))
	 (elfeed-search-title-max-width 256)
	 (date (elfeed-search-format-date (elfeed-entry-date entry)))
	 (title (or (elfeed-meta entry :title)
		    (elfeed-entry-title entry) ""))
	 (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	 (feed (elfeed-entry-feed entry))
	 (feed-title
	  (when feed
	    (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
	 (entry-authors (concatenate-authors
			 (elfeed-meta entry :authors)))
	 (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
	 (tags-str (mapconcat
		    (lambda (s) (propertize s 'face
					    'elfeed-search-tag-face))
		    tags ","))
	 (title-width (* (/ (- (window-width) 30) 3) 2))
	 (title-column (elfeed-format-column
			title (elfeed-clamp
			       elfeed-search-title-min-width
			       title-width
			       elfeed-search-title-max-width)
			:left))
	 (authors-width (/ title-width 2))
	 (authors-column (elfeed-format-column
			  entry-authors (elfeed-clamp
					 elfeed-search-title-min-width
					 authors-width
					 128)
			  :left)))

    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
			'face title-faces 'kbd-help title) " ")

    (insert (propertize authors-column
			'face 'elfeed-search-date-face
			'kbd-help entry-authors) " ")
    (when tags
      (insert "(" tags-str ")"))))

(setq elfeed-search-print-entry-function #'my-search-print-fn)

(setq elfeed-feeds
      (list (compose-arxiv-rss-search "cs.CV" 'CVPR 128)
	    (compose-arxiv-rss-search "cs.MM" 'MM 128)
	    (compose-arxiv-rss-search "cs.LG" 'ML 128)))

;; org capture in elfeed
(defun oc-get-elfeed-entry ()
  (when org-store-link-plist
    (let* ((title (plist-get org-store-link-plist :elfeed-entry-title))
	   (link (plist-get org-store-link-plist :elfeed-entry-link))
	   (content (elfeed-deref
		     (plist-get org-store-link-plist :elfeed-entry-content)))
	   (entry-date (format-time-string
			"%Y-%m-%d"
			(plist-get org-store-link-plist :elfeed-entry-date)))
	   (entry-meta (plist-get org-store-link-plist :elfeed-entry-meta))
	   (entry-authors (concatenate-authors (plist-get entry-meta :authors) t))
	   (entry-categories (string-join (plist-get entry-meta :categories) " ")))
      (concat "\n  " entry-authors "\n  [[" link "]] "
	      entry-categories " " entry-date "\n\n" content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; feed.el ends here
