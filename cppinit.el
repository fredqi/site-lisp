;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: cppinit.el
;; Author: Fred Qi
;; Created: 2005-02-01 16:10:07(+0800)
;; 
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;; 2006-04-24 00:05:24(+0800)    Fred Qi@lab
;;    Added a elisp function to recursively generating etags.
;; ----------------------------------------------------------------------
;; Last-Updated: 2022-01-23 22:03:54(+0800) [by Fred Qi]
;;     Update #: 569
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; COMPILING WITH MS VISUAL STUDIO
;; ----------------------------------------------------------------------
;;
;; note that the lib and include environment variables must be set, and they
;; must point to all directories containing headers and static libs required
;; for your build
;; (setq compile-command "NMAKE /f \"TestClasses.mak\" CFG=\"TestClasses - Win32 Release\"")
;; (setq compile-command "NMAKE /f \"TestClasses.mak\" CFG=\"TestClasses - Win32 Debug\"")
;; (setq compile-command "NMAKE /f \"SCTS.mak\" \"SCTS\" - Win32 Debug\"")
;; (setq compile-command "NMAKE /f \"SCTS.mak\" \"SCTS\" - Win32 Release\"")
;; (setq compile-command "NMAKE /f \"nqds_compare.mak\" \"nqds_compare\" - Win32 Debug\"")
;; (setq compile-command "NMAKE /f \"nqds_compare.mak\" \"nqds_compare\" - Win32 Release\"")
;;
;; packages:
;; toggle-source.el
;;	http://opensource.hld.ca/trac.cgi/browser/trunk/config/emacs/.elisp/toggle-source.el?rev=63
;;
;; References:
;; - <https://dreamerjonson.com/2019/12/25/emacs-cc/index.html>
;;
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

;;; Code:
(use-package flycheck
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint))
  
(use-package cc-mode
  :ensure t
  :hook
  (c-mode-common-hook . fred-c-mode-common-hook)
  (c-mode-hook . fred-c-mode-hook)
  (c++-mode-hook . fred-c-mode-hook))

;; rtags https://github.com/Andersbakken/rtags
(use-package rtags
  :ensure t
  :hook
  (c-mode . rtags-start-process-unless-running)
  (c++-mode . rtags-start-process-unless-running)
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-completions-enabled t)
  :bind
  (("M-." . rtags-find-symbol-at-point)
   ("M-," . rtags-find-references-at-point)
   ("C-c f" . rtags-location-stack-forward)
   ("C-c b" . rtags-location-stack-back)
   ("C-c R" . rtags-rename-symbol)
   ("C-c T" . rtags-print-symbol-info)
   ("C-c t" . rtags-symbol-type)
   ("C-c i" . rtags-get-include-file-for-symbol)))

;; https://github.com/atilaneves/cmake-ide
;; cmake-ide enable rdm(rtags) auto start and rc(rtags) to watch directory
(use-package cmake-ide
  :ensure t
  :config (cmake-ide-setup))

(use-package cmake-mode
  :ensure t
  :hook
  (cmake-mode-hook . fred-auto-header-hook)
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists.txt" . cmake-mode)))

;; ----------------------------------------------------------------------
;; CODE RELATED SETTINGS - MAINLY GEARED TOWARD C/C++
;; ----------------------------------------------------------------------
(setq gdb-many-windows t)
(setq indent-tabs-mode nil)
(setq angry-mob-with-torches-and-pitchforks t)

;; the "ellemtel" coding style is just what I want - almost! The only changes I
;; want are offset levels of 4 instead of 2, and the open & close braces to not
;; be indented. This syntax just takes the gnu style and makes these specific
;; changes.
(c-add-style "standard"
             '("ellemtel"
               (c-echo-syntactic-information-p  . t)
               (c-toggle-hungry-state           . 1)
               (c-basic-offset                  . 4)
               (c-offsets-alist                 . ((substatement-open . 0)
                                                   (case-label        . +)
                                                   (block-close       . 0)))))

;; use the "slightly modified gnu style" as my default
(setq c-default-style "standard")

;; these are additional keywords that I want to be syntax-highlighted.
(setq c-font-lock-extra-types
      (append
       '("BOOL" "LPCSTR" "LPCTSTR" "HRESULT" "BYTE" "DWORD" "UINT" "ULONG"
         "bool" "PCHAR" "UCHAR" "WORD" "size_t" "_int64" "_unit64" "bool"
         "boolean" "FILE" "todo" "TODO" "BUG" "FIXME" "TRUE" "FALSE" "true"
         "false")
       c-font-lock-extra-types))

;; more C++ types to font-lock
(setq c++-font-lock-extra-types
      (append
       '("ios" "string" "rope" "list" "slist" "deque" "vector" "set"
         "multiset" "map" "multimap" "hash" "stack" "queue" "priority_queue"
         "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator"
         "reference" "const_reference" "LPCTSTR" "BYTE" "WORD" "DWORD" "FIXME"
         "true" "false" "private" "protected" "public" "__forceinline" "default")
       c-font-lock-extra-types
       c++-font-lock-extra-types))

(add-to-list 'c++-font-lock-extra-types
			 "\\bwx[A-Z][a-z][a-zA-Z]*?\\b")
 
(defun c-wx-lineup-topmost-intro-cont (langelem)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "EVT_" (line-end-position) t)
		'c-basic-offset
      (c-lineup-topmost-intro-cont langelem))))
 
(defun fred-c-mode-common-hook ()
  "Fred's hook for all cc-modes."
  (fred-auto-header-hook)
  (hs-minor-mode 1)
  (local-set-key (kbd "C-c C-r") 'c-indent-line-or-region)
  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c \\") 'hs-toggle-hiding))

(defun fred-c-mode-hook ()
  "Fred's hook for c mode"
  (c-set-offset
   'topmost-intro-cont
   'c-wx-lineup-topmost-intro-cont)
  (cond (winnt-p
         (setenv "APPVER" "5.01")
         (setenv "TARGETOS" "WINNT")
         (setenv "_WIN32_IE" "0x0600")
         (setenv "CPU" "i386")
         (set
          (make-local-variable 'compile-command)
          (concat "nmake -k XP32_DEBUG\\"
                  (file-name-sans-extension
                   (file-name-nondirectory buffer-file-name))
                  ".obj")))))

(defun fred-recursive-etags (dir)
  "Fred's generating etags recursively on the given directory(dir)."
  (let ((ignore ".*[^.]\\{1,2\\}$")
		(etagscmd "etags -a -l c -l c++ --members --declarations -o")
		dirlist currdir files-currdir file output)
	(if (file-directory-p dir)
		(setq dirlist (cons (expand-file-name dir) ())
			  output (concat (expand-file-name dir) "/TAGS ")))
	(if (file-exists-p output) (delete-file output))
	(while dirlist
	  (setq currdir (car dirlist))
	  (setq dirlist (cdr dirlist))
      (message "etags is working: %s" currdir)
	  (shell-command (concat etagscmd output currdir "/*.h"))
	  (shell-command (concat etagscmd output currdir "/*.hpp"))
	  (shell-command (concat etagscmd output currdir "/*.c"))
	  (shell-command (concat etagscmd output currdir "/*.cpp"))
	  (setq files-currdir (directory-files currdir t ignore))
	  (while files-currdir
		(setq file (expand-file-name
					(car files-currdir)))
		(if (and (file-directory-p file)
                 (not (search ".svn" file)))
            (setq dirlist (cons file dirlist)))
		(setq files-currdir (cdr files-currdir))))))


;; hh mk:@MSITStore:%wxwin%\docs\htmlhelp\wx.chm::/wx_wxslider.html#wxslidergetticks

;; ----------------------------------------------------------------------
;;; END OF FILE
;; ----------------------------------------------------------------------
