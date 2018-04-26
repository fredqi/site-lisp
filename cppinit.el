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
;; Last-Updated: 2014-09-08 16:04:54(+0400) [by Fred Qi]
;;     Update #: 487
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
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(require 'doxymacs)
(require 'cc-mode)
(require 'etags)
(require 'tempo)
(require 'toggle-source)

(autoload 'cmake-mode "cmake-mode" "CMake major Mode" t)
;; (autoload 'iss-mode "iss-mode" "Innosetup Script Mode" t)
;; (autoload 'ini-mode "ini-mode" "Major Mode for editing ini files" t)
;; (autoload 'ned-mode "ned-mode" "Major Mode for editing Ned files" t)
;; (setq auto-mode-alist (cons '("\\.ned\\'" . ned-mode) auto-mode-alist))

;; ----------------------------------------------------------------------
;; CODE RELATED SETTINGS - MAINLY GEARED TOWARD C/C++
;; ----------------------------------------------------------------------

;; Tells Emacs which mode to use for which file type. this explicitly forces
;; these file types to invoke the modes I specify. This guarantees the behavior
;; I want when editing specific file types - source files in particular.

(add-to-list 'auto-mode-alist '("\\.[ch]$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu[h]?$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.[ch]xx\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.[ch]pp\\'"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'"      . c++-mode))

;; Need not to add following associations
;; ("\\.C$"    . c++-mode)
;; ("\\.H$"    . c++-mode)
;; ("\\.hh$"   . c++-mode)
;; ("\\.idl$"  . c++-mode)
;; ("\\.cmake$" . cmake-mode)
;; ("CMakeLists\\.txt$" . cmake-mode)
;; ("\\.iss$"  . iss-mode)
;; ("\\.ned$"  . ned-mode)
;; ("\\.ini$"  . ini-mode)
;; ("\\.vhdl$" . vhdl-mode)

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

;; (setq tags-table-list '("~/projects/vision"))
;; (add-to-list tags-table-list '("d:/svnwork/svn-build/src-trunk/subversion"))
;; (add-to-list tags-table-list '("~/trunk/projects/vision"))

;; (regexp-opt '("BOOL" "LPCSTR" "LPCTSTR" "HRESULT" "BYTE" "DWORD" "UINT" "ULONG"
;; 			  "bool" "PCHAR" "UCHAR" "WORD" "size_t" "_int64" "_unit64" "bool"
;; 			  "boolean" "FILE" "todo" "TODO" "BUG" "FIXME" "TRUE" "FALSE" "true"
;; 			  "false") )
;; (regexp-opt '( "ios" "string" "rope" "list" "slist" "deque" "vector" "set"
;; 			   "multiset" "map" "multimap" "hash" "stack" "queue" "priority_queue"
;; 			   "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator"
;; 			   "reference" "const_reference" "LPCTSTR" "BYTE" "WORD" "DWORD" "FIXME"
;; 			   "true" "false" "private" "protected" "public" "__forceinline" "default") )

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
  "Fred's hook for all cc-modes"
  (fred-auto-header-hook)
  (doxymacs-mode)
  (doxymacs-font-lock)
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

;; (setq iss-compiler-path local-inno-path)
;; (defun fred-iss-mode-init ()
;;   (fred-auto-header-hook)
;;   (define-key iss-mode-map [f6] 'iss-compile)
;;   (define-key iss-mode-map [(meta f6)] 'iss-run-installer))

;; (add-hook 'font-lock-mode-hook 'fred-doxymacs-font-lock-hook)
(add-hook 'c-mode-common-hook 'fred-c-mode-common-hook)
(add-hook 'c-mode-hook 'fred-c-mode-hook)
(add-hook 'c++-mode-hook 'fred-c-mode-hook)
;; (add-hook 'iss-mode-hook 'fred-iss-mode-init)
;; (add-hook 'ini-mode-hook 'fred-auto-header-hook)
;; (add-hook 'ned-mode-hook 'fred-auto-header-hook)
(add-hook 'cmake-mode-hook 'fred-auto-header-hook)

;; hh mk:@MSITStore:%wxwin%\docs\htmlhelp\wx.chm::/wx_wxslider.html#wxslidergetticks

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
