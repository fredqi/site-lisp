;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: pyinit.el
;; Author: Fred Qi
;; Created: 2010-03-04 12:11:08(+0800)
;; 
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;; ----------------------------------------------------------------------
;; Last-Updated: 2023-06-07 13:42:30(+0800) [by Fred Qi]
;;     Update #: 76
;; ----------------------------------------------------------------------

;;; Code:
(require 'cython-mode)
(require 'django-mode)

;; Make SConstruct files be editted in python mode
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq comment-start "# "
		  python-indent-offset 4)
	    (anaconda-mode)
	    (fred-auto-header-hook)
	    (require 'sphinx-doc)
	    (sphinx-doc-mode t)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(provide 'pyinit)
;;; pyinit.el ends here
