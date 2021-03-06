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
;; Last-Updated: 2017-03-11 16:55:05(+0800) [by Fred Qi]
;;     Update #: 47
;; ----------------------------------------------------------------------

;; (require 'python)
(require 'cython-mode)
(require 'django-mode)

; (elpy-enable)
;; (elpy-use-ipython)
;; (setq elpy-rpc-backend "jedi")
(setq flymake-log-level 3)
;; (elpy-clean-modeline)

;; Make SConstruct files be editted in python mode
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))

(setq python-indent-offset 4)

(defun fred-python-mode-hook ()
  "Fred's hook for python mode"
  (setq comment-start "# ")
  (setq python-python-command "python")
  (setq python-command python-python-command)
  (fred-auto-header-hook)
  (require 'sphinx-doc)
  (sphinx-doc-mode t))

(setq python-python-command local-python-path)

(add-hook 'python-mode-hook 'fred-python-mode-hook)

;; (require 'ein)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'android-mode)
(setq android-mode-sdk-dir "~/android/sdk")

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
