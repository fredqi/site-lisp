;; ----------------------------------------------------------------------
;; START OF FILE
;; ----------------------------------------------------------------------
;; 
;; Filename: mathinit.el
;; Author: Fred Qi
;; Created: 2005-02-20 23:36:18(+0800)
;; 
;; ----------------------------------------------------------------------
;;; CHANGE LOG
;; 12-Mar-2006    Fred Qi  
;;    Rev: previously matlabinit.el
;; ----------------------------------------------------------------------
;; Last-Updated: 2013-12-28 22:25:56(+0400) [by Fred Qi]
;;     Update #: 78
;; ----------------------------------------------------------------------
;;; COMMENTARY
;; ----------------------------------------------------------------------
;; Setup math modes corresponding to system enviroment
;; 
;; 
;; ----------------------------------------------------------------------


;; ----------------------------------------------------------------------
;; MATLAB settings
;; ----------------------------------------------------------------------
(autoload 'matlab-eei-connect "matlab-eei"
  "Connects Emacs to MATLAB's external editor interface.")

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
;; (autoload 'mlint-minor-mode "mlint" nil t)

(setq matlab-shell-command
	  (concat local-matlab-root "/bin/matlab" ) )
(setq matlab-indent-function t)			; if you want function bodies indented
(setq matlab-verify-on-save-flag nil)	; turn off auto-verify on save

(defun fred-matlab-mode-hook ()
  (fred-auto-header-hook)
  (setq fill-column 80)
  (if (fboundp 'matlab-eei-minor-mode)
	  (matlab-eei-minor-mode 1))
  ;; ; turn on mlint
  ;; (mlint-minor-mode 1)
  ; where auto-fill should wrap
  (imenu-add-to-menubar "Find"))

(add-hook 'matlab-mode-hook 'fred-matlab-mode-hook)
	
;; The mlint package
;; ---------------------------------------------------------------------- 
;; checks for common M-file coding errors, such as omitting semicolons at the
;; end of lines. mlint requires Eric Ludlam's cedet package, which is not
;; included in the EmacsLink distribution. If you enable mlint, you must
;; download cedet from http://cedet.sourceforge.net/ and install it on your
;; system before using EmacsLink.
;;
;; Uncomment the next two lines to enable use of
;; the mlint package provided with EmacsLink.

;; (setq matlab-show-mlint-warnings t)
;; (setq matlab-highlight-cross-function-variables t)


;; ----------------------------------------------------------------------
;; Fred's code stuffs
;; ----------------------------------------------------------------------
(defun fred-modify-calibration ( buffer )
  "Fred's function for gathering calibration data."
  (interactive "b")
  (beginning-of-buffer)
  (replace-regexp "^frame.*\n" "")
  (beginning-of-buffer)
  (replace-regexp ".*;.*;.*;.*;\n" "")
  (beginning-of-buffer))

;; ----------------------------------------------------------------------
;;; END OF FILE 
;; ----------------------------------------------------------------------
