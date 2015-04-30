;;; Python Environment
; python-mode
(setq py-install-directory "~/.emacs.d/plugins/python-mode.el-6.2.0")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)


;; To change the Python default shell use 
;;M-x customize-variable py-shell-name 
;; or insert following lines in init.el or .emacs
;;(setq py-shell-name "MY-PYTHON")
;; e.g.
;;(setq py-shell-name "ipython")
;; or
;;(setq py-shell-name "PATH/TO/MY-PYTHON")
;; e.g.
;;(setq py-shell-name "PATH/TO/ipython")

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
;(setq py-shell-switch-buffers-on-execute-p t)
;(setq py-switch-buffers-on-execute-p t)
; don't split windows
;(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


;;; Pymacs and ropemacs
; pymacs
;;(add-to-list 'load-path "~/.emacs.d/pymacs-0.25")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

; ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

