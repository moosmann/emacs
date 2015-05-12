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
(setq py-shell-name "/usr/local/bin/ipython")
;; or
;;(setq py-shell-name "PATH/TO/MY-PYTHON")
;; e.g.
;;(setq py-shell-name "PATH/TO/ipython")

; use IPython
(setq-default py-shell-name "/usr/local/bin/ipython")
; py-which-bufname is an internal variable, kind of legacy. better 
; don't touch
;(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
;(setq py-python-command-args
;  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;(setq py-force-py-shell-name-p t)

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


;-------------------------;
;;; Pylint thru flymake ;;;
;-------------------------;

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; Keymaps to navigate to the errors
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))


;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)
