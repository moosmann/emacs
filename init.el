;--------------------;
;;; User Interface ;;;
;--------------------;

;;; Remove start up splash screen
(setq inhibit-splash-screen t)

;;; Remove prompt for killing emacsclient buffers
;; other option is to use the -n option with emacsclient so that it
;; doesn't wait for the file to be edited before exiting. Eg,
;; 'emacsclient -n myfile.txt'
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; Disable toolbar
(tool-bar-mode -1)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style Undo
;; Bind the undo function to the key freed above
;(global-set-key [(control z)] 'undo)
(global-set-key [(control z)] 'yank)

;;; Add short cut for go-to-line
(global-set-key [f5] 'goto-line)

;;; Add short cut for other-window
(global-set-key [f6] 'other-window)

;;; What follows is largely taken form:
;;; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/

(add-to-list 'load-path "~/.emacs.d/")

; always use spaces, not tabs, when indenting
(setq indent-tabs-mode nil)

; ignore case when searching
(setq case-fold-search t)

; require final newlines in files when they are saved
(setq require-final-newline t)

; language
(setq current-language-environment "English")

;; ; window modifications
;; (global-set-key (kbd "S-C-") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-") 'shrink-window)
;; (global-set-key (kbd "S-C-") 'enlarge-window)

; set the keybinding so that you can use f4 for goto line
;(global-set-key &#91;f4&#93; 'goto-line)

;;; Interactively Do Things
(require 'ido)
(ido-mode t)


;----------------------;
;;; Windows & Frames ;;;
;----------------------;

; don't show the startup screen
;(setq inhibit-startup-screen t)
; don't show the menu bar
;(menu-bar-mode nil)
; don't show the tool bar
;(require 'tool-bar)
;(tool-bar-mode nil)
; don't show the scroll bar
;(scroll-bar-mode nil)

; number of characters until the fill column
(setq fill-column 70)

; specify the fringe width for windows -- this sets both the left and
; right fringes to 10
(require 'fringe)
(fringe-mode 10)

; lines which are exactly as wide as the window (not counting the
; final newline character) are not continued. Instead, when point is
; at the end of the line, the cursor appears in the right fringe.
(setq overflow-newline-into-fringe t)

; each line of text gets one line on the screen (i.e., text will run
; off the left instead of wrapping around onto a new line)
(setq truncate-lines t)
; truncate lines even in partial-width windows
(setq truncate-partial-width-windows t)

; display line numbers to the right of the window
(global-linum-mode t)
; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)

;------------;
;;; Cursor ;;;
;------------;

; highlight the current line
(require 'highlight-current-line)
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
(setq hl-line-face (quote highlight))

; don't blink the cursor
(blink-cursor-mode nil)

; make sure transient mark mode is enabled (it should be by default,
; but just in case)
(transient-mark-mode t)

; turn on mouse wheel support for scrolling
(require 'mwheel)
(mouse-wheel-mode t)

; E.g. when following error occurs:
; File error: Cannot open load file, popup
(setq package-user-dir "~/.emacs.d/elpa/")
(package-initialize)

;;; Auto Complete
;; install from MELPA archive via package.el
;; M-x package-install [RET] auto-complete [RET]
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150408.1132")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20150408.1132/dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


;;; Fill-Column Indicator
(add-to-list 'load-path "~/.emacs.d/plugins/fill-column-indicator")
(require 'fill-column-indicator)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)


;-------------------------;
;;; Syntax Highlighting ;;;
;-------------------------;

; text decoration
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)

; if there is size information associated with text, change the text
; size to reflect it
(size-indication-mode t)

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)

;----------;
;;; ELPA ;;;
;----------;
;; ELPA is the Emacs Lisp Package Archive, written originally by TomTromey. It is included in GnuEmacs, starting with version 24. package.el is the package manager library for ELPA. 
;; see http://emacswiki.org/emacs/ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;-----------;
;;; MELPA ;;;
;-----------;
;; To use the MELPA repository, you'll need an Emacs with package.el. Emacs 24 has package.el bundled with it
;; Enable installation of packages from MELPA by adding an entry to package-archives after (require 'package) and before the call to package-initialize in your init.el or .emacs file: 
;; see http://melpa.org/#/
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;-----------------------;
;;; Emacs custom file ;;;
;-----------------------;
(setq custom-file "~/.emacs.d/settings/custom.el")
(load custom-file)

;-----------;
;;; LaTeX ;;;
;-----------;
(setq latex-file "~/.emacs.d/settings/latex.el")
(load latex-file)

;------------;
;;; Python ;;;
;------------;
(setq python-file "~/.emacs.d/settings/python.el")
(load python-file)

;-----------------------------;
;;; C++ mode for CUDA files ;;;
;-----------------------------;
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;---------------;
;;; Functions ;;;
;---------------;

;;; Count words function
;; source: xemacs 20.3
(defun count-words-region (start end)
   (interactive "r")
   (save-excursion
      (let ((n 0))
       (goto-char start)
       (while (< (point) end)
         (if (forward-word 1)
             (setq n (1+ n))))
       (message "Region has %d words" n)
       n)))
;; add alias for word-count
(defalias 'word-count 'count-words-region)
;; (defun count-words (start end)
;;   "Print number of words in the region."
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region start end)
;;       (goto-char (point-min))
;;       (count-matches "\\sw+"))))


;-----------------;
;;; Color Theme ;;;
;-----------------;

; use the "Subtle Hacker" color theme as a base for the custom scheme
;;(require 'color-theme)
;;(color-theme-initialize)
;;(setq color-theme-is-global t)
;;(color-theme-subtle-hacker)

;; (custom-set-faces
;;  '(default ((t (:overline nil :inherit nil :stipple nil :background "gray2"
;;                 :foreground "#FFF991" :inverse-video nil :box nil
;;                 :strike-through nil :underline nil
;;                 :slant normal :weight normal :height 83 :width normal
;;                 :foundry "unknown" :family "DejaVu Sans Mono"))))
;;  '(border ((t nil)))
;;  '(cursor ((t (:background "firebrick1" :foreground "black"))))
;;  '(font-lock-comment-delimiter-face
;;     ((default (:inherit font-lock-comment-face :weight ultra-bold))
;;     (((class color) (min-colors 16)) nil)))
;;  '(font-lock-comment-face ((t (:foreground "lime green"))))
;;  '(font-lock-doc-face ((t (:foreground "tomato" :slant italic))))
;;  '(font-lock-function-name-face
;;     ((t (:foreground "deep sky blue" :underline t :weight bold))))
;;  '(font-lock-keyword-face ((t (:foreground "gold" :weight bold))))
;;  '(font-lock-string-face ((t (:foreground "tomato" :slant italic))))
;;  '(fringe ((nil (:background "black"))))
;;  '(highlight ((t (:background "khaki1" :foreground "black"
;;                   :box (:line-width -1 :color "firebrick1")))))
;;  '(highlight-current-line-face ((t (:inherit highlight))))
;;  '(lazy-highlight ((t (:background "paleturquoise" :foreground "black"))))
;;  '(link ((t (:foreground "DodgerBlue3" :underline t))))
;;  '(menu ((t (:background "gray2" :foreground "#FFF991"))))
;;  '(minibuffer-prompt ((t (:foreground "royal blue"))))
;;  '(mode-line ((t (:background "dark olive green"
;;                   :foreground "dark blue"
;;                   :box (:line-width -1 :color "gray75")
;;                   :weight bold))))
;;  '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
;;  '(mode-line-highlight ((((class color) (min-colors 88)) nil)))
;;  '(mode-line-inactive ((t (:background "dark olive green"
;;                            :foreground "dark khaki" :weight light))))
;;  '(mouse ((t (:background "Grey" :foreground "black"))))
;;  '(trailing-whitespace ((((class color) (background dark))
;;                           (:background "firebrick1")))))

; make sure the frames have the dark background mode by default
;;(setq default-frame-alist (quote (
;;  (frame-background-mode . dark)
;;)))
