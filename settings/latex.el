;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURATIONS FOR LATEX WITH AUCTEX
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Start in server mode
;(server-start)
(load "server")
(unless (server-running-p) (server-start))

;;; AUCTeX
(load "auctex.el" nil t t)

;;; Add short cut to start flyspell-mode
(global-set-key [f2] 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;; Add short cuts to chagne language of flyspell-mode
(global-set-key [f3]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "en_GB")))
(global-set-key [f4]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "de_DE")))
(global-set-key [f7]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "de_DE-neu")))
(global-set-key [f8]
 (lambda ()
   (interactive)
   (ispell-word)))

;;; using eps with pdflatex
(eval-after-load "tex" 
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
    )
  )

;;; ESTABLISH WORKING OF FORWARD AND INVERSE SEARCH

;;; Use pdflatex instead of latex to compile .tex files
(setq TeX-PDF-mode t) 

;;; compile using source specials, according 2010 manual
;;(setq LaTeX-command "latex -synctex=1")

;;; Tex Source Correlate Method
(setq TeX-source-correlate-method "synctex")

;;; Tex Source Correlate Start Server
(setq TeX-source-correlate-start-server "always")

;;; Toggle Tex Source Correlate Mode
(setq TeX-source-correlate-mode t)

;;; Next to commands are set by entries created by
;;; emacs->customization->save, see below

;;; Tex View Program List
;;(setq TeX-view-program-list '(("Okular" "okular --unique %o#src:%n`pwd`/./%b")))

;;; Tex View Program Selection
;(setq TeX-view-program-selection '((output-pdf "Okular")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; make system copy work with Emacs paste and Emacs copy work with system paste
(setq x-select-enable-clipboard t)

;;; change spell checking in auctex
(if (file-exists-p "/usr/bin/hunspell")
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

;;; When an error is found after compiling and C-c ` is invoked to jump
;;; to the error in the tex file an empty buffer opens. Adding the
;;; following should solve the problem in most cases
;;Strange behaviour debugging a document with AUCTeX-GNUEmacs in math mode
(defadvice TeX-parse-reset (after make-master-file-default () activate)
  (push (concat (substring (buffer-name) 1 (- (length (buffer-name)) 8))
                "." TeX-default-extension) TeX-error-file)
  (push nil TeX-error-offset))

;;; \frac{}{} short cut for auctex
(defun insert-frac ()
  "We insert  \\frac{}{} and position point before the first right brace."
  (interactive)
  (progn
    (insert "\\frac{}{}")
    (backward-char)
    (backward-char)
    (backward-char)))
(global-set-key "\C-cf"   'insert-frac)
