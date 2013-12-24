;;; init.el --- Load point for emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; I really need to org-ify and clean this up.

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(require 'package)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; start server for emacsclient
(server-start)

;;;;; Programming-related things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add all directories in the cask elpa directory to the elisp load-path.  This
;; is necessary because some things, like dash, aren't on the normal load-path.

;; defvar is OK here because the defvar in flycheck will override the docstring
;; but not the value.
(defvar flycheck-emacs-lisp-load-path load-path)
(let ((elpa-directory (expand-file-name
                       (format "~/.emacs.d/.cask/%s/elpa/" emacs-version))))
  (mapc (lambda (dir) (add-to-list 'flycheck-emacs-lisp-load-path dir))
        ;; All the packages have the compilation date at the end; this filters
        ;; out things like ., .., and the archives.
        (directory-files elpa-directory t "[0-9]+$")))


(add-hook 'prog-mode-hook (lambda () (column-marker-1 80)))

;; Markdown mode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; js2-mode
(setq auto-mode-alist (cons '("\\.js" . js2-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.json" . json-mode) auto-mode-alist))

;; Magit stuff
(global-set-key (kbd "C-c C-g") 'magit-status)


;; Lisp/elisp type things.
(defun kata/add-lisp-hook (hook)
  "Add HOOK to both Lisp mode and Emacs Lisp mode hooks."
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook))

(kata/add-lisp-hook 'rainbow-delimiters-mode)
(kata/add-lisp-hook 'paredit-mode)
(kata/add-lisp-hook 'flycheck-mode)

;; Get the right $PATH.
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Haskell stuff, too much for here.
(load "~/.emacs.d/haskell")

(require 'mmm-auto)

;; nginx mode.

(defvar nginx-font-lock-keywords)
(defun kata/clean-font-lock-face (face)
  "Replace an old font-lock face FACE with its new equivalent."
  (if (eq face 'font-lock-pseudo-keyword-face) 'font-lock-keyword-face face))
(defun kata/clean-font-lock-keywords (keywords)
  "Replace all old font-lock faces in KEYWORDS with the new equivalents."
  (mapc (lambda (keyword)
          (setcdr keyword (if (symbolp (cdr keyword))
                              (kata/clean-font-lock-face (cdr keyword))
                            (mapcar 'kata/clean-font-lock-face (cdr keyword)))))
        keywords))
(add-hook 'nginx-mode-hook
          (lambda () (kata/clean-font-lock-keywords nginx-font-lock-keywords)))
(add-to-list 'auto-mode-alist '("nginx/sites-\\(available\\|enabled\\)/" .
                                nginx-mode))
(add-to-list 'auto-mode-alist '("nginx/conf.d/" .
                                nginx-mode))

;; smart mode line.
(sml/setup)


;;;;; Miscellaneous stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tahoe stuff
(defvar tahoe-serve/prefix "https://serve.aleph-null.io/")

(defun tahoe-serve/make-sentinel (tempfile filename)
  "Create a tahoe process sentinel function.

When the tahoe put process is done uploading the file TEMPFILE
into a file named FILENAME, it will delete TEMPFILE and show a
message stating the upload is finished."
  (lambda (proc _)
    (when (eq (process-status proc) 'exit)
      (with-current-buffer "*tahoe-serve*"
        (let* ((uri (replace-regexp-in-string "\n$" "" (thing-at-point 'line)))
               (url (concat tahoe-serve/prefix uri)))
          (kill-new url)
          (message (format "%s is uploaded at URL %s" filename url))
          (delete-file tempfile))))))

(defun tahoe-serve/put-region (start end filename)
  "Upload the region from START to END using FILENAME as the name."
  (interactive "r\nsFilename: ")
  ;; For some reason running tahoe put - paste/filename and using
  ;; process-send-region doesn't work sometimes.
  (let* ((tempfile (make-temp-file "tahoe-put"))
         (proc (progn
                 (append-to-file start end tempfile)
                 (start-process "tahoe serve" "*tahoe-serve*"
                                "tahoe" "put" tempfile
                                (concat "paste/" filename)))))
    (set-process-sentinel proc (tahoe-serve/make-sentinel tempfile filename))))

(defun tahoe-serve/put (filename)
  "Upload the current buffer using FILENAME as the name."
  (interactive
   (let ((buffer-basename (and (buffer-file-name)
                               (file-name-nondirectory (buffer-file-name)))))
     (list (read-string
            (if buffer-basename
                (format "Filename (default %s): " buffer-basename)
              "Filename: ")
            nil nil buffer-basename))))
  (if (string= filename "")
      (message "Must supply filename")
    (tahoe-serve/put-region (point-min) (point-max) filename)))

;; Random keybindings.
(global-set-key (kbd "C-x k") 'kill-buffer)

;; Windmove. Not all platforms are going to have hyper, but that's fine.
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(windmove-default-keybindings 'hyper)


;; I like auto-indent on newline, dammit.
(global-set-key "
" (quote newline-and-indent))
(global-set-key "\C-j" (quote newline-and-indent))

;; Uniquify.
(require 'uniquify)

;; Ido.
(require 'ido)
(ido-mode t)

;; Trim whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; No seriously, I want UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'markdown-mode-hook 'visual-line-mode)

(defvar markdown-mode-font-lock-keywords-basic)
(defun kata/patch-markdown-font-lock-keywords ()
  "Fix markdown mode's font lock keywords.

Edits the markdown font lock keywords so fenced code blocks are
highlighted before anything else; this prevents issues when
fenced code blocks whitespace-indented blocks."
  (setq markdown-mode-font-lock-keywords-basic
        (cons (cons 'markdown-match-fenced-code-blocks '((0 markdown-pre-face)))
              markdown-mode-font-lock-keywords-basic)))
(add-hook 'markdown-mode-hook 'kata/patch-markdown-font-lock-keywords)

(require 'undo-tree)
(global-undo-tree-mode)

(menu-bar-mode -1)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"])
 '(ansi-term-color-vector [unspecified "#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"])
 '(backup-directory-alist (quote (("." . "~/.emacs.d/.backups"))))
 '(column-number-mode t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" "~")))
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes (quote ("de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "d05303816026cec734e26b59e72bb9e46480205e15a8a011c62536a537c29a1a" "4d66773cc6d32566eaf2c9c7ce11269d9eb26e428a1a4fa10e97bae46ff615da" default)))
 '(doc-view-continuous t)
 '(fill-column 80)
 '(flycheck-disabled-checkers (quote (haskell-ghc)))
 '(flycheck-highlighting-mode (quote lines))
 '(git-commit-max-summary-line-length 50)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-indentation-ifte-offset 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-global-externs (quote ("$" "_" "require" "define" "console")))
 '(mmm-global-mode (quote maybe) nil (mmm-mode))
 '(org-agenda-files (quote ("/amateurtopologist.com:/home/kata/org/habit.org" "/amateurtopologist.com:/home/kata/org/main.org")))
 '(org-agenda-skip-deadline-prewarning-if-scheduled 2)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-bullets-bullet-list (quote ("◉" "○" "◎")))
 '(org-columns-default-format "%25ITEM %SCHEDULED %TAGS")
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-log-repeat (quote time))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-reverse-note-order t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-startup-indented t)
 '(org-tags-column -75)
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(sml/hidden-modes (quote (" Undo-Tree" " Ind" " hl-p")))
 '(sml/replacer-regexp-list (quote (("^~/org/" ":Org:") ("^~/code/dotfiles/emacs\\.d/" ":ED:") ("^~/\\.emacs\\.d/" ":ED:") ("^/sudo:.*:" ":SU:") ("^~/Documents/" ":Doc:") ("^~/Dropbox" ":DB:") ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:") ("^/scpc:amateurtopologist.com:" ":neo-exdeath:"))))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator "/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "green3"))) t)
 '(diff-file-header ((t (:weight bold))) t)
 '(diff-header ((t nil)) t)
 '(diff-removed ((t (:foreground "red3"))) t)
 '(hl-line ((t (:inherit highlight :background "#aaa"))) t)
 '(magit-item-highlight ((t (:background "dim gray"))) t)
 '(magit-log-graph ((t (:foreground "grey80" :weight bold))) t)
 '(org-agenda-structure ((t (:foreground "light gray"))) t)
 '(org-done ((t (:foreground "#00ff00" :weight bold))) t)
 '(org-habit-alert-face ((t (:background "gold" :foreground "black"))) t)
 '(org-hide ((t (:foreground "black"))) t)
 '(org-scheduled ((t (:foreground "medium turquoise"))) t)
 '(org-scheduled-previously ((t (:foreground "yellow"))) t)
 '(org-scheduled-today ((t (:foreground "medium turquoise"))) t)
 '(table-cell ((t (:foreground "peach puff"))) t)
 '(wc-goal-face ((t (:foreground "pale green"))) t))
(put 'downcase-region 'disabled nil)


;; t to not error if it doesn't exist
(load "~/.emacs.d/local" :noerror)


;;; init.el ends here
