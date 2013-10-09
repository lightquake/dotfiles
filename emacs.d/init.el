(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; t to not error if it doesn't exist
(load "~/.emacs.d/local/init" t)

;;;;; Programming-related things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'column-marker)

;; Markdown mode
(require 'markdown-mode)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; js2-mode
(require 'js2-mode)
(setq auto-mode-alist (cons '("\\.js" . js2-mode) auto-mode-alist))
(add-hook 'js2-mode-hook (lambda () (interactive) (column-marker-1 80)))
(setq auto-mode-alist (cons '("\\.json" . json-mode) auto-mode-alist))

;; yaml-mode
(require 'yaml-mode)

;; Magit stuff
(require 'magit)
(global-set-key (kbd "C-c C-g") 'magit-status)

;; Rainbows!
(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Always want markers at column 80.
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))

;; Get the right $PATH.
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Haskell stuff, too much for here.
(load "~/.emacs.d/init/haskell")


;;;;; Miscellaneous stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Random keybindings
(global-set-key (kbd "C-x k") 'kill-buffer)

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

(require 'wc-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'wc-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"])
 '(ansi-term-color-vector [unspecified "#282a2e" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#e0e0e0"])
 '(column-number-mode t)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi" "~")))
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes (quote ("de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "d05303816026cec734e26b59e72bb9e46480205e15a8a011c62536a537c29a1a" "4d66773cc6d32566eaf2c9c7ce11269d9eb26e428a1a4fa10e97bae46ff615da" default)))
 '(doc-view-continuous t)
 '(fill-column 80)
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
 '(org-agenda-skip-scheduled-if-done t)
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-log-repeat (quote time))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-reverse-note-order t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-startup-indented t)
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-separator "/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eaeaea" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Inconsolata"))))
 '(diff-added ((t (:foreground "green3"))))
 '(diff-file-header ((t (:weight bold))))
 '(diff-header ((t nil)))
 '(diff-removed ((t (:foreground "red3"))))
 '(hl-line ((t (:inherit highlight :background "#aaa"))) t)
 '(magit-item-highlight ((t (:background "dim gray"))))
 '(magit-log-graph ((t (:foreground "grey80" :weight bold))))
 '(org-hide ((t (:foreground "black"))))
 '(table-cell ((t (:foreground "peach puff"))) t)
 '(wc-goal-face ((t (:foreground "pale green")))))
(put 'downcase-region 'disabled nil)
