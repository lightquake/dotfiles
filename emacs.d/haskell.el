;;; haskell.el -- Haskell-specific Emacs customization

;;; Commentary:

;;; Code:

(require 'fic-mode)
(add-hook 'haskell-mode-hook 'fic-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(require 'column-marker)
(add-hook 'haskell-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'haskell-mode-hook 'haskell-bindings-hook)

;; Haskell mode keybindings.
(defun haskell-bindings-hook ()
  "Set up haskell-mode bindings."
  ;; Load the file into the REPL.
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  ;; Switch to the REPL.
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  ;; Build.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-navigate-imports))

(eval-after-load 'flycheck
  '(require 'flycheck-hdevtools))

(require 'mmm-vars)
(mmm-add-classes
 '((hamlet-quasiquote
    :submode hamlet-mode
    :delimiter-mode nil
    :front "\\[x?hamlet|"
    :back "|\\]")))
(mmm-add-mode-ext-class 'haskell-mode nil 'hamlet-quasiquote)

;;; haskell.el ends here
