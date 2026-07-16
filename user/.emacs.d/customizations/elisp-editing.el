;; Editing Lisp-family code (Emacs Lisp, IELM, etc.)

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . enable-paredit-mode))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . eldoc-mode))
