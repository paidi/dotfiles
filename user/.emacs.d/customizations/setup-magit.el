;; Git, from inside Emacs

(use-package magit
  :straight t
  :commands (magit-status magit-log)
  :bind ("C-c g" . magit-status))
