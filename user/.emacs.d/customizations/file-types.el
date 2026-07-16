;; Major modes for common non-Python file types

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")

(use-package json-mode
  :straight t
  :mode "\\.json\\'")

;; Emacs 29+ tree-sitter major modes, used automatically in place of the
;; above when a grammar is available (falls back gracefully otherwise).
(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))
