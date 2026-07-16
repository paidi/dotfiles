;; Searching across files with `ag' (the_silver_searcher), the same tool
;; used from the shell.

(use-package ag
  :straight t
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :bind
  (("C-c s s" . ag-project)
   ("C-c s r" . ag-project-regexp)
   ("C-c s d" . ag)))
