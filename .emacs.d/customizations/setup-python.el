(elpy-enable)
;;use IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Use python3
(setq elpy-rpc-python-command "python3")

(add-hook 'elpy-mode-hook
  (lambda()
    (add-hook 'write-contents-functions
      (lambda()
        (save-excursion
          (delete-trailing-whitespace)))
      nil t)))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (setq python-remove-cwd-from-path nil)
