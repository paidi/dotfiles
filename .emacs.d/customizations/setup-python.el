(elpy-enable)
;;use IPython
;; (elpy-use-ipython)

(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=120"))
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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
