;; Python development.
;;
;; Toolchain (installed by ../../../setup-python.sh via `uv`):
;;   - uv               Python versions + per-project .venv
;;   - python-lsp-server (pylsp) with the python-lsp-ruff plugin
;;       -> completion, go-to-def, docs (via Eglot/Flymake)
;;       -> linting + formatting (via ruff, replacing flake8/isort/black)

(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  :config
  (dolist (interp '("jupyter-console" "jupyter"))
    (add-to-list 'python-shell-completion-native-disabled-interpreters interp))

  ;; ipython sometimes prompts for a password (e.g. when debugging over
  ;; a pty); let comint answer that prompt without echoing it back.
  (defun dotfiles--python-shell-watch-password ()
    (push 'comint-watch-for-password-prompt comint-output-filter-functions))
  (add-hook 'inferior-python-mode-hook #'dotfiles--python-shell-watch-password))

;; Auto-activate a project's virtualenv (`.venv/`, as created by `uv venv`
;; or `uv sync`) when visiting a file inside it.
(use-package pyvenv
  :straight t
  :config
  (defun dotfiles--activate-project-venv ()
    (when-let* ((root (locate-dominating-file default-directory ".venv")))
      (pyvenv-activate (expand-file-name ".venv" root))))
  (add-hook 'python-base-mode-hook #'dotfiles--activate-project-venv))

;; LSP support (completion, go-to-definition, diagnostics) via Eglot,
;; built into Emacs 29+, talking to python-lsp-server.
(use-package eglot
  :hook (python-base-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))
  ;; Route linting/formatting through ruff instead of pylsp's slower,
  ;; older built-in checkers/formatters.
  (setq-default
   eglot-workspace-configuration
   '(:pylsp (:plugins (:pycodestyle (:enabled :json-false)
                        :pyflakes (:enabled :json-false)
                        :mccabe (:enabled :json-false)
                        :yapf (:enabled :json-false)
                        :autopep8 (:enabled :json-false)
                        :ruff (:enabled t :formatEnabled t)))))
  ;; Format (and apply ruff's safe autofixes) on save in any Eglot-managed
  ;; buffer.
  (add-hook 'eglot-managed-mode-hook
            (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
