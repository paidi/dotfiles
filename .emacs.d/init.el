;; Bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;  package.el
;;; so package-list-packages includes them
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode))

(use-package diminish
  :straight t)

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))

(use-package flycheck-tip
  :straight t
  :bind
  (("C-c C-n" . error-tip-cycle-dwim)
   ("C-c C-p" . error-tip-cycle-dwim-reverse))
  )

(use-package json-mode
  :straight t)


(use-package linum-off
  :straight t
  :hook (find-file . my-find-file-check-make-large-file-read-only-hook)
  :config
  (global-linum-mode 1)

  (defun my-find-file-check-make-large-file-read-only-hook ()
    "If a file is over a given size, turn off nlinum and font-lock-mode."
    (if (> (buffer-size) (* 1024 1024))
        (progn (linum-mode -1)
               (font-lock-mode -1)))))

(use-package magit
  :straight t
  :commands (magit-status magit-log))

(use-package magit-filenotify
  :straight t
  :commands (magit-filenotify-mode))

(use-package markdown-mode
  :straight t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package org
  :mode (("\\.org$" . org-mode))  
  :straight t)

(use-package org-plus-contrib
   :mode (("\\.org$" . org-mode))
   :bind
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   )

(setq
 org-directory (concat (file-name-as-directory (getenv "HOME")) "org")
 org-agenda-files (list
		   (concat (file-name-as-directory org-directory) "notes.org")
		   )
 org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org")
 )

(use-package projectile
  :straight t
  :diminish projectile-mode
  :init
  ;; this must be done before :config so we can't use :bind
  (define-key global-map (kbd "C-c p") 'projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-globally-ignored-files
        (append '("*.txt" "*.o" "*.so" "*.csv" "*.tsv" "*~" "*.orig" "*#")
                projectile-globally-ignored-files))
  )

;;; Python

(use-package python
  :hook (inferior-python-mode . fix-python-password-entry)
  :config

  (setq python-shell-interpreter "jupyter-console"
        python-shell-interpreter-args "--simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter-console")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  
  (defun fix-python-password-entry ()
    (push
     'comint-watch-for-password-prompt comint-output-filter-functions))
  
  (defun my-setup-python (orig-fun &rest args)
    "Use corresponding kernel"
    (let* ((curr-python (car (split-string (pyenv--version-name) ":")))
           (python-shell-buffer-name (concat "Python-" curr-python))
	   (python-shell-interpreter-args (if (bound-and-true-p djangonaut-mode)
					      "shell_plus -- --simple-prompt"
					    (concat "--simple-prompt --kernel=" curr-python))))
      (apply orig-fun args)))
  
  (advice-add 'python-shell-get-process-name :around #'my-setup-python)
  (advice-add 'python-shell-calculate-command :around #'my-setup-python)

  ;; Add support for auto-generation of docstrings (buftra and pyment)
  (use-package buftra
    :straight (:host github :repo "humitos/buftra.el"))
  
  (use-package py-pyment
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :config
    (setq py-pyment-options '("--output=numpydoc")))

  ;; Automatically managed imports
  (use-package py-isort
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-isort-enable-on-save)
    :config
    (setq py-isort-options '("-l=88" "-m=3" "--tc" "--fgw=0" "--ca")))

  ;; Auto-flake
  (use-package py-autoflake
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-autoflake-enable-on-save)
    :config
    (setq py-autoflake-options '("--expand-star-imports")))

  ;; Auto-formatting of docstrings
  (use-package py-docformatter
    :straight (:host github :repo "humitos/py-cmd-buffer.el")
    :hook (python-mode . py-docformatter-enable-on-save)
    :config
    (setq py-docformatter-options '("--wrap-summaries=88" "--pre-summary-newline")))

  ;; Auto-formatting with black
  (use-package blacken
    :straight t
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '88))

  ;; TODO Add pyenv
  (use-package elpy
    :straight t
    :bind
    (:map elpy-mode-map
	  ("C-M-n" . elpy-nav-forward-block)
	  ("C-M-p" . elpy-nav-backward-block))
    :hook ((elpy-mode . flycheck-mode)
	   (elpy-mode . (lambda ()
			  (set (make-local-variable 'company-backends)
			       '((elpy-company-backend :with company-yasnippet))))))
    :init
    (elpy-enable)
    :config
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
					; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
    (setq elpy-shell-echo-output nil)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-timeout 2))
  
  (use-package jupyter
    :straight t
    :hook
    (jupyter-repl-mode . (lambda ()
			   (setq company-backends '(company-capf))))
    :bind
    (:map jupyter-repl-mode-map
	  ("C-M-n" . jupyter-repl-history-next)
	  ("C-M-p" . jupyter-repl-history-previous)
	  ("M-n" . jupyter-repl-forward-cell)
	  ("M-p" . jupyter-repl-backward-cell)
	  :map jupyter-repl-interaction-mode-map
	  ("M-i" . nil)
	("C-?" . jupyter-inspect-at-point)
	)
    )
)

(use-package session
  :straight t
  :init
  (session-initialize))


(use-package subword
  :straight t
  :diminish subword-mode
  ;; need to load after diminish so it gets diminished
  :after (diminish)
  :init
  (global-subword-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (show-paren-mode 1)
  (electric-pair-mode 1))

(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

(use-package yasnippet
  :straight t
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . nil)
        ("TAB" . nil))
  :config
  (yas-global-mode))
