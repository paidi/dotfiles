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

(use-package f
  :straight t)

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

  (defun activate-local-venv ()
    "Activate local python version, if it exists"
    (f-traverse-upwards
     (lambda (path)
       (let ((python-version-path (f-expand ".python-version" path)))
	 (if (f-exists? python-version-path)
	     (pyvenv-workon (s-trim (f-read-text python-version-path))))
	 )
       )
     )
    )
  
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
    (setq py-docformatter-options '("--wrap-summaries=100" "--pre-summary-newline")))

  ;; Auto-formatting with black
  (use-package blacken
    :straight t
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length '100))

  (use-package pyenv-mode
    :straight t
    :init
    (add-to-list 'exec-path "~/.pyenv/shims")    
    (setenv "WORKON_HOME" "~/.pyenv/versions/")
    :config
    (pyenv-mode)
    (add-hook 'find-file-hook 'activate-local-venv)
    )
  
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
    ; fix for MacOS, see https://github.com/jorgenschaefer/elpy/issues/1550
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (setq elpy-shell-echo-output nil)
    (setq elpy-rpc-python-command "python3")
    (setq elpy-rpc-timeout 2)
    (setq python-indent-offset 3)
    )
  
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

;;; R

(use-package ess-site
  :straight ess
  :config
  ;; Execute screen options after initialize process
  (add-hook 'ess-post-run-hook 'ess-execute-screen-options)

  ;; Disable IDO so helm is used instead
  (setq ess-use-ido nil)

  ;; We don’t want R evaluation to hang the editor, hence
  (setq ess-eval-visibly 'nowait)

  ;; Unbind ess-insert-assign (defaut value is "_")
  (setq ess-smart-S-assign-key nil))


(use-package ess-r-mode
  :straight ess
  :config
  ;; Hot key C-S-m for pipe operator in ESS
  (defun pipe_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1))

  ;; ESS syntax highlight
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators . t)
	  (ess-fl-keyword:delimiters . t)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . t)
	  (ess-R-fl-keyword:%op% . t)))

  (setq inferior-ess-r-font-lock-keywords
	'((ess-S-fl-keyword:prompt . t)
	  (ess-R-fl-keyword:messages . t)
	  (ess-R-fl-keyword:modifiers . nil)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:keywords . nil)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-fl-keyword:matrix-labels . t)
	  (ess-fl-keyword:fun-calls . nil)
	  (ess-fl-keyword:numbers . nil)
	  (ess-fl-keyword:operators . nil)
	  (ess-fl-keyword:delimiters . nil)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . nil)))

  :bind
  (:map ess-r-mode-map
	("M--" . ess-insert-assign)
	("C-S-m" . pipe_R_operator)
	:map
	inferior-ess-r-mode-map
	("M--" . ess-insert-assign)
	("C-S-m" . pipe_R_operator))
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

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)


;; Julia
(straight-override-recipe
 '(julia-mode :type git :host github :repo "non-Jedi/julia-emacs" :branch "devel"))
(straight-use-package 'julia-mode)
(add-hook 'julia-mode-hook #'(lambda () (abbrev-mode 1)))

;; (straight-use-package
;;  '(eglot-jl :type git :host github :repo "non-Jedi/eglot-jl" :files ("*.el" "*.jl" "*.toml")))

;; (with-eval-after-load 'eglot
;;     (eglot-jl-init))

;; ;; Disable line numbers
;; (global-linum-mode 0)
