;; Dired: browsing and acting on files from Emacs

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alh --group-directories-first")
  :config
  ;; BSD ls (macOS default) doesn't support --group-directories-first;
  ;; use GNU ls from coreutils (Homebrew) if it's on PATH.
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls (setq insert-directory-program gls)))))

(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode))
