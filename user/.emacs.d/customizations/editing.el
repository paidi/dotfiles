;; General editing behaviour

(setq-default indent-tabs-mode nil)
(setq-default fill-column 88)

;; Remember point position in visited files
(use-package saveplace
  :init (save-place-mode 1))

;; Keep backups in one place instead of scattering `foo~' everywhere, and
;; skip auto-save/lockfiles entirely
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Hippie-expand: a more powerful dabbrev-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlight matching parenthesis
(show-paren-mode 1)
(electric-pair-mode 1)

;; Comment/uncomment the current line if no region is active
(defun comment-or-uncomment-region-or-line ()
  "Comment or uncomment the current region, or the current line if no
region is active."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c C-r") 'comment-or-uncomment-region-or-line)

;; Swap isearch to regexp-by-default; plain isearch moves to C-M-s/C-M-r
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; camelCase/snake_case-aware word motion (M-f, M-b, etc.)
(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; The opposite of M-q (fill-paragraph): join a paragraph into one line
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-Q") 'unfill-paragraph)
