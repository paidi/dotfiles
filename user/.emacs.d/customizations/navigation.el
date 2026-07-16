;; File/buffer navigation. Project- and file-search navigation lives in
;; completion.el (Consult) and search.el (ag); this file covers the rest.

;; When several buffers visit identically-named files, disambiguate using
;; part of the directory path rather than "<2>", "<3>", etc.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Recently opened files
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 200)

;; Buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Built-in project.el powers C-x p ... (find file, grep, switch project,
;; compile, etc.) - no extra package needed.

;; Jump to the matching parenthesis
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-M-#") 'goto-match-paren)
