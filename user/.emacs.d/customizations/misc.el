;; Small standalone tweaks that don't warrant their own file

;; Allow typing y/n instead of yes/no
(setq use-short-answers t) ; Emacs 28+; falls back harmlessly otherwise

;; Shell scripts: 2-space indent
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Skip the startup screen
(setq inhibit-startup-message t)

;; Make scripts executable automatically when saved, if they have a shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
