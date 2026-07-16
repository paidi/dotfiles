(setq package-enable-at-startup nil)

;; Raise the GC threshold during startup for faster init; a package like
;; gcmh could tune this further at runtime, but this alone is most of
;; the win and needs no extra dependency.
(setq gc-cons-threshold (* 64 1024 1024))

;; Disable frame chrome before the first frame is created, to avoid a
;; flash of menu/tool/scroll bars that then immediately disappear.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Use UTF8 everywhere, see https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Don't ask about killing process buffers on shutdown
;; https://emacs.stackexchange.com/questions/14509/kill-process-buffer-without-confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Show filename in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; allow typing y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; enable up/down case region key shortcuts
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

