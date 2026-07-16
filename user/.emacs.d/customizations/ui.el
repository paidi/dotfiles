;; Look and feel
;; (menu bar / tool bar / scroll bar are disabled in early-init.el, before
;; the first frame is created)

;; Line numbers in the margin (display-line-numbers-mode is the modern,
;; much faster replacement for the old linum-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; Slightly larger default font for readability
(set-face-attribute 'default nil :height 140)

(setq
 ;; Make killing/yanking interact with the system clipboard
 select-enable-clipboard t
 select-enable-primary t
 ;; Save clipboard strings into the kill ring before they're overwritten
 save-interprogram-paste-before-kill t
 ;; Full path in the frame title
 frame-title-format "%b (%f)"
 ;; No audible/visible bell
 ring-bell-function 'ignore)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)
