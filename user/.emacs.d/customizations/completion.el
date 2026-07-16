;; Modern completion stack: Vertico (minibuffer UI) + Orderless (matching)
;; + Marginalia (annotations) + Consult (search/navigation commands) for
;; the minibuffer, and Corfu + Cape for in-buffer completion.

(use-package savehist
  :init (savehist-mode))

(use-package vertico
  :straight t
  :init (vertico-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight t
  :init (marginalia-mode))

(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)))

(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init (global-corfu-mode))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))
