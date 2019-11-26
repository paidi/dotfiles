;; org-mode
(require 'org)
(require 'org-trello)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-latex-create-formula-image-program 'dvipng)
(setq org-default-notes-file (concat (getenv "HOME") "/Dropbox/org/work.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-agenda-files (concat (getenv "HOME") "/Dropbox/org/work.org"))

;; Persist clock (for time logging)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
