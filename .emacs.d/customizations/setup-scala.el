(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package ensime
  :ensure t
  :pin melpa-stable)
