(require 'cl)

;; Set package archives
(require 'package)
(require 'scroll-bar)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; Load and activate emacs packages
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    ;; autocomplete
    auto-complete

    ;; CIDER - clojure development
    cider
    ac-cider

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; Syntax highlighting for CSS
    css-mode

    ;; syntax highlighting for CUDA
    cuda-mode

    ;; emacs mode for quicly browsing, filtering and editing directories
    ;; of plain text nodes
    deft

    ;;
    elpy
    flycheck
    py-autopep8
    pyvenv
    ein

    ;; Emacs Speaks Statistics
    ess

    ;; Select regions by semantic units
    expand-region

    ;; Emacs paste mode for github gists
    gist

    ;; Minor mode for running gradle from within emacs
    gradle-mode

    ;; Emacs mode for Haskell
    haskell-mode

    ;; Helm -
    helm

    ;; Fancy completion all over emacs
    ido-ubiquitous

    ;; Provides a REPL for ruby
    inf-ruby

    ;; json-mode
    json-mode

    ;; emacs major mode for editing lua
    lua-mode

    ;; Awesome emacs mode for git
    magit

    ;; Major mode for editing Markdown formatted text
    markdown-mode

    ;; Monokai theme
    monokai-theme

    ;; Trello for org-mode
    org-trello

    ;; Minor mode for structuring editing of S-mode data
    paredit

    ;; Project interaction
    projectile

    ;; Major mode for python development
    python-mode
    ipython

    ;; Python tools
    elpy
    flycheck
    py-autopep8
    python-pytest

    ;; Python notebooks
    request
    ein

    ;; Minor mode for displaying strings representing colours
    rainbow-mode

    request

    ;; Scala mode
    scala-mode

    ;; Enhanced M-x
    smex

    ;; use-package macro
    use-package

    ;; Visual feedback for changes to the buffer
    volatile-highlights

    websocket

    ;; YAML mode
    yaml-mode

    ;; zotext - for Zotero integration
    zotxt
    )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;;
;; Customizations
;;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")


;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Magit customisation
(load "setup-magit.el")

;; Org-mode
(load "setup-org-mode.el")

(load "setup-org-mode.el")

;; Language-specific
(load "setup-clojure.el")
(load "setup-python.el")
(load "setup-scala.el")
(load "setup-latex.el")

;; Colour mach parens and other structure characters to make code easy to follow
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(fci-rule-color "#20240E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files (quote ("~/Dropbox/org/work.org")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (unfill ## pytest flymake-json python-pylint docker dockerfile-mode ensime xclip yaml-mode volatile-highlights use-package smex scala-mode rainbow-mode ipython python-mode projectile paredit monokai-theme markdown-mode magit lua-mode json-mode inf-ruby ido-ubiquitous helm haskell-mode gradle-mode gist expand-region ess deft cuda-mode clojure-mode-extra-font-locking ac-cider cider auto-complete)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

;; use c-mode for OpenCL and Cuda files
(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.cu$" . cuda-mode) auto-mode-alist))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; livedown
;; (require 'livedown)

;; Jekyll
(require 'hyde)

;; Lua

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(setq-default lua-indent-level 3)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("th" . lua-mode))
(setq lua-default-application "th")

;; Clean whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Load ido-ubiquituous
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; Autocomplete mode settings
(ac-config-default)
(global-auto-complete-mode t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-fullscreen)
(scroll-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(load-theme 'monokai t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

(load-theme 'misterioso)

(setq x-select-enable-clipboard t)

;; Disable line numbers
(global-linum-mode 0)
