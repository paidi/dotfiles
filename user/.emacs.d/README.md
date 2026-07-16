# Emacs config

Package management is via [straight.el](https://github.com/radian-software/straight.el)
+ `use-package`. Configuration is split by concern under `customizations/`
and loaded automatically by `init.el` (see that file for the load order).

| File | Covers |
|---|---|
| `completion.el` | Vertico/Orderless/Marginalia/Consult (minibuffer), Corfu/Cape (in-buffer) |
| `dired.el` | File browsing (`diredfl` for colour) |
| `editing.el` | General editing: backups, hippie-expand, isearch, rainbow-delimiters |
| `elisp-editing.el` | Paredit + eldoc for Emacs Lisp |
| `file-types.el` | markdown/yaml/json modes, tree-sitter auto-remap |
| `misc.el` | Small one-off tweaks |
| `navigation.el` | Buffers, recentf, uniquify, matching-paren |
| `search.el` | `ag` (the_silver_searcher) integration |
| `setup-magit.el` | Magit (`C-c g`) |
| `setup-python.el` | uv/pyvenv + Eglot + pylsp + ruff |
| `ui.el` | Look and feel |

First launch will take a minute or two while straight.el clones and builds
every package. `pylsp` (from `python-lsp-server`) needs to be on `PATH` for
Python support - installed by `../../setup-python.sh`.
