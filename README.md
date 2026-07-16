# dotfiles

Personal dev environment: Emacs, zsh, and a modern Python/CLI toolchain.
Works on macOS and Ubuntu (Linux is written carefully but only tested via
the `Dockerfile.ubuntu` smoke test, not on a real machine - macOS is the
daily driver and is what's actually been run end-to-end).

## Install

```sh
git clone https://github.com/paidi/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./setup.sh
```

`setup.sh` does three things, and each is safe to re-run on its own:

1. **`install-macos.sh`** / **`install-linux.sh`** - installs [Homebrew](https://brew.sh)
   (used on both macOS and Linux, so one package list covers both - see
   `packages/brew.txt`). On Linux, `packages/apt.txt` lists the handful of
   system packages needed to bootstrap Homebrew in the first place.
2. **`setup-symlinks.sh`** - symlinks everything in `user/` into `$HOME`
   (e.g. `user/.zshrc` -> `~/.zshrc`). If something's already there and
   isn't already one of these symlinks, it's moved aside to `<name>.bak`
   rather than overwritten.
3. **`setup-python.sh`** - installs [uv](https://docs.astral.sh/uv/) if
   needed, then Python interpreters (`packages/python-versions.txt`) and
   global Python CLI tools (`packages/uv-tools.txt`) via `uv tool install`.

After that, open a new terminal (or `exec zsh`) and start `emacs` - the
first launch takes a minute or two while `straight.el` clones and builds
packages.

## What's here

### Shell: zsh, emacs-style navigation

- `bindkey -e` and `.inputrc`'s `editing-mode emacs` make zsh and any
  readline-based tool use Emacs keybindings (`C-a`/`C-e`/`C-k`/`M-f`/`M-b`/...).
- `WORDCHARS` is trimmed so word-motion (`M-f`/`M-b`/`M-d`) stops at `/`,
  which makes navigating paths much more precise.
- Arrow-key history substring search, and Option/Alt+arrow for word motion
  (Terminal.app/iTerm2).
- No plugin manager (dropped Antigen) - `zsh-autosuggestions`,
  `zsh-syntax-highlighting` and `zsh-history-substring-search` are
  installed via Homebrew and sourced directly from `$(brew --prefix)`,
  which works the same way on macOS and Linuxbrew.
- [starship](https://starship.rs) prompt, [zoxide](https://github.com/ajeetdsouza/zoxide)
  (`z`-style "jump to frecent directory", cd right into a fuzzy match),
  [fzf](https://github.com/junegunn/fzf) fuzzy-finder keybindings (`C-r`
  for history, `C-t` for files).
- `tmux.conf` also sets Emacs-style keys for copy-mode, 256-colour/truecolor,
  mouse support, and a longer scrollback.

### Editor: Emacs, primarily for Python

Config lives in `user/.emacs.d/`, split by concern under `customizations/`
(see `user/.emacs.d/README.md` for the file-by-file breakdown). Highlights:

- **Python**: [uv](https://docs.astral.sh/uv/) for interpreters and
  per-project `.venv`s (auto-activated on visiting a file under one via
  `pyvenv`), [Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)
  (built in to Emacs 29+) talking to `python-lsp-server`, and
  [ruff](https://docs.astral.sh/ruff/) for linting + formatting (format-on-save),
  replacing the old flake8/isort/autoflake/black/docformatter stack with
  one fast tool.
- **Completion**: Vertico/Orderless/Marginalia/Consult for the minibuffer
  (buffer switching, search, `M-x`, ...), Corfu/Cape for in-buffer
  completion. Replaces the old (mostly disabled) ido/smex setup.
- **Search**: `ag` (the_silver_searcher) wired up via `ag.el`
  (`C-c s s`/`C-c s r`/`C-c s d`), and `M-s r` for `consult-ripgrep`.
- **Git**: Magit (`C-c g`).
- **Files**: Dired with `diredfl` (colourised), GNU `ls` on macOS so
  directories sort first.
- **Editing**: paredit for Lisp, rainbow-delimiters, subword-mode,
  hippie-expand, tree-sitter major modes where a grammar is available
  (`treesit-auto`).

Removed from the previous version of this repo as no longer used: Clojure
(cider/paredit-for-clojure), JS/CoffeeScript, LaTeX/AUCTeX, R/ESS, Julia,
org-mode (had hardcoded paths from an old machine), elpy (superseded by
Eglot), and the `vendor/` Jekyll blog-authoring elisp.

### CLI tools for working with files

Installed via Homebrew (`packages/brew.txt`): `ag`/`the_silver_searcher`,
`ripgrep`, `fd`, `bat`, `eza`, `jq`, `fzf`, `tree`, plus GNU `coreutils`/
`findutils`/`sed` (so scripts behave the same as on Linux) and `moreutils`.
`csvkit` (CSV cleaning/querying/converting) is installed as a Python tool
via `uv tool install` instead, since it's a suite of Python CLI scripts -
see `packages/uv-tools.txt`.

`.zshrc` aliases `eza` in for `ls`/`ll` and `bat` in for `cat`; it
deliberately does *not* alias `fd`/`rg` over `find`/`grep` - their flags
aren't compatible, so just use them by their own names.

### Python tooling

- `uv` replaces pyenv + pipx + pip-tools/poetry/pipenv for interpreter
  management, virtualenvs, and running project commands.
- `ruff` replaces flake8 + isort + autoflake + black + pyupgrade for
  linting and formatting (`ruff check --fix` + `ruff format`).
- `python-lsp-server` (with the `python-lsp-ruff` plugin) is the language
  server Emacs talks to.
- `pre-commit`, `jupyterlab`, `cookiecutter`, `httpie`, `twine`, `git-up`,
  `argcomplete` and `gimme-aws-creds` are installed globally as `uv` tools
  (`packages/uv-tools.txt`) - add more there as needed.

## Linux notes

The Linux path (`install-linux.sh`) installs a small `apt` bootstrap list
(build tools, zsh, git - just enough to install Homebrew), then uses the
same `packages/brew.txt` as macOS via Linuxbrew. `Dockerfile.ubuntu` runs
the whole install inside `ubuntu:22.04` as a smoke test:

```sh
docker build -f Dockerfile.ubuntu -t dotfiles-ubuntu-test .
docker run -it --rm dotfiles-ubuntu-test zsh
```

This hasn't been run against a real Ubuntu machine, only reasoned through
and (when Docker is available) smoke-tested in the container above - if
something's off, that Dockerfile is the place to start debugging.
