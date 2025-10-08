.emacs.d
========

Modernised Emacs configuration targeting Emacs 29/30. Highlights:

* Uses `use-package` with the latest GNU, NonGNU and MELPA archives.
* Ships with Vertico/Orderless/Consult for a fast, minimal completion stack.
* Enables quality-of-life defaults such as relative line numbers, project
  integration, on-the-fly linting and LSP (via `eglot` + `flycheck-eglot`).
* Ships first-class language support for Python, Go, Rust (with rust-analyzer)
  and modern C/C++ (via clangd).

### Getting started

1. Ensure you are running Emacs 29.1 or newer (native compilation is enabled
   automatically when available).
2. Clone this repository into `~/.emacs.d` (back up any existing configuration
   first).
3. Launch Emacs; the required packages will be downloaded on the first run.

The configuration keeps user customisations in `custom.el`. Feel free to edit
`init.el` to suit your workflow—the file is organised into clearly labelled
sections for easy navigation. Use `C-c C-i` inside Emacs to jump straight to the
configuration. This key chord lives in the user-reserved `C-c` control prefix
and intentionally repurposes the seldom used `C-c TAB` slot.

### FAQ

#### What replaced `ido-ubiquitous`?

`ido-ubiquitous` was renamed to [`ido-completing-read+`](https://github.com/DarwinAwardWinner/ido-completing-read-plus). This configuration opts for Vertico/Orderless/Consult instead, offering a more powerful completing-read experience out of the box.
