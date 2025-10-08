;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A lightweight, batteries-included configuration that embraces the
;; modern Emacs 29/30 ecosystem.  The goal is to provide sensible
;; defaults for day-to-day editing while keeping the file approachable
;; for future tweaks.

;;; Code:

;; -------------------------------------------------------------------
;; Package manager setup
;; -------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(setq package-native-compile t)

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t)

;; Keep customisations out of this file and load them if present.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; -------------------------------------------------------------------
;; Core UI/UX tweaks
;; -------------------------------------------------------------------

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function #'ignore
      native-comp-async-report-warnings-errors 'silent)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(global-hl-line-mode 1)
(column-number-mode 1)
(which-function-mode 1)
(display-time-mode 1)
(setq display-time-default-load-average nil)

(setq display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(defun disable-display-line-numbers ()
  "Turn off `display-line-numbers-mode'."
  (display-line-numbers-mode 0))
(dolist (mode '(eshell-mode-hook shell-mode-hook term-mode-hook))
  (add-hook mode #'disable-display-line-numbers))
(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'disable-display-line-numbers))

(show-paren-mode 1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(save-place-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)

(setq-default fill-column 80
              sentence-end-double-space nil)
(when (fboundp 'display-fill-column-indicator-mode)
  (setq-default display-fill-column-indicator-column 80)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'display-fill-column-indicator-mode)))

;; -------------------------------------------------------------------
;; Editing behaviour
;; -------------------------------------------------------------------

(setq-default indent-tabs-mode nil
              tab-width 4
              sh-basic-offset 4
              c-basic-offset 4
              python-indent-offset 4
              create-lockfiles nil
              make-backup-files nil)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(make-directory (expand-file-name "auto-save" user-emacs-directory) t)
(make-directory (expand-file-name "backups" user-emacs-directory) t)

(setq x-select-enable-clipboard t
      kill-do-not-save-duplicates t
      vc-follow-symlinks t)

;; Recognise additional file types.
(add-to-list 'auto-mode-alist '("SConstruct"  . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))
(add-to-list 'auto-mode-alist '("\\.th\\'"     . lua-mode))

;; -------------------------------------------------------------------
;; Packages
;; -------------------------------------------------------------------

(use-package better-defaults)

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.3)
  (which-key-max-display-columns 5))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ([remap switch-to-buffer] . consult-buffer)))

(use-package embark
  :bind
  ("C-;" . embark-act)
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.05)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations t))

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode       . paredit-mode)
         (clojure-mode    . paredit-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-shell-interpreter "python3"))

(use-package clojure-mode)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package auctex)

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc"))

(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'"   . c-mode)
         ("\\.h\\'"   . c-or-c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cc\\'"  . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode))
  :custom
  (c-default-style '((c-mode    . "linux")
                     (c++-mode  . "llvm")
                     (java-mode . "java")
                     (other     . "gnu"))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . (lambda ()
                       (setq-local fill-column 100)))
  :custom
  (rust-format-on-save t))

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (go-mode     . eglot-ensure))
  :config
  (dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook
                  rust-mode-hook rust-ts-mode-hook))
    (add-hook hook #'eglot-ensure))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) "clangd") t)
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) "rust-analyzer") t))

(use-package exec-path-from-shell
  :if (memq system-type '(darwin gnu/linux))
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "CARGO_HOME" "RUSTUP_HOME")))

;; Project-aware navigation with built-in project.el.
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-eshell "Eshell"))))

;; -------------------------------------------------------------------
;; Quality-of-life commands
;; -------------------------------------------------------------------

(defun toggle-fullscreen (&optional frame)
  "Toggle fullscreen for FRAME.
If FRAME is nil use the currently selected frame.  Preserve the
previous frame state so we can restore it when toggling back."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (current (frame-parameter frame 'fullscreen)))
    (set-frame-parameter
     frame 'fullscreen
     (if (eq current 'fullboth)
         (prog1 (or (frame-parameter frame 'toggle-fullscreen--previous) nil)
           (set-frame-parameter frame 'toggle-fullscreen--previous nil))
       (set-frame-parameter frame 'toggle-fullscreen--previous current)
       'fullboth))))
(global-set-key (kbd "<f11>") #'toggle-fullscreen)

;; Provide a simple way to jump back to this configuration file.
(defun open-init-file ()
  "Open this init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))
(global-set-key (kbd "C-c C-i") #'open-init-file)

;; -------------------------------------------------------------------
;; Final setup
;; -------------------------------------------------------------------

(message "Emacs is ready — happy hacking!")

;;; init.el ends here
