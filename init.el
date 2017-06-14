;;
;; Package manager settings
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             ;; '("marmalade" . "http://marmalade-repo.org/packages/")
             )
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar installed-packages '(better-defaults
                             paredit
                             idle-highlight-mode
                             ido-ubiquitous
                             fill-column-indicator
                             find-file-in-project
                             smex
                             company
                             clojure-mode
                             scala-mode
                             python-mode
                             lua-mode
                             auctex)
  "Emacs packages to be installed if they aren't already.")

(dolist (p installed-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; We don't want custom messing with this file
(setq custom-file "~/.emacs.d/custom.el")


;;
;; Frame appearance
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(if window-system
  (scroll-bar-mode -1))
(show-paren-mode 1)
(transient-mark-mode 1)
(add-hook 'find-file-hook (lambda ()
                            (linum-mode 1)
                            (line-number-mode -1)))
(setq inhibit-startup-screen t)


;; Fullscreen mode, bound to F11
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)


;; column marker
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(setq fci-rule-width 1)
(setq fci-rule-column 80)


;; whitespace, which function, column number, deletion
(which-function-mode)
(delete-selection-mode 1)
(setq column-number-mode t)
(setq whitespace-line-column 160)


;;
;; Editing behaviour
;;
(setq-default sh-basic-offset 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;; scons
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

;; Torch
(add-to-list
 'auto-mode-alist
 '("\\.th$" . lua-mode))


;; no pretty fns
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)
