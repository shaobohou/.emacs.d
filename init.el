;;
;; Package manager settings
;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar installed-packages '(starter-kit
                             starter-kit-lisp
                             starter-kit-bindings
                             clojure-mode
                             ;; clojure-project-mode
                             ;; clojure-test-mode
                             clojurescript-mode
                             ;; nrepl
                             scala-mode
                             durendal
                             python-mode
                             gist
                             ess
                             auctex
                             octave-mod)
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

;; hide *nrepl-connection* and *nrepl-server* from C-x b
(setq nrepl-hide-special-buffers t)
;; stop the error buffer from popping up while working in buffers other than the REPL
(setq nrepl-popup-stacktraces nil)
;; enable error buffer popping also in the REPL
(setq nrepl-popup-stacktraces-in-repl t)
;; rainbow parentheses
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; whitespace
(setq whitespace-line-column 160)
(global-whitespace-mode 1)
;; (add-hook 'before-save-hook 'whitespace-cleanup)

(which-function-mode)

;;
;; Editing behaviour
;;

(setq-default sh-basic-offset 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session

(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))

;; no pretty fns
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)
