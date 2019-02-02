;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization
;;; Code:

;;;; Disable gc and file name handlers until startup is finished
(setf file-name-handler-alist nil)
(defun restore-file-name-handler ()
  "Restore default file name handler after Emacs has finished starting up."

  (setf file-name-handler-alist
        (append (car (get 'file-name-handler-alist 'standard-value))
                file-name-handler-alist)))
(add-hook 'emacs-startup-hook #'restore-file-name-handler)

(defun disable-gc ()
  "Turn off garbage collection."

  (setf gc-cons-percentage 1.0
        gc-cons-threshold most-positive-fixnum))
(defun restore-gc ()
  "Enable garbage collection."

  (setf gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value))
        gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(defun finalize-gc ()
  "Finalize garbage collection reset and add minibuffer hooks."

  (restore-gc)
  (add-hook 'minibuffer-setup-hook #'disable-gc)
  (add-hook 'minibuffer-exit-hook #'restore-gc))
(disable-gc)
(run-with-idle-timer 3 nil #'finalize-gc)

;;;; Disable VC mode
(with-eval-after-load 'vc
  (setf vc-handled-backends nil)
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state))

;;;; Unicode
(charset-priority-list)
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;;; package.el replacement
(setf straight-check-for-modifications '(check-on-save find-when-checking)
      straight-recipes-gnu-elpa-use-mirror t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el" t t)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil t))

(straight-use-package 'use-package)
(setf straight-use-package-by-default t)

(use-package no-littering)

;;;; General
(setf
 ;; Decrease work to create autoloads
 autoload-compute-prefixes nil
 ;; Don't ping random machines
 ffap-machine-p-known 'reject
 ;; Quiet init
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode #'fundamental-mode
 initial-scratch-message nil
 (symbol-function 'display-startup-echo-area-message) #'ignore
 ;; Control file creation - use version control for version control
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 ;; Customize is terrible (we won't load the file)
 custom-file (concat no-littering-etc-directory "custom.el")
 ;; Quiet byte compilation
 byte-compile-warnings '(not free-vars unresolved noruntime lexical)
 ;; Security
 gnutls-verify-error t
 tls-checktrust t
 tls-program '("gnutls-cli --x509cafile %t -p %p %h"
               "openssl s_client -connect %h:%p -no_ssl2 -no_ssl2 -ign_eof")
 ;; Broken Emacs defaults
 sentence-end-double-space nil
 ;; Broken OS behavior
 dired-use-ls-dired nil
 use-dialog-box nil)
(setq-default indent-tabs-mode nil)

;;;; Theme
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setf show-paren-delay 0)
(setq-default cursor-type 'bar
              echo-keystrokes 0.25
              truncate-lines t)

(setq-default mode-line-format
              (cl-set-difference mode-line-format
                                 '(mode-line-front-space mode-line-mule-info mode-line-client)))

(defconst lyn-font-size 14
  "Size at which to render fonts.")
(defconst lyn-font-stack '("Triplicate T4c", "GoMono Nerd Font" "Menlo")
  "Fonts to render with, in priority order.")
(defun lyn-font-available-p (name)
  "NAME when NAME is available as a font for Emacs to use. Nil otherwise."

  (car (member name (font-family-list))))
(let ((available (cl-some #'lyn-font-available-p lyn-font-stack)))
  (when available
    (push `(font . ,(concat available "-" (number-to-string lyn-font-size))) default-frame-alist)))

(push '(height . 50) default-frame-alist)
(push '(width . 100) default-frame-alist)

(setq frame-title-format nil
      ns-use-proxy-icon nil)
(push '(ns . ((ns-transparent-titlebar . t))) window-system-default-frame-alist)

(use-package srcery-theme
  :config (load-theme 'srcery t))
(use-package minions
  :config (minions-mode 1)
  :custom (minions-mode-line-lighter "\u2026"))

(defun lyn-dashboard-buffer (&optional switch)
  "Get the *dashboard* buffer, for `initial-buffer-choice' or SWITCH to it.

When called interactively, switch to the *dashboard* buffer."
  (interactive "p")

  (if (not switch)
      (get-buffer "*dashboard*")
    (switch-to-buffer "*dashboard*")
    (dashboard-refresh-buffer)))
(bind-key "C-c d" #'lyn-dashboard-buffer)
(use-package dashboard
  :config (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 10)
                     (bookmarks . 5)
                     (projects . 5)))
  (dashboard-startup-banner 'logo)
  (initial-buffer-choice #'lyn-dashboard-buffer))

(provide 'early-init)
;;; early-init.el ends here
