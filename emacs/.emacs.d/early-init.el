;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization
;;; Code:

;;;; Disable gc and file name handlers until startup is finished
(setf file-name-handler-alist nil)
(defun lyn-file-name-handler-restore ()
  "Restore the default file name handler."

  (setf file-name-handler-alist
        (append (car (get 'file-name-handler-alist 'standard-value))
                file-name-handler-alist)))
(add-hook 'emacs-startup-hook #'lyn-file-name-handler-restore)

(defun lyn-gc-disable ()
  "Turn off garbage collection."

  (setf gc-cons-percentage 1.0
        gc-cons-threshold most-positive-fixnum))
(defun lyn-gc-restore ()
  "Enable garbage collection."

  (setf gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value))
        gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(defun lyn-gc-finalize ()
  "Reset garbage collection and add minibuffer hooks to toggle it."

  (remove-hook 'pre-command-hook #'lyn-gc-finalize)
  (lyn-gc-restore)
  ;; Additionally disable and restore gc on minibuffer,
  ;; as amx/flx allocate a lot of memory
  (add-hook 'minibuffer-setup-hook #'lyn-gc-disable)
  (add-hook 'minibuffer-exit-hook #'lyn-gc-restore))
(lyn-gc-disable)
(add-hook 'pre-command-hook #'lyn-gc-finalize)

;;;; Disable VC mode
(remove-hook 'find-file-hook #'vc-refresh-state)

;;;; Unicode
(charset-priority-list)
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;;; package.el replacement
(setf straight-check-for-modifications '(check-on-save find-when-checking)
      straight-recipes-gnu-elpa-use-mirror t
      straight-use-package-by-default t)
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

(eval-when-compile
  (straight-use-package 'use-package))

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
;; Disable tabs almost everywhere
(setq-default indent-tabs-mode nil)

;;;; Theme
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setf show-paren-delay 0)
(setq-default cursor-type 'bar
              echo-keystrokes 0.01
              truncate-lines t)

(setq-default mode-line-format
              (cl-set-difference mode-line-format
                                 '(mode-line-front-space mode-line-mule-info mode-line-client)))

(defconst lyn-font-size 14
  "Size at which to render fonts.")
(defconst lyn-font-stack '("Triplicate T4c", "GoMono Nerd Font" "Menlo")
  "Fonts to render with, in priority order.")
(defun lyn-font-available-p (name)
  "Return NAME when NAME is available as a font. Nil otherwise."

  (car (member name (font-family-list))))
(let ((font (cl-some #'lyn-font-available-p lyn-font-stack)))
  (when font
    (push (cons 'font (concat font "-" (number-to-string lyn-font-size)))
          default-frame-alist)))

(setq frame-title-format nil
      ns-use-proxy-icon nil)
(push '(ns . ((ns-transparent-titlebar . t))) window-system-default-frame-alist)

(use-package srcery-theme
  :config (load-theme 'srcery t))

(use-package minions
  :config (minions-mode 1)
  :custom (minions-mode-line-lighter "\u2026"))

(provide 'early-init)
;;; early-init.el ends here
