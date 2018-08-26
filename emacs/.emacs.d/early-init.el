;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization
;;; Code:

;;;; Disable gc and file name handlers until startup is finished
(let ((file-name-handler-alist-original file-name-handler-alist)
      (gc-cons-percentage-original gc-cons-percentage)
      (gc-cons-threshold-original gc-cons-threshold))
  (setf file-name-handler-alist nil
        gc-cons-percentage 1.0
        gc-cons-threshold most-positive-fixnum)
  (defun finalize-file-name-handler ()
    "Restore default file name handler."

    (setf file-name-handler-alist file-name-handler-alist-original))
  (add-hook 'emacs-startup-hook #'finalize-file-name-handler)

  (defun finalize-gc ()
    "Restore default gc."

    (setf gc-cons-percentage gc-cons-percentage-original
	  gc-cons-threshold gc-cons-threshold-original))
  (run-with-idle-timer 3 nil #'finalize-gc))

;;;; Decrease littering
(defconst cache-directory (concat user-emacs-directory "cache/")
  "Volatile storage.")

(defun ensure-directories ()
  "Ensure essential directories exist."

  (dolist (directory `(,cache-directory))
    (unless (file-directory-p directory)
      (make-directory directory))))
(ensure-directories)

;;;; Unicode
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;;;; General
(fset #'display-startup-echo-area-message #'ignore)
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
 ;; Control file creation
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 auto-save-list-file-name (concat cache-directory "auto-save")
 backup-directory-alist `((".*" . ,(concat cache-directory "backup")))
 tramp-auto-save-directory (concat cache-directory "tramp-auto-save")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name (concat cache-directory "tramp-persistency")
 ;; Customize is terrible
 custom-file (concat cache-directory "custom.el")
 ;; Quiet byte compilation
 byte-compile-warnings '(not free-vars unresolved noruntime lexical)
 ;; Security
 gnutls-verify-error t
 tls-checktrust t
 tls-program '("gnutls-cli --x509cafile %t -p %p %h"
               "openssl s_client -connect %h:%p -no_ssl2 -no_ssl2 -ign_eof")
 ;; Broken Emacs defaults
 sentence-end-double-space nil
 ;; Broken OS defaults
 dired-use-ls-dired nil)

;;;; package.el replacement
(setf straight-check-for-modifications '(check-on-save find-when-checking))
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
(defmacro def-package! (recipe-or-name &rest body)
  "Call `straight-use-package' followed by `use-package'.

If RECIPE-OR-NAME is a list, it is assumed to be a `straight-use-package'
recipe. Otherwise, it is assumed to be a `use-package' name. BODY is passed
through to `use-package'."
  (declare (indent defun))

  (cond ((listp recipe-or-name)
	 `(progn (straight-use-package ,recipe-or-name)
		 (use-package ,(car recipe-or-name) . ,body)))
	(`(progn (straight-use-package ',recipe-or-name)
		 (use-package ,recipe-or-name . ,body)))))

;;;; Theme
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setf frame-title-format nil
      show-paren-delay 0
      ns-use-proxy-icon nil)
(setq-default cursor-type 'bar
	      display-line-numbers-grow-only t
              display-line-numbers-type 'relative
	      echo-keystrokes 0.25
              truncate-lines t)
(push '(font . "Menlo-12") default-frame-alist)
(push '(height . 25) default-frame-alist)
(push '(width . 80) default-frame-alist)
(push '(ns . ((ns-transparent-titlebar . t))) window-system-default-frame-alist)

(def-package! doom-themes
  :demand
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))
(def-package! solaire-mode
  :demand
  :hook (((after-revert change-major-mode ediff-prepare-buffer) . turn-on-solaire-mode)
	 (minibuffer-setup . solaire-mode-in-minibuffer))
  :config (solaire-mode-swap-bg))
(straight-use-package 'smart-mode-line)
(def-package! smart-mode-line
  :demand
  :custom
  (sml/replacer-regexp-list nil)
  (sml/theme nil)
  :config (sml/setup))

(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)

(provide 'early-init)
;;; early-init.el ends here
