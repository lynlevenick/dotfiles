;;; early-init.el --- override many defaults -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization
;;; Code:

;; Disable gc and file name handlers until startup is finished
(let ((file-name-handler-alist-original file-name-handler-alist)
      (gc-cons-percentage-original gc-cons-percentage)
      (gc-cons-threshold-original gc-cons-threshold))
  (setf file-name-handler-alist nil
        gc-cons-percentage 1.0
        gc-cons-threshold (* 1024 1024 1024))
  (defun finalize-file-name-handler ()
    "Restore default file name handler."

    (setf file-name-handler-alist file-name-handler-alist-original))
  (add-hook 'emacs-startup-hook #'finalize-file-name-handler)

  (defun finalize-gc ()
    "Restore default gc."

    (setf gc-cons-percentage gc-cons-percentage-original
	  gc-cons-threshold gc-cons-threshold-original))
  (run-with-idle-timer 5 nil #'finalize-gc))

(defconst cache-directory (concat user-emacs-directory "cache/")
  "Volatile storage.")

(defun ensure-directories ()
  "Ensure essential directories exist."

  (dolist (directory `(,cache-directory))
    (unless (file-directory-p directory)
      (make-directory directory))))
(ensure-directories)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(fset #'display-startup-echo-area-message #'ignore)
(setf
 ;; Disable package.el
 package-enable-at-startup nil
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

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load (concat user-emacs-directory "theme") nil t)

(provide 'early-init)
;;; early-init.el ends here
