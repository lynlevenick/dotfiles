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

    (setf gc-cons-percentage gc-cons-percentage-original))
    (setf gc-cons-threshold gc-cons-threshold-original)
  (run-with-idle-timer 5 nil #'finalize-gc))

(defconst cache-directory (concat user-emacs-directory "cache/")
  "Volatile storage.")

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
(use-package delight :ensure t)

(load-relative "theme" nil t)

(defun ensure-directories ()
  "Ensure essential directories exist."

  (dolist (directory `(,cache-directory))
    (unless (file-directory-p directory)
      (make-directory directory))))

(defun ensure-wrap (&optional unwrap)
  "Wrap Emacs.app to have a proper PATH. With UNWRAP, unwrap it."

  (let ((app-directory (cond ((file-exists-p "/Applications/Emacs.app")
                              "/Applications/Emacs.app")
                             ((file-exists-p "~/Applications/Emacs.app")
                              "~/Applications/Emacs.app"))))
    (unless app-directory
      (user-error "Could not find Emacs.app"))
    (let ((old-binary (expand-file-name "Contents/MacOS/Emacs" app-directory))
          (new-binary (expand-file-name "Contents/MacOS/RunEmacs" app-directory)))
      (cond (unwrap
             (unless (file-exists-p new-binary)
               (user-error "Emacs is not patched"))
             (copy-file new-binary old-binary t nil t t)
             (unless (file-exists-p old-binary)
               (error "Copy failed"))
             (delete-file new-binary))
            ((and (not (file-exists-p new-binary))
		  (y-or-n-p "Emacs needs to be patched. Patch? "))
             (copy-file old-binary new-binary nil nil t t)
             (unless (file-exists-p new-binary)
               (error "Copy failed"))
             (with-temp-buffer
               (insert "#!/usr/bin/env sh\n"
                       "args=\"$@\"\n"
                       (concat "exec \"${SHELL}\" -c \"'"
			       new-binary
			       "' ${args}\""))
               (write-file old-binary))
             (chmod old-binary (file-modes new-binary)))))))

(ensure-directories)
(ensure-wrap)

(provide 'early-init)
;;; early-init.el ends here
