;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization

;;; Code:

;;;; Disable special file name handling and GC until startup is finished

(setf (get 'file-name-handler-alist 'standard-value)
      (list file-name-handler-alist)
      file-name-handler-alist nil)
(defun lyn-file-name-handler-restore ()
  "Restore the default file name handler."

  (cl-callf append file-name-handler-alist
    (car (get 'file-name-handler-alist 'standard-value))))
(add-hook 'emacs-startup-hook #'lyn-file-name-handler-restore)

(defvar lyn-gc-delayed-restore-timer nil
  "Timer tracking delay before restoring garbage collection.")

(defun lyn-gc-disable (&optional threshold)
  "Turn off garbage collection."

  (unless threshold (setf threshold (* 16 1024 1024)))
  (when lyn-gc-delayed-restore-timer
    (cancel-timer lyn-gc-delayed-restore-timer)
    (setf lyn-gc-delayed-restore-timer nil))
  (setf gc-cons-percentage 0.5
        gc-cons-threshold threshold))
(defun lyn-gc-restore ()
  "Enable garbage collection."

  (setf gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value))
        gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
  (garbage-collect))
(defun lyn-gc-restore-delayed ()
  "Schedule enabling garbage collection for idle time."

  (unless lyn-gc-delayed-restore-timer
    (setf lyn-gc-delayed-restore-timer
          (run-with-idle-timer 2 nil #'lyn-gc-restore))))

(lyn-gc-disable (* 64 1024 1024))
(lyn-gc-restore-delayed)

;; Additionally disable and restore gc on minibuffer,
;; as fuzzy matching tends to allocate a lot of memory
(add-hook 'minibuffer-setup-hook #'lyn-gc-disable)
(add-hook 'minibuffer-exit-hook #'lyn-gc-restore-delayed)

;;;; Disable VC mode

(remove-hook 'find-file-hook #'vc-refresh-state)

;;;; package.el replacement

(setf straight-check-for-modifications '(check-on-save find-when-checking)
      straight-recipes-gnu-elpa-use-mirror t
      straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil t))

(eval-when-compile
  (straight-use-package 'use-package)
  (require 'use-package)
  (setf use-package-expand-minimally t
        use-package-verbose nil))

;;;; Quiet init

(setf inhibit-startup-screen t
      inhibit-default-init t
      initial-major-mode #'fundamental-mode
      initial-scratch-message nil
      (symbol-function 'display-startup-echo-area-message) #'ignore)

;;;; Theme

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setf (default-value 'cursor-type) 'bar
      echo-keystrokes 1e-9
      (default-value 'truncate-lines) t
      (car mouse-wheel-scroll-amount) 1)

(setf (default-value 'mode-line-format)
      (cl-set-difference mode-line-format
                         '(mode-line-front-space
                           mode-line-mule-info
                           mode-line-client
                           mode-line-end-spaces)))

(use-package srcery-theme
  :config
  (load-theme 'srcery t)
  (custom-set-faces
   `(region ((t :background ,srcery-gray-4 :inverse-video nil :extend t)))
   `(vterm-color-black ((t :foreground ,srcery-black :background ,srcery-bright-black)))
   `(vterm-color-red ((t :foreground ,srcery-red :background ,srcery-bright-red)))
   `(vterm-color-green ((t :foreground ,srcery-green :background ,srcery-bright-green)))
   `(vterm-color-yellow ((t :foreground ,srcery-yellow :background ,srcery-bright-yellow)))
   `(vterm-color-blue ((t :foreground ,srcery-blue :background ,srcery-bright-blue)))
   `(vterm-color-magenta ((t :foreground ,srcery-magenta :background ,srcery-bright-magenta)))
   `(vterm-color-cyan ((t :foreground ,srcery-cyan :background ,srcery-bright-cyan)))
   `(vterm-color-white ((t :foreground ,srcery-white :background ,srcery-bright-white)))))

;; Transparent empty titlebar on NS, buffer name on others
(setf ns-use-proxy-icon nil
      frame-title-format (and (not (memq window-system '(ns mac))) "%b")
      (alist-get 'ns-transparent-titlebar
                 (alist-get 'ns window-system-default-frame-alist)) t)
