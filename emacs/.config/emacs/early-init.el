;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization

;;; Code:

;;;; Set comp speed options

(setf comp-speed 2)

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

(defun lyn-gc-disable ()
  "Turn off garbage collection."

  (when lyn-gc-delayed-restore-timer
    (cancel-timer lyn-gc-delayed-restore-timer)
    (setf lyn-gc-delayed-restore-timer nil))
  (setf gc-cons-percentage 1.0
        gc-cons-threshold most-positive-fixnum))
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

(lyn-gc-disable)
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

;;;; Unicode

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setf (default-value 'buffer-file-coding-system) 'utf-8-unix)

(use-package no-littering)

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
      (default-value 'echo-keystrokes) 1e-6
      (default-value 'truncate-lines) t
      (car mouse-wheel-scroll-amount) 1)

(setf (default-value 'mode-line-format)
      (cl-set-difference mode-line-format
                         '(mode-line-front-space
                           mode-line-mule-info
                           mode-line-client
                           mode-line-end-spaces)))

(use-package m :straight (:host github :repo "lynlevenick/emacs-m"))

(use-package micromap :straight (:host github :repo "lynlevenick/emacs-micromap")
  :config (micromap-mode)
  :custom
  (micromap-foreground "#FBB829")
  (micromap-background "#444444"))

(use-package minions :straight (:host github :repo "lynlevenick/minions" :branch "remove-lambda-allocation")
  :config (minions-mode)
  :custom (minions-mode-line-lighter "\u2026")) ; Horizontal ellipsis

(use-package srcery-theme
  :config (load-theme 'srcery t))

;; Transparent empty titlebar on NS, buffer name on others
(setf ns-use-proxy-icon nil
      frame-title-format nil
      (alist-get 'ns-transparent-titlebar
                 (alist-get 'ns window-system-default-frame-alist)) t)
