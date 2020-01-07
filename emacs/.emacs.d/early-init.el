;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization
;;; Code:

;;;; Disable special file name handling and GC until startup is finished

(setf (get 'file-name-handler-alist 'standard-value) (list file-name-handler-alist)
      file-name-handler-alist nil)
(defun lyn-file-name-handler-restore ()
  "Restore the default file name handler."

  (setf file-name-handler-alist
        (append (car (get 'file-name-handler-alist 'standard-value))
                file-name-handler-alist)))
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

(defun lyn-gc-finalize ()
  "Reset garbage collection and add minibuffer hooks to toggle it."

  (remove-hook 'pre-command-hook #'lyn-gc-finalize)
  (lyn-gc-restore)

  ;; Additionally disable and restore gc on minibuffer,
  ;; as amx/flx allocate a lot of memory
  (add-hook 'minibuffer-setup-hook #'lyn-gc-disable)
  (add-hook 'minibuffer-exit-hook #'lyn-gc-restore-delayed))
(lyn-gc-disable)
(add-hook 'pre-command-hook #'lyn-gc-finalize)

;;;; Disable VC mode

(remove-hook 'find-file-hook #'vc-refresh-state)

;;;; Unicode

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setf (default-value 'buffer-file-coding-system) 'utf-8-unix)

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
  (straight-use-package 'use-package)
  (setf use-package-expand-minimally t
        use-package-verbose nil))

(use-package no-littering)

;;;; Quiet init

(setf
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode #'fundamental-mode
 initial-scratch-message nil
 (symbol-function 'display-startup-echo-area-message) #'ignore)

;;;; Theme

(when (and (fboundp 'menu-bar-mode)
           (not (eq window-system 'ns)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setf (default-value 'cursor-type) 'bar
      (default-value 'echo-keystrokes) 1e-6
      (default-value 'truncate-lines) t)

(setf (default-value 'mode-line-format)
      (cl-set-difference mode-line-format
                         '(mode-line-front-space mode-line-mule-info mode-line-client)))

(defconst lyn-font-size 14
  "Size at which to render fonts.")
(defconst lyn-font-stack '("Triplicate T4c", "GoMono Nerd Font" "Menlo")
  "Fonts to render with, in priority order.")
(defun lyn-font-available-p (name)
  "Return NAME if NAME is available as a font or nil if not."

  (car (member name (font-family-list))))
(when-let ((font (cl-some #'lyn-font-available-p lyn-font-stack)))
  (let ((sized-font (concat font "-" (number-to-string lyn-font-size))))
    (setf (face-font 'default) sized-font
          (face-font 'fixed-pitch) sized-font)))
(setf (face-font 'variable-pitch) (concat "Charter-" (number-to-string lyn-font-size)))

;; Transparent empty titlebar on NS, buffer name on others
(when (boundp 'ns-use-proxy-icon)
  (setf ns-use-proxy-icon (not (eq window-system 'ns))))
(setf frame-title-format (unless (eq window-system 'ns) "%b")
      (alist-get 'ns-transparent-titlebar
                 (alist-get 'ns window-system-default-frame-alist)) t)

(use-package srcery-theme
  :config (load-theme 'srcery t))

(use-package minions
  :config (minions-mode)
  :custom (minions-mode-line-lighter "\u2026")) ; Horizontal ellipsis

(provide 'early-init)
;;; early-init.el ends here
