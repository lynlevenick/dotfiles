;;; init.el --- My Emacs initialization
;;; Commentary:
;; There are many Emacs configurations, but this one is mine.
;;; Code:

(defvar file-name-handler-alist-original)
(defvar gc-cons-threshold-original)
(setf file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil
      gc-cons-threshold-original gc-cons-threshold
      gc-cons-threshold (* 64 1024 1024))
(run-with-idle-timer
 5 nil
 (lambda ()
   (setf file-name-handler-alist file-name-handler-alist-original
	       gc-cons-threshold gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)))

(require 'package)
(setf package-enable-at-startup nil)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package delight
  :ensure t)

;; Tons of default Emacs settings
(setf backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      custom-file "~/.emacs.d/custom.el"
      echo-keystrokes 0.0001
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-major-mode #'fundamental-mode
      initial-scratch-message nil
      jit-lock-stealth-time 1
      sentence-end-double-space nil
      user-full-name "Lyn Levenick"
      user-mail-address "lyn.levenick@gmail.com")
(setq-default indent-tabs-mode nil
	            tab-width 2)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(use-package dired
  :config (setf dired-use-ls-dired nil))

;;;; Fix broken defaults
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;;;; Theme
(push '(font . "Menlo-12") initial-frame-alist)
(push '(font . "Menlo-12") default-frame-alist)
(use-package smart-mode-line
  :ensure t
  :config (progn
            (setf sml/replacer-regexp-list nil
                  sml/theme nil)
            (sml/setup)))
(use-package spacemacs-common
  :ensure spacemacs-theme
  :config (load-theme 'spacemacs-dark t))

;;;; Editing
(defun lyn-smarter-move-beginning-of-line (arg)
  "Move point between beginning of indentation and beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning of end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(bind-key "C-a" #'lyn-smarter-move-beginning-of-line)
(use-package flycheck
  :ensure t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package syntax-subword
  :ensure t
  :config (progn
            (setf syntax-subword-skip-spaces 'consistent)
            (global-syntax-subword-mode 1)))
(use-package ws-butler
  :ensure t
  :delight ws-butler-mode
  :commands (ws-butler-mode)
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))

;;;; Interaction
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config (setf aw-background nil
                aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame))
(use-package magit
	:ensure t
	:bind ("C-c g" . magit-status)
	:config (setf magit-completing-read-function 'magit-ido-completing-read))
(use-package windsize
  :ensure t
  :bind (("C-s-<up>" . windsize-up)
         ("C-s-<down>" . windsize-down)
         ("C-s-<left>" . windsize-left)
         ("C-s-<right>" . windsize-right)
         ("C-s-w" . windsize-up)
         ("C-s-s" . windsize-down)
         ("C-s-a" . windsize-left)
         ("C-s-d" . windsize-right)))

;;;; Major Modes
(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'")

;;;; Searching
(defun lyn-isearch-delete-something ()
  "Delete non-matching text or the last character.

If isearch has a failing match, deletes the failing portion.
If isearch has no failing match, deletes the last character.
If no previous match was done, just beeps."
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setf isearch-string (substring isearch-string
                                    0
                                    (or (isearch-fail-pos)
                                        (1- (length isearch-string))))
          isearch-message (mapconcat #'isearch-text-char-description
                                     isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))
(bind-key "<backspace>" #'lyn-isearch-delete-something isearch-mode-map)
(use-package anzu
  :ensure t
  :demand
  :delight anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode 1))
(use-package helm
  :ensure t
  :init (progn
          (require 'helm-config)
          (setf helm-buffers-fuzzy-matching t
                helm-candidate-number-limit 100
                helm-move-to-line-cycle-in-source t
                helm-split-window-inside-p t)
          (bind-key "C-c h" #'helm-command-prefix)
          (unbind-key "C-x c")
          (helm-mode 1))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         (:map helm-map
               ("<tab>" . helm-execute-persistent-action)
               ("C-z" . helm-select-action)))
  :config (progn
            (use-package helm-descbinds
              :ensure t
              :init (helm-descbinds-mode 1))))

(provide 'init)
;;; init.el ends here