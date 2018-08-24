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
(defun lyn-restore-original-vars ()
  "Restore variables temporarily set during initialization."
  (setf file-name-handler-alist file-name-handler-alist-original
	      gc-cons-threshold gc-cons-threshold-original)
  (makunbound 'file-name-handler-alist-original)
  (makunbound 'gc-cons-threshold-original))
(run-with-idle-timer 5 nil #'lyn-restore-original-vars)

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
	            tab-width 2
              truncate-lines t)
(column-number-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(use-package dired
  :init (setf dired-use-ls-dired nil))

;;;; Fix broken defaults
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;;;; Theme
(setf frame-title-format nil
      ns-use-proxy-icon nil)
(push '(font . "Menlo-12") default-frame-alist)
(push '(font . "Menlo-12") initial-frame-alist)
(push '(height . 25) default-frame-alist)
(push '(height . 25) initial-frame-alist)
(push '(width . 80) default-frame-alist)
(push '(width . 80) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) initial-frame-alist)
(use-package smart-mode-line
  :ensure t
  :after (doom-themes solaire-mode)
  :init (setf sml/replacer-regexp-list nil
              sml/theme nil)
  :config (sml/setup))
(use-package solaire-mode
  :ensure t
  :after (doom-themes)
  :hook ((after-revert change-major-mode ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (solaire-mode-swap-bg)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

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
  (setf arg (or arg 1))

  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(bind-key "C-a" #'lyn-smarter-move-beginning-of-line)
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :init (setq-default display-line-numbers-type 'relative))
(use-package flycheck
  :ensure t
  :init (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :init (setf show-paren-delay 0))
(use-package syntax-subword
  :ensure t
  :init
  (setf syntax-subword-skip-spaces 'consistent)
  (global-syntax-subword-mode 1))
(use-package ws-butler
  :ensure t :delight ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;;;; Interaction
(use-package ace-window
  :ensure t
  :init (setf aw-background nil
              aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-scope 'frame)
  :bind (("C-x o" . ace-window)))
(use-package magit
	:ensure t
	:bind ("C-c g" . magit-status))
(use-package projectile
  :ensure t :delight projectile-mode
  :init
  (setf projectile-project-search-path '("~/"))
  (projectile-mode 1)
  :bind-keymap (("C-c p" . projectile-command-map)))
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
(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")
(use-package ruby-mode
  :init (setq-default ruby-align-chained-calls t))

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
  :ensure t :delight anzu-mode
  :init (global-anzu-mode 1)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))
(use-package counsel
  :ensure t :delight counsel-mode
  :after (ivy)
  :init (setf counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color-never '%s' %s")
  :config (counsel-mode 1))
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :bind (("s-d" . counsel-projectile-find-dir)
         ("s-f" . counsel-projectile-find-file)
         ("s-g" . counsel-projectile-rg)
         ("s-p" . counsel-projectile-switch-project))
  :config (counsel-projectile-mode 1))
(use-package ivy
  :ensure t :delight ivy-mode
  :init
  (setf projectile-completion-system 'ivy)
  (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)))
(use-package swiper
  :ensure t
  :after (ivy counsel)
  :bind (("C-r" . counsel-grep-or-swiper)
         ("C-s" . counsel-grep-or-swiper)))

(provide 'init)
;;; init.el ends here
