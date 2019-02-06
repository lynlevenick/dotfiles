;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(unless (bound-and-true-p early-init-file)
  (load (concat user-emacs-directory "early-init") nil t))

(defvar lyn-with-hook-once--count 0
  "The number of times `lyn-with-hook-once' has been called.

Used to generate symbols for the hook functions.")
(defmacro lyn-with-hook-once (hook &rest body)
  "Arrange to execute BODY when HOOK is run, one time.

If called repeatedly before HOOK runs, BODY will be run
only once.

If called twice, with HOOK running in between, BODY will
have executed once and be arranged to be executed once
more."
  (declare (indent defun))

  (let ((name (make-symbol (concat "with-hook-once--hook-" (number-to-string lyn-with-hook-once--count)))))
    (setf lyn-with-hook-once--count (1+ lyn-with-hook-once--count))
    `(progn
       (unless (fboundp ',name)
         (defun ,name ()
           (remove-hook ,hook #',name)
           . ,body))
       (add-hook ,hook #',name))))

;;;; Defaults
(use-package add-node-modules-path
  :commands (add-node-modules-path)
  :init
  (with-eval-after-load 'prettier-js
    (add-hook 'prettier-js-mode-hook #'add-node-modules-path))
  :hook (css-mode . add-node-modules-path))
(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  :init
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook #'exec-path-from-shell-initialize))
  :hook (after-init . exec-path-from-shell-initialize))
(use-package imenu
  :bind (("C-c i" . imenu)))
(when (fboundp 'ns-next-frame) (bind-key "s-`" #'ns-next-frame))

;;;; Editing
(defun lyn-smart-move-beginning-of-line (arg)
  "Move point between beginning of indentation or beginning of line.

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
(global-set-key [remap move-beginning-of-line] #'lyn-smart-move-beginning-of-line)

(use-package editorconfig
  :hook (after-init . editorconfig-mode))
(use-package flycheck
  :init
  (defvar lyn-flycheck-handle-alist
    '(("bundle" . lyn-flycheck-bundle-exec))
    "How to transform a special-cased executable for a command.")
  (defvar lyn-flycheck-wrap-alist
    '(("rubocop" . "bundle"))
    "Executables to transform for special casing.")
  :hook (prog-mode . flycheck-mode)
  :config
  (defun lyn-flycheck-bundle-exec (executable special &rest args)
    "Transforms EXECUTABLE and SPECIAL into a command for bundler, with ARGS trailing."

    `(,executable "exec" ,special . ,args))
  (defun lyn-flycheck-executable-find (executable)
    "Fake EXECUTABLE for specific cases, to e.g. run rubocop through bundle exec."

    (let* ((file (file-name-nondirectory executable))
           (mapped (alist-get file lyn-flycheck-wrap-alist nil nil #'string=)))
      (if mapped
          (concat (flycheck-default-executable-find mapped) "://" file)
        (flycheck-default-executable-find executable))))
  (defun lyn-flycheck-command-wrapper (command)
    "Handle specially formed COMMANDs from `lyn-flycheck-executable-find'."

    (let* ((components (split-string (car command) "://"))
           (executable (nth 0 components))
           (special (nth 1 components))
           (file (file-name-nondirectory executable)))
      (if special
          (apply (alist-get file lyn-flycheck-handle-alist nil nil #'string=)
                 executable special (cdr command))
        command)))
  :custom
  (flycheck-errors-delay 0.25)
  (flycheck-executable-find #'lyn-flycheck-executable-find)
  (flycheck-command-wrapper-function #'lyn-flycheck-command-wrapper))
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; Interaction
(use-package ace-window
  :bind (("C-x o" . ace-window))
  :custom
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))
(use-package company
  :hook (prog-mode . company-mode))
(use-package dired :straight nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (wdired-allow-to-change-permissions t)
  (wdired-allow-redirect-links t))
(use-package dired-x :straight nil
  :after dired)
(use-package magit
  :defer
  :init (lyn-with-hook-once 'find-file-hook (require 'magit))
  :custom
  (magit-list-refs-sortby "-committerdate")
  (magit-completing-read-function #'magit-ido-completing-read))
(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))
(use-package paren :straight nil
  :hook (prog-mode . show-paren-mode))
(use-package projectile
  :bind-keymap (("s-p" . projectile-command-map))
  :hook (after-init . projectile-mode))
(use-package tramp
  :defer
  :init (lyn-with-hook-once 'post-self-insert-hook (require 'tramp)))
(use-package transpose-frame
  :bind (("C-c t" . transpose-frame)))
(use-package windsize
  :bind (("C-s-<up>" . windsize-up)
         ("C-s-<down>" . windsize-down)
         ("C-s-<left>" . windsize-left)
         ("C-s-<right>" . windsize-right)
         ("C-s-w" . windsize-up)
         ("C-s-s" . windsize-down)
         ("C-s-a" . windsize-left)
         ("C-s-d" . windsize-right)))
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)))

;;;; Major Modes
(use-package haml-mode
  :mode "\\.haml\\'")
(use-package js2-mode
  :mode "\\.js\\'"
  :custom (js2-skip-preprocessor-directives t))
(use-package org
  :mode ("\\.org\\'" . org-mode))
(use-package rjsx-mode
  :mode "\\.jsx\\'")
(use-package ruby-mode :straight nil
  :defer
  :custom
  (ruby-align-chained-calls t)
  (ruby-insert-encoding-magic-comment nil))
(use-package rust-mode
  :mode "\\.rs\\'")
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Major Mode Integration
(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))
(use-package prettier-js
  :after js2-mode
  :hook
  (js2-mode . prettier-js-mode)
  (js2-jsx-mode . prettier-js-mode))

;;;; Searching
(use-package amx
  :commands (amx-mode)
  :init (lyn-with-hook-once 'pre-command-hook (amx-mode))
  :bind (("M-X" . amx-major-mode-commands)))
(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :hook (after-init . global-anzu-mode))
(use-package avy
  :bind (("C-c c" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
         ("C-c n" . avy-goto-line-below)
         ("C-c p" . avy-goto-line-above)
         ("C-c r" . avy-goto-char-2-above)
         ("C-c s" . avy-goto-char-2-below)))
(use-package deadgrep
  :bind (("C-c g" . deadgrep)))
(use-package flx-ido
  :after ido
  :config (flx-ido-mode 1)
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil))
(use-package ido
  :defer
  :commands (ido-mode ido-everywhere)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (ido-mode 1)
    (ido-everywhere 1))
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-enable-flex-matching t))
(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  :after ido
  :config (ido-vertical-mode 1)
  :custom (ido-vertical-show-count t))

(provide 'init)
;;; init.el ends here
