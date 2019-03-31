;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(unless (bound-and-true-p early-init-file)
  (require 'early-init (concat user-emacs-directory "early-init")))

(defvar lyn-with-hook-once--count 0
  "The number of times `lyn-with-hook-once' has been called.

Used to generate symbols for the hook functions.")
(defmacro lyn-with-hook-once (hook &rest body)
  "Arrange to execute BODY once, the next time HOOK is run."
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
  (with-eval-after-load 'elm-mode
    (add-hook 'elm-mode-hook #'add-node-modules-path))
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'add-node-modules-path))
  (with-eval-after-load 'typescript-mode
    (add-hook 'typescript-mode-hook #'add-node-modules-path))
  :hook (css-mode . add-node-modules-path))
(use-package bind-key)
(use-package exec-path-from-shell
  :init
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook #'exec-path-from-shell-initialize))
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil))
(use-package imenu :straight nil
  :bind (("C-c i" . imenu)))

(bind-key "C-?" #'undo-only)
(when (fboundp 'ns-next-frame) (bind-key "s-`" #'ns-next-frame))

(run-with-idle-timer 5 nil #'server-start)

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
(bind-key [remap move-beginning-of-line] #'lyn-smart-move-beginning-of-line)

(use-package editorconfig
  :commands (editorconfig-mode)
  :init (lyn-with-hook-once 'find-file-hook (editorconfig-mode 1)))
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
  :commands (ace-window)
  :init (bind-key [remap other-window] #'ace-window)
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
  :commands (mode-line-bell-mode)
  :init (lyn-with-hook-once 'pre-command-hook (mode-line-bell-mode 1)))
(use-package multi-term
  :commands (multi-term multi-term-dedicated-window-p)
  :init
  (defun lyn-multi-term-dwim (&optional dedicated)
    "Create new terminal in the project root if available,
otherwise `default-directory'."
    (interactive)

    (let ((open-command (if dedicated
                            #'multi-term-dedicated-open
                          #'multi-term)))
      (if (projectile-project-p)
          (projectile-with-default-dir (projectile-project-root)
            (funcall open-command))
        (funcall open-command))))
  (defun lyn-multi-term-dedicated-dwim ()
    "Close dedicated terminal if focused, or focus the dedicated terminal."
    (interactive)

    (cond ((multi-term-dedicated-window-p)
           (multi-term-dedicated-close))
          ((multi-term-dedicated-exist-p)
           (select-window multi-term-dedicated-window))
          ((progn
             (lyn-multi-term-dwim :dedicated)
             (select-window multi-term-dedicated-window)))))
  :bind (("C-c d" . lyn-multi-term-dedicated-dwim)
         ("C-c t" . lyn-multi-term-dwim))
  :custom
  (term-bind-key-alist '(("<C-tab>" . multi-term-next)
                         ("<C-S-tab>" . multi-term-prev)
                         ("C-c C-c" . term-interrupt-subjob)
                         ("C-y" . term-paste)
                         ("<C-backspace>" . term-send-raw)
                         ("M-b" . term-send-raw-meta)
                         ("M-f" . term-send-raw-meta)
                         ("M-d" . term-send-raw-meta)
                         ("<M-backspace>" . term-send-raw-meta)
                         ("<M-left>" . term-send-backward-word)
                         ("<M-right>" . term-send-forward-word))))
(use-package nswbuff
  :bind (("<C-tab>" . nswbuff-switch-to-next-buffer)
         ("<C-S-tab>" . nswbuff-switch-to-previous-buffer))
  :custom
  (nswbuff-buffer-list-function #'nswbuff-projectile-buffer-list)
  (nswbuff-clear-delay 1)
  (nswbuff-display-intermediate-buffers t))
(use-package paren :straight nil
  :hook (prog-mode . show-paren-mode))
(use-package projectile
  :commands (projectile-mode)
  :bind-keymap (("s-p" . projectile-command-map))
  :init (lyn-with-hook-once 'pre-command-hook (projectile-mode 1)))
(use-package tramp
  :defer
  :init (lyn-with-hook-once 'post-self-insert-hook (require 'tramp)))
(use-package transpose-frame
  :bind (("C-c /" . transpose-frame)))
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
  :commands (zygospore-toggle-delete-other-windows)
  :init (bind-key [remap delete-other-windows] #'zygospore-toggle-delete-other-windows))

;;;; Major Modes
(use-package elm-mode
  :commands (company-elm)
  :mode "\\.elm\\'"
  :config (add-to-list 'company-backends #'company-elm)
  :custom (elm-format-on-save t))
(use-package haml-mode
  :mode "\\.haml\\'")
(use-package nim-mode
  :mode "\\.nim\\(s\\|ble\\)?\\'")
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 60))
(use-package org
  :mode ("\\.org\\'" . org-mode))
(use-package rjsx-mode
  :mode "\\.jsx?\\'")
(use-package ruby-mode :straight nil
  :defer
  :custom
  (ruby-align-chained-calls t)
  (ruby-insert-encoding-magic-comment nil))
(use-package rust-mode
  :mode "\\.rs\\'")
(use-package tide
  :commands (tide-setup)
  :init
  (defun lyn-tide-setup ()
    "Prepare for Typescript development."

    (tide-setup)
    (tide-hl-identifier-mode 1)
    (add-hook 'before-save-hook #'tide-format-before-save nil :local))
  (add-hook 'typescript-mode-hook #'lyn-tide-setup))
(use-package typescript-mode
  :mode "\\.tsx?\\'")
(use-package web-mode
  :mode "\\.njkl?\\'")
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Major Mode Integration
(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))
(use-package prettier-js
  :commands (prettier-js-mode)
  :init
  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'prettier-js-mode)))

;;;; Searching
(use-package amx
  :commands (amx-mode)
  :init (lyn-with-hook-once 'pre-command-hook (amx-mode))
  :bind (("M-X" . amx-major-mode-commands)))
(use-package anzu
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode)
  :init
  (bind-key [remap query-replace] #'anzu-query-replace)
  (bind-key [remap query-replace-regexp] #'anzu-query-replace-regexp)
  (lyn-with-hook-once 'pre-command-hook (global-anzu-mode 1)))
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
(use-package ido :straight nil
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
  :config (ido-vertical-mode 1))

(provide 'init)
;;; init.el ends here
