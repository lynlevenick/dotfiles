;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(unless (bound-and-true-p early-init-file)
  (load (concat user-emacs-directory "early-init") nil t))

;;;; Defaults
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))
(bind-key "C-c i" #'imenu)
(bind-key "s-`" #'ns-next-frame)

;;;; Editing
(defun lyn-smarter-move-beginning-of-line (arg)
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
(global-set-key [remap move-beginning-of-line] #'lyn-smarter-move-beginning-of-line)

(use-package editorconfig
  :hook (after-init . editorconfig-mode))
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom (flycheck-errors-delay 0.25))
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
(use-package magit)
(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))
(use-package projectile
  :bind-keymap (("s-p" . projectile-command-map))
  :hook (after-init . projectile-mode))
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
(defun lyn-pulse-point (&rest _)
  "Pulse at point."

  (pulse-momentary-highlight-one-line (point)))
(advice-add 'recenter-top-bottom
            :after #'lyn-pulse-point)

;;;; Major Modes
(use-package elm-mode
  :mode "\\.elm\\'"
  :custom (elm-format-command "npm run elm-format"))
(use-package haml-mode
  :mode "\\.haml\\'")
(use-package js2-mode
  :mode ("\\.js\\'"
         ("\\.jsx\\'" . js2-jsx-mode)))
(use-package org
  :mode ("\\.org\\'" . org-mode))
(setf ruby-align-chained-calls t
      ruby-insert-encoding-magic-comment nil)
(use-package rust-mode
  :mode "\\.rs\\'")
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Language Integration
(with-eval-after-load 'flycheck
  (defun lyn-flycheck-bundle-exec (executable special &rest args)
    "Transforms EXECUTABLE and SPECIAL into a command for bundler, with ARGS trailing."

    `(,executable "exec" ,special . ,args))

  (defvar lyn-flycheck-handle-alist
    '(("bundle" . lyn-flycheck-bundle-exec))
    "How to transform a special-cased executable for a command.")
  (defvar lyn-flycheck-wrap-alist
    '(("rubocop" . "bundle"))
    "Executables to transform for special casing.")

  (defun lyn-flycheck-executable-find (executable)
    "Fake COMMAND for specific cases, to e.g. run rubocop through bundle exec."

    (let* ((file (file-name-nondirectory executable))
           (mapped (alist-get file lyn-flycheck-wrap-alist nil :remove #'string=)))
      (if mapped
          (concat (flycheck-default-executable-find mapped) "://" file)
        (flycheck-default-executable-find executable))))
  (setf flycheck-executable-find #'lyn-flycheck-executable-find)

  (defun lyn-flycheck-command-wrapper (command)
    "Handle specially formed COMMANDs from `lyn-flycheck-executable-find'."

    (let* ((components (split-string (car command) "://"))
           (executable (nth 0 components))
           (special (nth 1 components))
           (file (file-name-nondirectory executable)))
      (if special
          (apply (alist-get file lyn-flycheck-handle-alist nil :remove #'string=)
                 executable special (cdr command))
        command)))
  (setf flycheck-command-wrapper-function #'lyn-flycheck-command-wrapper))
(use-package eglot)
(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

;;;; Searching
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
         ("C-c s" . avy-goto-char-2-below))
  :config (advice-add 'avy-action-goto
                      :after #'pulse-momentary-highlight-one-line))
(use-package deadgrep
  :bind (("C-c g" . deadgrep)))
(use-package flx-ido
  :after ido
  :config (flx-ido-mode 1)
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil))
(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  :after ido
  :config (ido-vertical-mode 1)
  :custom (ido-vertical-show-count t))
(use-package smex
  :after ido
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))
;; Turn on ido-mode
(ido-mode 1)
(ido-everywhere 1)
(setf ido-enable-flex-matching t
      magit-completing-read-function #'magit-ido-completing-read)

(provide 'init)
;;; init.el ends here
