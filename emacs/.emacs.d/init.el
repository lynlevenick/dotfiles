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
  "Arrange to execute BODY once, the next time HOOK is run.

This function is especially helpful when configuring package initialization.
A global mode, or one which maps keys, can be set to run only after some
context-relevant thing has happened, rather than loading immediately."
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
  :hook (css-mode elm-mode rjsx-mode typescript-mode))
(use-package bind-key)
(use-package exec-path-from-shell
  :hook ((after-init projectile-after-switch-project-hook) . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil))
(use-package imenu :straight nil
  :bind (("C-c i" . imenu)))

(bind-key "C-?" #'undo-only)
(when (fboundp 'ns-next-frame) (bind-key "s-`" #'ns-next-frame))

(run-with-idle-timer 5 nil #'server-start)

;;;; Editing
(defun lyn-smart-isearch-delete-char ()
  "Delete the failed portion of the search string or the last character."
  (interactive)

  (if (= 0 (length isearch-string))
      (ding)
    (setf isearch-string
          (substring isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
          isearch-message
          (mapconcat 'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))
(bind-key [remap isearch-delete-char] #'lyn-smart-isearch-delete-char isearch-mode-map)
(defun lyn-smart-move-beginning-of-line (arg)
  "Move point between beginning of indentation or beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.

Effectively toggles between the first non-whitespace character and
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
  :init
  (lyn-with-hook-once 'find-file-hook
    (editorconfig-mode)))
(use-package flycheck
  :init
  ;; Needed to allow (setf (url-type x) y)
  (require 'url)
  (defvar lyn-flycheck-handle-alist
    '(("bundle" lyn-flycheck-bundle-exec))
    "How to transform a executable with a schema for a command.")
  (defvar lyn-flycheck-wrap-alist
    '(("rubocop" "bundle" lyn-flycheck-bundle-should-enable)
      ("haml" "bundle" lyn-flycheck-bundle-should-enable))
    "Executables and the schema to prepend to them for `lyn-flycheck-handle-alist'.")
  :hook (prog-mode . flycheck-mode)
  :config
  (defun lyn-flycheck-bundle-should-enable (command)
    "True if COMMAND should be run through bundler."

    (let ((bundler-executable (flycheck-default-executable-find "bundle")))
      (and bundler-executable
           (= 0 (call-process bundler-executable
                              nil nil nil "show" command)))))
  (defun lyn-flycheck-bundle-exec (command &rest args)
    "Transforms COMMAND into a command for bundler, with ARGS trailing."

    `(,(flycheck-default-executable-find "bundle") "exec" ,command . ,args))

  (defun lyn-flycheck-executable-find (executable)
    "Fake EXECUTABLE for specific cases, to e.g. run rubocop through bundle exec."

    (let* ((file (file-name-nondirectory executable))
           (mapped (cdr (assoc-string file lyn-flycheck-wrap-alist)))
           (mapped-schema (car mapped))
           (mapped-check (cadr mapped)))
      (if (or (and mapped-schema (not mapped-check))
              (and mapped-check (funcall mapped-check file)))
          (concat mapped-schema ":" file)
        (flycheck-default-executable-find executable))))
  (defun lyn-flycheck-command-wrapper (command)
    "Handle specially formed COMMANDs from `lyn-flycheck-executable-find'."

    (let* ((url (url-generic-parse-url (car command)))
           (type (url-type url)))
      (if (not type)
          command
        (setf (url-type url) nil)
        (apply (cadr (assoc-string (file-name-nondirectory type)
                                   lyn-flycheck-handle-alist))
               (url-recreate-url url)
               (cdr command)))))
  :custom
  (flycheck-display-errors-delay 0.25)
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
  :init
  (lyn-with-hook-once 'find-file-hook
    (require 'magit))
  :custom
  (magit-list-refs-sortby "-committerdate")
  (magit-completing-read-function #'magit-ido-completing-read))
(use-package magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))
(use-package magit-todos
  :hook (magit-mode . magit-todos-mode)
  :custom (magit-todos-rg-extra-args '("--hidden")))
(use-package mode-line-bell
  :commands (mode-line-bell-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (mode-line-bell-mode)))
(use-package multi-term
  :commands (multi-term multi-term-dedicated-window-p)
  :init
  (defun lyn-multi-term-dwim (&optional dedicated)
    "Create new terminal in the project root if available,
otherwise in `default-directory'."
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
(use-package olivetti
  :hook (org-mode . turn-on-olivetti-mode))
(use-package paren :straight nil
  :hook (prog-mode . show-paren-mode))
(use-package paren-face
  :commands (global-paren-face-mode)
  :init
  (lyn-with-hook-once 'after-change-major-mode-hook
    (global-paren-face-mode)))
(use-package projectile
  :commands (projectile-mode)
  :bind-keymap (("s-p" . projectile-command-map))
  :init
  (lyn-with-hook-once 'pre-command-hook
    (projectile-mode)))
(use-package tramp :straight nil
  :defer
  :init
  (lyn-with-hook-once 'post-self-insert-hook
    (require 'tramp)))
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
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom (nov-text-width 60))
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :custom
  (org-adapt-indentation nil)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-special-ctrl-k t)
  (org-support-shift-select t)
  :custom-face
  ;; Fix heading size
  (org-level-1               ((t (:weight semi-bold :height 1.0))))
  (org-level-2               ((t (:weight semi-bold :height 1.0))))
  (org-level-3               ((t (:weight semi-bold :height 1.0))))
  (org-level-4               ((t (:weight semi-bold :height 1.0))))
  (org-level-5               ((t (:weight semi-bold :height 1.0))))
  ;; Fix link styling - TODO: color?
  (org-link                  ((t (:underline nil))))
  ;; These faces work better in monospace
  (org-block                 ((t (:inherit fixed-pitch))))
  (org-block-begin-line      ((t (:inherit fixed-pitch))))
  (org-block-end-line        ((t (:inherit fixed-pitch))))
  (org-checkbox              ((t (:inherit fixed-pitch))))
  (org-code                  ((t (:inherit fixed-pitch))))
  (org-document-info-keyword ((t (:inherit fixed-pitch))))
  (org-formula               ((t (:inherit fixed-pitch))))
  (org-latex-and-related     ((t (:inherit fixed-pitch))))
  (org-meta-line             ((t (:inherit fixed-pitch))))
  (org-table                 ((t (:inherit fixed-pitch))))
  (org-verbatim              ((t (:inherit fixed-pitch)))))
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
    (tide-hl-identifier-mode)
    (add-hook 'before-save-hook #'tide-format-before-save nil :local))
  (add-hook 'typescript-mode-hook #'lyn-tide-setup))
(use-package typescript-mode
  :mode "\\.tsx?\\'")
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Major Mode Integration
(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))
(use-package prettier-js
  :commands (prettier-js-mode)
  :init
  (add-hook 'rjsx-mode-hook #'prettier-js-mode :append))

;;;; Searching
(use-package amx
  :commands (amx-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (amx-mode))
  :bind (("M-X" . amx-major-mode-commands)))
(use-package anzu
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode)
  :init
  (bind-key [remap query-replace] #'anzu-query-replace)
  (bind-key [remap query-replace-regexp] #'anzu-query-replace-regexp)
  (lyn-with-hook-once 'pre-command-hook
    (global-anzu-mode)))
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
  :config (flx-ido-mode)
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil))
(use-package ido :straight nil
  :defer
  :commands (ido-mode ido-everywhere)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (ido-mode)
    (ido-everywhere))
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-enable-flex-matching t))
(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode))
(use-package ido-vertical-mode
  :after ido
  :config (ido-vertical-mode))

(provide 'init)
;;; init.el ends here
