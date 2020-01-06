;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(unless (bound-and-true-p early-init-file)
  (require 'early-init (concat user-emacs-directory "early-init")))

(defmacro lyn-with-hook-once (hook &rest body)
  "Arrange to execute BODY once, the next time HOOK is run.

This function is especially helpful when configuring package initialization.
A global mode, or one which maps keys, can be set to run only after some
context-relevant thing has happened, rather than loading immediately."
  (declare (indent defun))

  (let ((name (gensym)))
    `(progn
       (unless (fboundp (quote ,name))
         (defun ,name ()
           (remove-hook ,hook (function ,name))
           ,@body))
       (add-hook ,hook (function ,name)))))

(defun lyn-accessible-directory-p (filename)
  "Like ‘file-accessible-directory-p’ for FILENAME but work around an Apple bug."

  (and (file-directory-p filename)
       (file-accessible-directory-p filename)))

(defun lyn-safe-default-directory (&optional file)
  "Return a safe ‘default-directory’ relative to FILE or ‘default-directory’.

Like ‘magit’s ‘magit--safe-default-directory’."

  (catch 'unsafe-default-dir
    (let ((dir (file-name-as-directory (expand-file-name (or file default-directory))))
          (previous nil))
      (while (not (lyn-accessible-directory-p dir))
        (setf dir (file-name-directory (directory-file-name dir)))
        (when (equal dir previous)
          (throw 'unsafe-default-dir nil))
        (setf previous dir))
      dir)))

(defun lyn-relevant-dir (&optional file)
  "Relative to FILE or ‘default-directory’, get a safe default directory.

Returns from function ‘projectile-project-root’ relative to FILE if ‘projectile’ is loaded."

  (lyn-safe-default-directory
   (if (and (featurep 'projectile)
            (projectile-project-p file))
       (projectile-project-root file)
     file)))

(defmacro lyn-with-relevant-dir (file &rest body)
  "Execute BODY with ‘lyn-relevant-dir’ relative to FILE as ‘default-directory’."
  (declare (indent defun) (debug (form body)))

  `(when-let ((default-directory (lyn-relevant-dir ,file)))
     ,@body))

(defconst lyn-fetchhash--sentinel (gensym)
  "Sentinel for ‘lyn-fetchhash’ to detect missing values.")
(defmacro lyn-fetchhash (key table set-when-default)
  "Look up KEY in TABLE and return value or assign SET-WHEN-DEFAULT and return."

  `(if-let ((cached (gethash ,key ,table lyn-fetchhash--sentinel))
            ((eq cached lyn-fetchhash--sentinel)))
       (setf (gethash ,key ,table) ,set-when-default)
     cached))
(use-package bind-key)

;;;; Defaults

;; SOMEDAY:
;; Smooth scroll when this doesn't generate an obscene amount of garbage
;; and/or it properly handles the macOS scrolling behaviors
;; emacs-mac-port does scroll correctly - with stuttering for an unknown
;; reason. There isn't a great solution here with any approach

;; (when (fboundp 'pixel-scroll-mode)
;;   (pixel-scroll-mode)
;;   (setf pixel-resolution-fine-flag 1
;;         pixel-dead-time 0
;;         mouse-wheel-progressive-speed t))

(bind-key "C-?" #'undo-only)
(when (boundp 'mac-option-modifier) (setf mac-option-modifier 'meta))
(when (boundp 'mac-command-modifier) (setf mac-command-modifier 'super))
(when (fboundp 'ns-next-frame) (bind-key "s-`" #'ns-next-frame))

(use-package imenu :straight nil
  :bind (("C-c i" . imenu)))

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

;; ‘comment-dwim-2’ provides a slightly-smarter version of ‘comment-dwim’
(use-package comment-dwim-2
  :bind (([remap comment-dwim] . comment-dwim-2)))

(use-package editorconfig
  :commands (editorconfig-mode)
  :init
  (lyn-with-hook-once 'find-file-hook
    (editorconfig-mode)))

;; ‘electric-mode’ provides smart quotes used in elisp documentation
(use-package electric :straight nil
  :hook (emacs-lisp-mode . electric-quote-local-mode))

;; ‘uniquify’ determines how buffer names are made distinct when visiting
;; files with the same name in different directories
(use-package uniquify :straight nil
  :defer
  :custom (uniquify-buffer-name-style 'forward))

;;;; Interaction

(use-package ace-window
  :bind (([remap other-window] . ace-window))
  :custom
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))

(use-package company
  :hook (prog-mode . company-mode))

(use-package company-statistics
  :commands (company-statistics-mode)
  :init
  (lyn-with-hook-once 'company-mode-hook
    (company-statistics-mode)))

(use-package dired :straight nil
  :defer
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (wdired-allow-to-change-permissions t)
  (wdired-allow-redirect-links t))

(use-package dired-x :straight nil
  :init
  (with-eval-after-load 'dired
    (require 'dired-x))
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(use-package isearch :straight nil
  :defer
  :custom (lazy-highlight-initial-delay 0))

(use-package magit
  :defer
  :init
  (lyn-with-hook-once 'find-file-hook
    (require 'magit))
  :custom
  (magit-list-refs-sortby "-committerdate")
  (magit-completing-read-function #'magit-ido-completing-read)
  (magit-diff-adjust-tab-width t))

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
    "Create new terminal in the project root or fall back to ‘default-directory’."
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

;; ‘olivetti’ provides a mode which centers content in a buffer via margins.
(use-package olivetti
  :hook (org-mode . turn-on-olivetti-mode))

(use-package paren :straight nil
  :hook (prog-mode . show-paren-mode)
  :custom (show-paren-delay 0))

(use-package paren-face
  :commands (global-paren-face-mode)
  :init
  (lyn-with-hook-once 'after-change-major-mode-hook
    (global-paren-face-mode)))

(use-package projectile
  :commands (projectile-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (projectile-mode))
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

(use-package tramp :straight nil
  :defer
  :init
  (lyn-with-hook-once 'post-self-insert-hook
    (require 'tramp)))

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
  ;; TODO: No links in ~code~
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
  (org-block                 ((t (:inherit fixed-pitch)))) ; TODO: Follow buffer text size (C-x C-+)
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

(use-package typescript-mode
  :mode "\\.tsx?\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;;; Syntax Checking, Linting, and Formatting

;; ‘apheleia’ provides asynchronous autoformatting
(use-package apheleia :straight (:host github :repo "raxod502/apheleia")
  :commands (apheleia-global-mode)
  :init
  (lyn-with-hook-once 'find-file-hook
    (apheleia-global-mode)))

;; ‘flycheck’ provides in-buffer errors, warnings, and syntax checking
(use-package flycheck
  :commands (global-flycheck-mode)
  :init
  (lyn-with-hook-once 'find-file-hook
    (global-flycheck-mode))
  :config
  ;; Override the sh-shellcheck checker's configuration to normalize the shell
  ;; ‘false’ to ‘sh’.
  (add-to-list 'flycheck-shellcheck-supported-shells 'false)
  (defun lyn-flycheck--normalize-sh (symbol)
    "Return 'sh if SYMBOL is 'false, otherwise return SYMBOL."

    (if (eq symbol 'false)
        'sh
      symbol))
  (dolist (elt (get 'sh-shellcheck 'flycheck-command))
    (when (and (listp elt)
               (equal (cdr elt) '((symbol-name sh-shell))))
      (setcdr elt '((symbol-name (lyn-flycheck--normalize-sh sh-shell))))))

  ;; Extend flycheck to handle running an executable to determine if a command
  ;; is runnable, and to support running an executable through another.
  (defvar lyn-flycheck-handle-alist
    '(("bundle" lyn-flycheck-bundle-exec))
    "Map the schema a file exists under to a handler method.")
  (defvar lyn-flycheck-wrap-alist
    '(("rubocop" "bundle" lyn-flycheck-bundle-should-enable)
      ("haml" "bundle" lyn-flycheck-bundle-should-enable))
    "Schema to prepend to certain executables and optionally a test run before enabling.

If the test returns a non-nil value, then the schema will be prepended
during discovery of the specified executable.")

  (defvar lyn-flycheck--bundle-executable-cache (make-hash-table)
    "Cache for bundle executables by project or directory.")
  (defun lyn-flycheck--bundle-executable (dir)
    "Retrieve cached bundle executable for DIR."

    (lyn-fetchhash dir lyn-flycheck--bundle-executable-cache
                   (flycheck-default-executable-find "bundle")))

  (defvar lyn-flycheck--bundle-should-enable-cache (make-hash-table)
    "Cache for results from ‘lyn-flycheck-bundle-should-enable’.")
  (defun lyn-flycheck-bundle-should-enable (command)
    "True if COMMAND should be run through bundle."

    (let ((dir (lyn-relevant-dir)))
      (lyn-fetchhash dir lyn-flycheck--bundle-should-enable-cache
                     (let ((bundle-executable (lyn-flycheck--bundle-executable dir)))
                       (and bundle-executable
                            (= 0 (call-process bundle-executable
                                               nil nil nil "show" command)))))))

  (defun lyn-flycheck-bundle-exec (command &rest args)
    "Transforms COMMAND into a command for bundle, with ARGS trailing."

    `(,(lyn-flycheck--bundle-executable (lyn-relevant-dir))
      "exec" ,command ,@args))

  (defun lyn-flycheck-executable-find (executable)
    "Wrap EXECUTABLE for specific cases, to e.g. run rubocop through bundle exec."

    (let* ((file (file-name-nondirectory executable))
           (mapped (cdr (assoc-string file lyn-flycheck-wrap-alist)))
           (mapped-schema (car mapped))
           (mapped-check (cadr mapped)))
      (if (or (and mapped-schema (not mapped-check))
              (and mapped-check (funcall mapped-check file)))
          (concat mapped-schema ":" file)
        (flycheck-default-executable-find executable))))

  (defun lyn-flycheck-command-wrapper (command)
    "Handle specially formed COMMANDs from ‘lyn-flycheck-executable-find’."

    (let* ((url (url-generic-parse-url (car command)))
           (type (url-type url)))
      (if (not type)
          command
        (aset url 1 nil)                ; expanded (setf (url-type url) nil), (require 'url) needed before setf expansion
        (apply (cadr (assoc-string (file-name-nondirectory type)
                                   lyn-flycheck-handle-alist))
               (url-recreate-url url)
               (cdr command)))))
  :custom
  (flycheck-display-errors-delay 0.25)
  (flycheck-executable-find #'lyn-flycheck-executable-find)
  (flycheck-command-wrapper-function #'lyn-flycheck-command-wrapper))

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package tide
  :commands (tide-setup)
  :init
  (defun lyn-tide-setup ()
    "Prepare for Typescript development."

    (tide-setup)
    (tide-hl-identifier-mode))
  (lyn-with-hook-once 'typescript-mode-hook
    (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) :append))
  (add-hook 'typescript-mode-hook #'lyn-tide-setup))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; Paths
;; It is important that these hooks happen before hooks that try to integrate
;; with Javascript or Ruby or other languages
;; TODO: Better solution for this ordering problem
(use-package add-node-modules-path
  :commands (add-node-modules-path))
(use-package exec-path-from-shell
  :commands (exec-path-from-shell-initialize)
  ;; TODO: Stop using rvm, use shim-based tools instead like rbenv
  ;; TODO: This can vary depending on projectile project, and the switch-project-hook
  ;;       isn't that great of a solution for catching issues. Can we initialize once
  ;;       on startup, and once upon visiting a projectile project, then make the
  ;;       appropriate variables local per-file per-project?
  ;; TODO: This blows away the result of add-node-modules-path
  ;; HACK: Doing this on every find-file is, uh, terrible. Thanks RVM!
  :hook ((after-init . exec-path-from-shell-initialize))
  :custom
  (exec-path-from-shell-check-startup-files nil))

(defvar lyn-original-exec-path exec-path
  "The value of ‘exec-path’ when Emacs was first started.")
(defvar lyn-local-exec-path-cache (make-hash-table :test 'equal)
  "Cache for exec paths by project or directory.")
(defun lyn-local-exec-path (&optional drop-cache)
  "Set up ‘exec-path’ for the current project.

If DROP-CACHE is non-nil, then recreate ‘lyn-local-exec-path-cache’."
  (interactive "p")

  (when drop-cache
    (setf lyn-local-exec-path-cache (make-hash-table :test 'equal)))

  (when (and (or buffer-file-name                     ; Accessing a file
                 (derived-mode-p 'magit-mode))        ; or running under Magit
             (not (file-remote-p default-directory))) ; File not under Tramp
    (lyn-with-relevant-dir nil
      (when (file-directory-p default-directory)
        (make-local-variable 'exec-path)
        (setf exec-path
              (lyn-fetchhash
               default-directory lyn-local-exec-path-cache
               (progn
                 (setf exec-path lyn-original-exec-path)
                 (exec-path-from-shell-initialize)
                 (add-node-modules-path)
                 exec-path)))))))
(add-hook 'find-file-hook #'lyn-local-exec-path)
(add-hook 'magit-setup-buffer-hook #'lyn-local-exec-path)

;;;; Searching

(use-package amx
  :commands (amx-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (amx-mode))
  :bind (("M-X" . amx-major-mode-commands)))

(use-package anzu
  :commands (global-anzu-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (global-anzu-mode))
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package avy
  :bind (("C-c c" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
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

;;;; Window manipulation

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

(use-package winner :straight nil
  :commands (winner-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (winner-mode)))

;; ‘zygospore’ provides toggling of the ‘delete-other-windows’ command
(use-package zygospore
  :bind (([remap delete-other-windows] . zygospore-toggle-delete-other-windows)))

(provide 'init)
;;; init.el ends here
