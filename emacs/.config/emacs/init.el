;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; There are many configurations, but this one is mine.

;;; Code:

;;;; Dependencies

(eval-when-compile
  (require 'rx))

;;;; Post-initialization theming

(setf (face-font 'default) "Comic Code-12"
      (face-font 'fixed-pitch) "Comic Code-12"
      (face-font 'variable-pitch) "Valkyrie T4-14")

;;;; Convenience

(defun gensym-list (length &optional prefix)
  "Return a list of new uninterned symbols. See ‘gensym’.

List is of LENGTH and symbols have PREFIX."

  (cl-loop for i below length
           collect (gensym prefix)))

(defmacro lyn-with-gensyms (names &rest body)
  "Bind NAMES to symbols generated with ‘gensym’ then eval BODY.

Each element within NAMES is either a symbol SYMBOL or a
pair (SYMBOL STRING-DESIGNATOR). Bare symbols are equivalent
to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL STRING-DESIGNATOR) specifies that the variable
named by SYMBOL should be bound to a symbol constructed using ‘gensym’
with the string designated by STRING-DESIGNATOR being its
first argument.

Ported from Alexandria."
  (declare (indent 1))

  `(let ,(cl-loop for name in names
                  collect (cl-multiple-value-bind (symbol string)
                              (cl-etypecase name
                                (symbol (cl-values name (symbol-name name)))
                                (list ; Work around incomplete typecase
                                 (cl-check-type (car name) symbol)
                                 (cl-check-type (cdr name) list)
                                 (cl-check-type (cddr name) null)
                                 (cl-values (car name)
                                            (cl-etypecase (cadr name)
                                              (symbol (symbol-name (cadr name)))
                                              (string (cadr name))))))
                            `(,symbol (gensym ,string))))
     ,@body))

(defmacro lyn-once-only (specs &rest body)
  "Bind SPECS within BODY such that they are evaluated only once.

Each element within SPECS is either a symbol SYMBOL or a
pair (SYMBOL INITFORM). Bare symbols are equivalent to the
pair (SYMBOL SYMBOL).

Each pair (SYMBOL INITFORM) specifies a single intermediate
variable. SYMBOL will be bound in BODY to the associated
INITFORM.

INITFORMs of all pairs are evaluated before binding SYMBOLs
and evaluating BODY.

Ported from Alexandria."
  (declare (indent 1))

  ;; Create gensyms and create (symbol . initform) pairs for later consumption
  (let ((gensyms (gensym-list (length specs) "once-only"))
        (names-and-forms
         (cl-loop for spec in specs
                  collect (cl-etypecase spec
                            (list (cl-destructuring-bind (name form) spec
                                    (cons name form)))
                            (symbol (cons spec spec))))))
    ;; Bind gensyms in scope
    `(lyn-with-gensyms ,(cl-loop for g in gensyms
                                 for (n . _) in names-and-forms
                                 collect `(,g ,n))
       ;; Bind gensyms to initial values
       `(let ,(list ; Work around error in expanding multiple arguments to ,
                ,@(cl-loop for g in gensyms
                           for (_ . f) in names-and-forms
                           collect ``(,,g ,,f)))
          ;; Bind names to gensyms
          ,(let ,(cl-loop for g in gensyms
                          for (n . _) in names-and-forms
                          collect `(,n ,g))
             ,@body)))))

(defmacro lyn-with-hook-once (hook &rest body)
  "Arrange to execute BODY once, the next time HOOK is run.

This function is especially helpful when configuring package initialization.
A global mode, or one which maps keys, can be set to run only after some
context-relevant thing has happened, rather than loading immediately."
  (declare (indent defun))

  (lyn-with-gensyms (name)
    `(progn
       (unless (fboundp ',name)
         (defun ,name ()
           (remove-hook ,hook #',name)
           ,@body))
       (add-hook ,hook #',name))))

(defun lyn-accessible-directory-p (filename)
  "As ‘file-accessible-directory-p’ for FILENAME but work around an Apple bug."

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
  "Return a “most relevant” directory for FILE.

Relative to function ‘projectile-project-root’ if ‘projectile-project-p’,
otherwise relative to FILE.

See ‘lyn-safe-default-directory’."

  (lyn-safe-default-directory
   (if (projectile-project-p file)
       (projectile-project-root file)
     file)))

(defmacro lyn-with-relevant-dir (file &rest body)
  "Execute BODY with ‘default-directory’ “most relevant” for FILE.

See ‘lyn-relevant-dir’."
  (declare (indent defun) (debug (form body)))

  `(when-let ((default-directory (lyn-relevant-dir ,file)))
     ,@body))

(defconst lyn-fetchhash--sentinel (gensym)
  "Sentinel for ‘lyn-fetchhash’ to detect missing values.")
(defmacro lyn-fetchhash (key table set-when-default)
  "Look up KEY in TABLE and return value or assign SET-WHEN-DEFAULT and return."

  (lyn-once-only (key table)
    (lyn-with-gensyms (cached)
      `(let ((,cached (gethash ,key ,table lyn-fetchhash--sentinel)))
         (if (eq ,cached lyn-fetchhash--sentinel)
             (puthash ,key ,set-when-default ,table)
           ,cached)))))

(use-package bind-key)

;;;; Defaults

;; SOMEDAY:
;; Smooth scroll when this doesn't generate an obscene amount of garbage
;; and/or it properly handles the macOS scrolling behaviors

;; (when (and (display-graphic-p)
;;            (fboundp 'pixel-scroll-mode))
;;   (pixel-scroll-mode)
;;   (setf pixel-resolution-fine-flag 1
;;         pixel-dead-time 0
;;         mouse-wheel-progressive-speed t))

(setf
 ;; Decrease work to create autoloads
 autoload-compute-prefixes nil
 ;; Don't ping random machines
 ffap-machine-p-known 'reject
 ;; Control file creation - use version control for version control
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 ;; Customize is terrible (we won't load the file)
 custom-file (concat temporary-file-directory "custom.el")
 ;; Security
 auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")
 gnutls-verify-error t
 tls-checktrust t
 tls-program '("gnutls-cli --x509cafile %t -p %p %h"
               "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
 ;; Typography
 sentence-end-double-space nil
 ;; Editing
 undo-limit (* 16 1024 1024)
 undo-strong-limit (* 24 1024 1024)
 ;; Delay font lock to speed up interaction
 fast-but-imprecise-scrolling t
 jit-lock-defer-time 0
 ;; OS integration
 use-dialog-box nil
 read-process-output-max (* 1 1024 1024) ; 1 MB
 ;; Disable tabs almost everywhere
 (default-value 'indent-tabs-mode) nil)

(bind-key "C-?" #'undo-only)
(when (boundp 'mac-option-modifier) (setf mac-option-modifier 'meta))
(when (boundp 'mac-command-modifier) (setf mac-command-modifier 'super))
(when (fboundp 'ns-next-frame) (bind-key "s-`" #'ns-next-frame))

(use-package imenu :straight (:type built-in)
  :bind (("C-c i" . imenu)))

;;;; Editing

;; ‘comment-dwim-2’ provides a slightly-smarter version of ‘comment-dwim’
(use-package comment-dwim-2
  :bind (([remap comment-dwim] . comment-dwim-2)))

(use-package editorconfig
  :commands (editorconfig-mode)
  :init
  (lyn-with-hook-once 'find-file-hook
    (editorconfig-mode)
    (editorconfig-apply)))

;; ‘electric-mode’ provides smart quotes used in elisp documentation
(use-package electric :straight (:type built-in)
  :hook (emacs-lisp-mode . electric-quote-local-mode))

;; ‘uniquify’ determines how buffer names are made distinct when visiting
;; files with the same name in different directories
(use-package uniquify :straight (:type built-in)
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
  :hook (prog-mode . company-mode)
  :custom
  (company-capf t)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1))

(use-package company-statistics
  :after company
  :config (company-statistics-mode))

(use-package dired :straight (:type built-in)
  :defer
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (wdired-allow-to-change-permissions t))

(use-package dired-x :straight (:type built-in)
  :init
  (with-eval-after-load 'dired
    (require 'dired-x))
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(use-package freeze-it
  :bind (("C-c w d" . freeze-it-mode)))

(use-package magit
  :defer
  :init
  (lyn-with-hook-once 'find-file-hook
    (require 'magit))
  :custom
  (magit-list-refs-sortby "-committerdate")
  (magit-diff-adjust-tab-width t))

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

    (lyn-with-relevant-dir nil
      (if dedicated
          (multi-term-dedicated-open)
        (multi-term))))
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
  :hook (org-mode . turn-on-olivetti-mode)
  :bind (("C-c w o" . olivetti-mode)))

(use-package paren :straight (:type built-in)
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
                ("C-c p" . projectile-command-map))
  :custom (projectile-completion-system 'default))

(use-package tramp :straight (:type built-in)
  :defer
  :init
  (lyn-with-hook-once 'post-self-insert-hook
    (require 'tramp)))

;;;; Major Modes

(use-package d-mode
  :mode (rx ".d" (opt "i") string-end))

(use-package elm-mode
  :commands (company-elm)
  :mode (rx ".elm" string-end)
  :config (add-to-list 'company-backends #'company-elm)
  :custom (elm-format-on-save t))

(use-package haml-mode
  :mode (rx ".haml" string-end))

(use-package json-mode
  :mode (rx ".json" string-end))

(use-package nov
  :mode ((rx ".epub" string-end) . nov-mode)
  :custom (nov-text-width 60))

(use-package org
  :mode ((rx ".org" string-end) . org-mode)
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

(use-package pico8-mode :straight (:host github :repo "Kaali/pico8-mode")
  :mode (rx ".p8" string-end)
  :custom
  (pico8-documentation-file
   (cond ((memq window-system '(mac ns))
          (expand-file-name "~/Library/Application Support/pico-8/pico-8.txt"))
         nil))
  :custom-face
  (pico8--non-lua-overlay ((t (:inherit default)))))

(use-package ruby-mode :straight (:type built-in)
  :defer
  :custom
  (ruby-align-chained-calls t)
  (ruby-insert-encoding-magic-comment nil))

(use-package rust-mode
  :mode (rx ".rs" string-end))

(use-package yaml-mode
  :mode (rx ".y" (opt "a") "ml" string-end))

(use-package web-mode
  ;; (rx-to-string (cons 'or (cl-loop for (_ . test) in (append web-mode-content-types web-mode-engine-file-regexps)
  ;;                                  unless (string= test ".")
  ;;                                  collect `(regexp ,test)))
  ;;               :no-group)
  :mode (rx "." (or "cjs" "mjs" (seq (any "jt") "s" (opt "x"))) string-end)
  :custom (web-mode-enable-auto-quoting nil))

(use-package zig-mode
  :mode (rx ".zig" string-end))

;;;; Syntax Checking, Linting, and Formatting

;; ‘apheleia’ provides asynchronous autoformatting
(use-package apheleia :straight (:host github :repo "raxod502/apheleia")
  :commands (apheleia-global-mode)
  :init
  (lyn-with-hook-once 'find-file-hook
    (apheleia-global-mode))
  :config
  (setf (alist-get 'clang-format apheleia-formatters) '("clang-format")
        (alist-get 'dfmt apheleia-formatters) '("dfmt" "--brace_style" "otbs")
        (alist-get 'rustfmt apheleia-formatters) '("rustfmt" "--emit" "stdout")
        (alist-get 'c-mode apheleia-mode-alist) 'clang-format
        (alist-get 'd-mode apheleia-mode-alist) 'dfmt
        (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt))

;; ‘flycheck’ provides in-buffer errors, warnings, and syntax checking
(use-package flycheck
  :commands (global-flycheck-mode)
  :init
  (lyn-with-hook-once 'find-file-hook
    (global-flycheck-mode))

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
                     (when-let ((bundle-executable
                                 (lyn-flycheck--bundle-executable dir)))
                       (= 0 (call-process bundle-executable
                                          nil nil nil "show" command))))))

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
        (aset url 1 nil) ; expanded (setf (url-type url) nil), (require 'url) needed to expand
        (apply (cadr (assoc-string (file-name-nondirectory type)
                                   lyn-flycheck-handle-alist))
               (url-recreate-url url)
               (cdr command)))))
  :config
  ;; Override the sh-shellcheck checker's configuration to normalize the shell
  ;; ‘false’ to ‘sh’.
  (defun lyn-flycheck--normalize-sh (symbol)
    "Return 'sh if SYMBOL is 'false, otherwise return SYMBOL."
    (declare (pure t) (side-effect-free t))

    (if (eq symbol 'false)
        'sh
      symbol))
  (cl-loop for elt in (get 'sh-shellcheck 'flycheck-command)
           when (equal (cdr-safe elt) '((symbol-name sh-shell)))
           do (setf (cdr elt)
                    '((symbol-name (lyn-flycheck--normalize-sh sh-shell)))))

  (add-to-list 'flycheck-shellcheck-supported-shells 'false)
  :custom
  ;; Apply flycheck extension from above
  (flycheck-executable-find #'lyn-flycheck-executable-find)
  (flycheck-command-wrapper-function #'lyn-flycheck-command-wrapper)
  ;; Tweak syntax checking
  (flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch mode-enabled))
  (flycheck-display-errors-delay 0))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package lsp-mode
  :hook (prog-mode . lsp)
  :custom
  (lsp-enable-snippet nil)
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-display-inlay-hints t))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; Paths
(use-package add-node-modules-path
  :commands (add-node-modules-path))
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :commands (exec-path-from-shell-initialize)
    :init (add-hook 'after-init-hook #'exec-path-from-shell-initialize)
    :custom (exec-path-from-shell-check-startup-files nil)))

(defvar lyn-original-exec-path exec-path
  "The value of variable ‘exec-path’ when Emacs was first started.")
(defvar lyn-local-exec-path-cache (make-hash-table :test 'equal)
  "Cache for exec paths by project or directory.")
(defun lyn-local-exec-path (&optional drop-cache)
  "Set up variable ‘exec-path’ for the current project.

If DROP-CACHE is non-nil, then recreate ‘lyn-local-exec-path-cache’."
  (interactive "p")

  (when drop-cache
    (setf lyn-local-exec-path-cache (make-hash-table :test 'equal)))

  (when (and (or buffer-file-name                     ; Accessing a file
                 (derived-mode-p 'magit-mode))        ; or running under Magit
             (not (file-remote-p default-directory))) ; File not under Tramp
    (lyn-with-relevant-dir nil
      (make-local-variable 'exec-path)
      (let ((relevant-exec-path
             (let ((exec-path lyn-original-exec-path))
               (lyn-fetchhash
                default-directory lyn-local-exec-path-cache
                (progn
                  (when (fboundp 'exec-path-from-shell-initialize)
                    (exec-path-from-shell-initialize))
                  (add-node-modules-path)
                  exec-path)))))
        (setf exec-path relevant-exec-path)))))
;; HACK: Doing this on every find-file isn't great. Caching makes this slightly
;;       less terrible but it's still terrible. It would be preferable if
;;       there was a shim-based solution which ran according to the current
;;       directory instead of needing to futz with the path constantly.
(add-hook 'find-file-hook #'lyn-local-exec-path)
(add-hook 'magit-pre-display-buffer-hook #'lyn-local-exec-path)

;;;; Searching

(use-package avy
  :bind (("C-c c" . avy-goto-char-2)
         ("C-c l" . avy-goto-line)
         ("C-c r" . avy-goto-char-2-above)
         ("C-c s" . avy-goto-char-2-below)))

(use-package ctrlf :straight (:host github :repo "raxod502/ctrlf")
  :commands (ctrlf-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (require 'map) ; ctrlf doesn't require map like it should
    (ctrlf-mode))
  (defun lyn-disable-ctrlf-mode-in-buffer ()
    "Disable function ‘ctrlf-mode’ for the current buffer."

    (ctrlf-local-mode -1))
  (add-hook 'term-mode-hook #'lyn-disable-ctrlf-mode-in-buffer))

(use-package deadgrep
  :bind (("C-c g" . deadgrep)))

(use-package selectrum :straight (:host github :repo "raxod502/selectrum")
  :commands (selectrum-mode)
  :init
  (lyn-with-hook-once 'pre-command-hook
    (selectrum-mode))
  :config
  (setf extended-command-suggest-shorter nil))

(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
             :files ("selectrum-prescient.el"))
  :after selectrum
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

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

(use-package winner :straight (:type built-in)
  :commands (winner-mode)
  :init
  (lyn-with-hook-once 'window-configuration-change-hook
    (winner-mode)))

;;;; Profiling

;; ;; ‘explain-pause-mode’ does constant profiling to find out why Emacs is slow
;; (use-package explain-pause-mode :straight (:host github :repo "lastquestion/explain-pause-mode")
;;   :commands (explain-pause-mode)
;;   :init (explain-pause-mode))

(provide 'init)
;;; init.el ends here
