;;; early-init.el --- override many defaults -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Emacs 27+ introduces early-init.el, run before package and UI initialization

;;; Code:

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
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el" t t)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil t))

(eval-when-compile
  (straight-use-package 'use-package)
  (setf use-package-expand-minimally t
        use-package-verbose nil))

;;;; Unicode

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setf (default-value 'buffer-file-coding-system) 'utf-8-unix)

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
           (not (memq window-system '(mac ns))))
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
  (let ((gensyms (cl-loop for spec in specs collect (gensym "once-only")))
        (names-and-forms
         (cl-loop for spec in specs
                  collect (cl-etypecase spec
                            (list (cl-destructuring-bind (name form) spec
                                    (cons name form)))
                            (symbol (cons spec spec))))))
    ;; Bind gensyms in scope
    `(lyn-with-gensyms (,@(cl-loop for g in gensyms
                                   for (n . _) in names-and-forms
                                   collect `(,g ,n)))
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

(defconst memo--sentinel (gensym)
  "Sentinel value for ‘memo-memoize’ signaling an uncalled method.")

(defun memo-memoize--one (func)
  "Return a memoized FUNC. Remember the most recent invocation."
  (declare (pure t) (side-effect-free t))

  (let ((prev-args memo--sentinel)
        prev-value)
    (lambda (&rest args)
      (if (equal prev-args args)
          prev-value
        (setf prev-args args
              prev-value (apply func args))))))

(defun memo-memoize--buffer-one (func)
  "Return a memoized FUNC. Remember the most recent invocation per buffer."

  (lyn-with-gensyms (prev-args-sym prev-value-sym)
    (eval
     `(progn
        (defvar-local ,prev-args-sym memo--sentinel)
        (defvar-local ,prev-value-sym nil)))
    (lambda (&rest args)
      (if (equal (symbol-value prev-args-sym) args)
          (symbol-value prev-value-sym)
        (set prev-args-sym args)
        (set prev-value-sym (apply func args))))))

(defun memo-memoize--buffer-contents (func)
  "Return a memoized FUNC. Remember results per buffer until buffer change."

  (lyn-with-gensyms (prev-value-sym)
    (eval
     `(defvar-local ,prev-value-sym memo--sentinel))
    (add-hook 'after-change-functions
              (lambda (&rest _) (set prev-value-sym memo--sentinel)))
    (lambda (&rest args)
      (if (eq (symbol-value prev-value-sym) memo--sentinel)
          (set prev-value-sym (apply func args))
        (symbol-value prev-value-sym)))))

(defun memo-memoize (func &rest props)
  "Memoize FUNC.

Optional PROPS are additional properties to apply to the
memoized function, like, e.g. ‘:storage STORAGE’.

:clear-on  When storage is invalidated.
           May be nil or the symbol ‘edit’. Default is nil.
:local     Whether storage is buffer-local.
           May be nil or t. Default is nil.
:storage   Storage to be used during memoization.
           May be one of the symbols ‘latest’ or ‘hash’.
           Default is ‘latest’."

  (let ((func-name nil))
    (when (eq (type-of func) 'symbol)
      (when (get func :memo-memoize-orig-func)
        (user-error "%s is already memoized" func))
      (put func :memo-memoize-orig-docs (documentation func))
      (put func 'function-documentation
           (concat (documentation func) " (memoized)"))
      (put func :memo-memoize-orig-func (symbol-function func))
      (setf func-name func)
      (cl-callf symbol-function func))

    (setf func
          (cl-etypecase func
            (function
             (if-let ((strategy (or (plist-get props :strategy) 'one))
                      (strategy-func (intern (concat "memo-memoize--"
                                                     (symbol-name strategy))))
                      ((functionp strategy-func)))
                 (funcall strategy-func func)
               (user-error "Unknown strategy %s" strategy)))
            (t
             (signal 'wrong-type-argument
                     `("Must be symbol or function"
                       ,func)))))

    (when func-name
      (fset func-name func))))

(defmacro defmemo-memoize (name arglist &rest body)
  "Define NAME as a memoized function.

NAME, ARGLIST, DOCSTRING, DECL and BODY have the same meaning as in ‘defun’.
PROPS has the same meaning as in ‘memo-memoize’.

\(fn NAME ARGLIST &optional DOCSTRING DECL PROPS... &rest BODY)"
  (declare (debug defun) (doc-string 3) (indent 2))

  (let ((body-prefix nil)
        (props nil))
    ;; Take docstring and decl
    (when (eq (type-of (car body)) 'string)
      (push (pop body) body-prefix))
    (when (eq (car-safe (car body)) 'declare)
      (push (pop body) body-prefix))
    (cl-callf reverse body-prefix)
    ;; Take props
    (while (keywordp (car body))
      (push (pop body) props)
      (push (pop body) props))
    (cl-callf reverse props)
    ;; Construct function
    `(prog1
       (defun ,name ,arglist
         ,@body-prefix
         ,@body)
       (memo-memoize (function ,name) ,@props))))

(require 'color)

(defun lyn-parse-color (color)
  "Convert hex COLOR to RGB triplet in [0.0 1.0]."
  (declare (pure t) (side-effect-free t))

  ;; Directly parse 24-bit hex format rather than relying on
  ;; ‘color-name-to-rgb’ as it is both inefficient and doesn't
  ;; work properly in some terminals.
  (if (string-match "#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)" color)
      (list (/ (string-to-number (match-string 1 color) 16) 255.0)
            (/ (string-to-number (match-string 2 color) 16) 255.0)
            (/ (string-to-number (match-string 3 color) 16) 255.0))
    (color-name-to-rgb color)))

(defun lyn-color-blend (c1 c2 alpha)
  "Blend the two colors C1 and C2 with ALPHA.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (declare (pure t) (side-effect-free t))

  (unless (consp c1)
    (cl-callf lyn-parse-color c1))
  (unless (consp c2)
    (cl-callf lyn-parse-color c2))

  (pcase-let ((`(,c1r ,c1g ,c1b) c1)
              (`(,c2r ,c2g ,c2b) c2)
              (inv-alpha (- 1 alpha)))
    (color-rgb-to-hex
     (+ (* c1r alpha) (* c2r inv-alpha))
     (+ (* c1g alpha) (* c2g inv-alpha))
     (+ (* c1b alpha) (* c2b inv-alpha))
     2)))

(defun lyn-xpm-header (width height &rest colors)
  "Return XPM header for an image of WIDTH and HEIGHT.

COLORS are mapped to numbers 0-9."
  (declare (pure t) (side-effect-free t))

  (apply #'concat
         (format "/* XPM */static char*_[]={\"%i %i %i 1\","
                 width height (length colors))
         (cl-loop for color in colors
                  for idx from 0
                  collect (format "\"%i c %s\"," idx color))))

(defun lyn-hud--xpm-row (character width)
  "Return a row of XPM data of WIDTH made up of CHARACTER."
  (declare (pure t) (side-effect-free t))

  (concat "\"" (make-string width character) "\""))

(defmemo-memoize lyn-hud--xpm-row-off (width)
  "Return a row of XPM data of WIDTH made up of the character 0."

  (lyn-hud--xpm-row ?0 width))

(defmemo-memoize lyn-hud--xpm-row-start-frac (width)
  "Return a row of XPM data of WIDTH made up of the character 1."

  (lyn-hud--xpm-row ?1 width))

(defmemo-memoize lyn-hud--xpm-row-on (width)
  "Return a row of XPM data of WIDTH made up of the character 2."

  (lyn-hud--xpm-row ?2 width))

(defmemo-memoize lyn-hud--xpm-row-end-frac (width)
  "Return a row of XPM data of WIDTH made up of the character 3."

  (lyn-hud--xpm-row ?3 width))

(defmemo-memoize lyn-hud--xpm
    (width height hl-start hl-end hl-max on-color off-color)
  "Generate WIDTH by HEIGHT xpm image.

Highlight from HL-START to HL-END within [1 HL-MAX] with ON-COLOR
against OFF-COLOR."
  :strategy 'buffer-one

  (unless (floatp hl-max)
    (cl-callf float hl-max))
  (let* ((height- (1- height))
         (start (* height- (/ (1- hl-start) hl-max)))
         (end (* height- (/ hl-end hl-max)))
         (start- (1- start))
         (end+ (1+ end))
         (start-color (lyn-color-blend on-color off-color
                                       (- (fceiling start) start)))
         (end-color (lyn-color-blend on-color off-color
                                     (- end (ffloor end))))
         (line-on (lyn-hud--xpm-row-on width))
         (line-off (lyn-hud--xpm-row-off width)))
    (concat
     (propertize "??%" 'display
                 (create-image
                  (apply #'concat
                         (lyn-xpm-header width height
                                         off-color start-color on-color end-color)
                         (cl-loop for i below height
                                  collect (cond
                                           ((<= start i end) line-on)
                                           ((< start- i end)
                                            (lyn-hud--xpm-row-start-frac width))
                                           ((< start i end+)
                                            (lyn-hud--xpm-row-end-frac width))
                                           (line-off))
                                  collect (if (< i height-)
                                              ","
                                            "};")))
                  'xpm t :ascent 'center))
     " ")))

(defmemo-memoize lyn-hud--last-line-number ()
  "Return line number at ‘point-max’.

Unreliable if there have been modifications to the buffer
since the last ‘redisplay’.

Does not work for buffers which are not displayed.

Does not work if either ‘line-number-display-limit’ or
‘line-number-display-limit-width’ are exceeded at ‘point-max’."
  (declare (side-effect-free t))
  :strategy 'buffer-contents

  (save-excursion
    (goto-char (point-max))
    (string-to-number (format-mode-line "%l"))))

(defun lyn-hud--line-number-at-point (point)
  "Return line number at POINT.

Unreliable if there have been modifications to the buffer
since the last ‘redisplay’.

Does not work for buffers which are not displayed.

Does not work if either ‘line-number-display-limit’ or
‘line-number-display-limit-width’ are exceeded at POINT."
  (declare (side-effect-free t))

  (save-excursion
    (goto-char point)
    (string-to-number (format-mode-line "%l"))))

(defun lyn-hud ()
  "Return an XPM of relative buffer location."

  (when (display-graphic-p)
    (lyn-hud--xpm (* 2 (frame-char-width)) (frame-char-height)
                  (lyn-hud--line-number-at-point (window-start))
                  (lyn-hud--line-number-at-point (window-end))
                  (lyn-hud--last-line-number)
                  "#FBB829" "#444444")))

(defconst lyn-mode-line-hud
  '(:eval (lyn-hud))
  "Ending mode line segment in graphical mode.")

(setf (default-value 'mode-line-format)
      (cl-set-difference mode-line-format
                         '(mode-line-front-space
                           mode-line-mule-info
                           mode-line-client
                           mode-line-end-spaces)))
(let* ((format (default-value 'mode-line-format))
       (idx (cl-position 'mode-line-position format)))
  (push 'lyn-mode-line-hud (nthcdr idx format)))
(setf mode-line-percent-position nil)

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
(setf (face-font 'variable-pitch)
      (concat "Charter-" (number-to-string lyn-font-size)))

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
