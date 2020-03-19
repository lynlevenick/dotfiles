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

  (let ((prev-args-sym (gensym))
        (prev-value-sym (gensym)))
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

  (let ((prev-value-sym (gensym)))
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
memoized function, like, e.g. ‘:strategy STRATEGY’.

:strategy  Strategy to be used during memoization. One of the
           symbols ‘one’ (the default), ‘buffer-one’, or
           ‘buffer-contents’."

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
          (cl-typecase func
            (function
             (if-let ((strategy (or (plist-get props :strategy) 'one))
                      (strategy-func (intern (concat "memo-memoize--" (symbol-name strategy))))
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
     (+ (* c1b alpha) (* c2b inv-alpha)))))

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

  (concat "\"" (make-vector width character) "\""))

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
    (width height fill-start-frac fill-start fill-end fill-end-frac on-color off-color)
  "Generate WIDTH by HEIGHT xpm image.

Highlight from FILL-START to FILL-END with ON-COLOR against OFF-COLOR.
FILL-START-FRAC and FILL-END-FRAC set the fraction of coverage along
the edges of the highlighted area."
  :strategy 'buffer-one

  (let* ((height- (1- height))
         (frac-start-color (lyn-color-blend on-color off-color fill-start-frac))
         (frac-end-color (lyn-color-blend on-color off-color fill-end-frac))
         (data (list (lyn-xpm-header width height off-color frac-start-color on-color frac-end-color)))
         (line-on (lyn-hud--xpm-row-on width))
         (line-off (lyn-hud--xpm-row-off width)))
    (dotimes (i height)
      (push (cond
             ((and (= i (1- fill-start))
                   (> fill-start-frac 0))
              (lyn-hud--xpm-row-start-frac width))
             ((<= fill-start i fill-end) line-on)
             ((and (= i (1+ fill-end))
                   (> fill-end-frac 0))
              (lyn-hud--xpm-row-end-frac width))
             (line-off))
            data)
      (push (if (< i height-)
                ","
              "};")
            data))
    (create-image (apply #'concat (nreverse data))
                  'xpm t :ascent 'center)))

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

  (let* ((height (frame-char-height))
         (height- (1- height))
         (last-line (float (lyn-hud--last-line-number)))
         ;; Exact area coverage
         (fill-start-exact (* height- (/ (1- (lyn-hud--line-number-at-point (window-start))) last-line)))
         (fill-end-exact (* height- (/ (lyn-hud--line-number-at-point (window-end)) last-line)))
         ;; Complete area coverage (no fractional part)
         (fill-start (ceiling fill-start-exact))
         (fill-end (floor fill-end-exact))
         ;; Fractional area coverage (rounded to reduce redraw/allocations)
         (fill-start-frac (/ (round (- fill-start fill-start-exact) 0.125) 8.0))
         (fill-end-frac (/ (round (- fill-end-exact fill-end) 0.125) 8.0)))
    (lyn-hud--xpm (* 2 (frame-char-width)) height
                  fill-start-frac fill-start fill-end fill-end-frac
                  "#FBB829" "#444444")))

(defconst lyn-mode-line-hud
  '(:eval
    (when (display-graphic-p)
      (concat (propertize "hud" 'display (lyn-hud)) " ")))
  "Ending mode line segment in graphical mode.")

(setf (default-value 'mode-line-format)
      (cl-set-difference mode-line-format
                         '(mode-line-front-space mode-line-mule-info mode-line-client mode-line-end-spaces)))
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
