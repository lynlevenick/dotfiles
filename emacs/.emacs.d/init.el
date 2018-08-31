;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(unless (bound-and-true-p early-init-file)
  (load (concat user-emacs-directory "early-init") nil t))

;;;; Defaults
(def-package! exec-path-from-shell
  :config (exec-path-from-shell-initialize))

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

(def-package! editorconfig
  :config (editorconfig-mode))
(def-package! flycheck
  :hook (prog-mode . flycheck-mode)
  :custom (flycheck-errors-delay 0.25))
(def-package! ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; Interaction
(def-package! ace-window
  :bind (("C-x o" . ace-window))
  :custom
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))
(def-package! avy
  :bind (("C-c l" . avy-goto-line)
         ("C-c s" . avy-goto-char-timer)))
(def-package! company
  :hook (prog-mode . company-mode))
(def-package! magit)
(def-package! projectile
  :demand
  :bind-keymap (("C-c p" . projectile-command-map))
  :config (projectile-mode 1))
(def-package! transpose-frame
  :bind (("C-c t" . transpose-frame)))
(def-package! windsize
  :bind (("C-s-<up>" . windsize-up)
         ("C-s-<down>" . windsize-down)
         ("C-s-<left>" . windsize-left)
         ("C-s-<right>" . windsize-right)
         ("C-s-w" . windsize-up)
         ("C-s-s" . windsize-down)
         ("C-s-a" . windsize-left)
         ("C-s-d" . windsize-right)))

;;;; Languages
(def-package! haml-mode
  :mode "\\.haml\\'")
(setf ruby-align-chained-calls t)

;;;; Searching
(def-package! anzu
  :demand
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode))
(def-package! flx-ido
  :after ido
  :config (flx-ido-mode 1)
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil))
(def-package! ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))
(def-package! ido-vertical-mode
  :after ido
  :config (ido-vertical-mode 1))
(def-package! smex
  :after ido
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))
;; Turn on ido-mode
(ido-mode 1)
(ido-everywhere 1)
(setf magit-completing-read-function #'magit-ido-completion-read)

(provide 'init)
;;; init.el ends here
