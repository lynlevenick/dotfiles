;;; init.el --- Package configuration -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(unless (bound-and-true-p early-init-file)
  (load (concat user-emacs-directory "early-init") nil t))

;;;; Defaults
(def-package! delight)
(def-package! exec-path-from-shell
  :init (exec-path-from-shell-initialize))

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
(global-set-key [remap move-beginning-of-line] #'lyn-smarter-move-beginning-of-line)

(def-package! editorconfig
  :delight
  :hook (prog-mode . editorconfig-mode))
(def-package! flycheck
  :hook (prog-mode . flycheck-mode)
  :custom (flycheck-errors-delay 0.25))
(def-package! syntax-subword
  :hook (prog-mode . syntax-subword-mode)
  :custom (syntax-subword-skip-spaces 'consistent))
(def-package! ws-butler
  :delight
  :hook (prog-mode . ws-butler-mode))

;;;; Interaction
(def-package! ace-window
  :bind (("C-x o" . ace-window))
  :custom
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame))
(def-package! company
  :delight
  :hook (prog-mode . company-mode))
(def-package! magit)
(def-package! projectile
  :delight
  :hook (after-init . projectile-mode)
  :bind-keymap (("C-c p" . projectile-command-map)))
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
(def-package! counsel
  :delight
  :hook (ivy-mode . counsel-mode)
  :custom (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color-never '%s' %s"))
(def-package! counsel-projectile
  :hook (counsel-mode . counsel-projectile-mode)
  :bind (("s-d" . counsel-projectile-find-dir)
         ("s-f" . counsel-projectile-find-file)
         ("s-g" . counsel-projectile-rg)
         ("s-p" . counsel-projectile-switch-project)))
(def-package! ivy
  :delight
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume))
  :custom (projectile-completion-system 'ivy))
(def-package! swiper
  :bind (("C-s" . counsel-grep-or-swiper)))

(provide 'init)
;;; init.el ends here
