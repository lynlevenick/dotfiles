;;; init.el --- Package configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; There are many configurations, but this one is mine.
;;; Code:

(defun current-file ()
  "Return currently executing file."

  (cond ((stringp (car-safe current-load-list)) (car current-load-list))
	(load-file-name)
	((buffer-file-name))))
(defun load-relative (name &rest arguments)
  "Load NAME relative to the current file.

ARGUMENTS are passed through to `load'."

  (apply #'load (concat (file-name-directory (current-file)) name) arguments))
(unless (bound-and-true-p early-init-file)
  (load-relative "early-init" nil t))

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
(bind-key "C-a" #'lyn-smarter-move-beginning-of-line)
(use-package flycheck :ensure t
  :hook (prog-mode . flycheck-mode))
(use-package syntax-subword :ensure t
  :hook (prog-mode . syntax-subword-mode)
  :init (setf syntax-subword-skip-spaces 'consistent))
(use-package ws-butler :ensure t
  :delight ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;;;; Interaction
(use-package ace-window :ensure t
  :bind (("C-x o" . ace-window))
  :init (setf aw-background nil
              aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-scope 'frame))
(use-package magit :ensure t
	:bind ("C-c g" . magit-status))
(use-package projectile :ensure t
  :delight projectile-mode
  :hook (after-init . projectile-mode)
  :bind-keymap (("C-c p" . projectile-command-map)))
(use-package windsize :ensure t
  :bind (("C-s-<up>" . windsize-up)
         ("C-s-<down>" . windsize-down)
         ("C-s-<left>" . windsize-left)
         ("C-s-<right>" . windsize-right)
         ("C-s-w" . windsize-up)
         ("C-s-s" . windsize-down)
         ("C-s-a" . windsize-left)
         ("C-s-d" . windsize-right)))

;;;; Major Modes
(use-package haml-mode :ensure t
  :mode "\\.haml\\'")
(setq-default ruby-align-chained-calls t)

;;;; Searching
(use-package counsel :ensure t
  :delight counsel-mode
  :hook (ivy-mode . counsel-mode)
  :init (setf counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color-never '%s' %s"))
(use-package counsel-projectile :ensure t
  :hook (counsel-mode . counsel-projectile-mode)
  :bind (("s-d" . counsel-projectile-find-dir)
         ("s-f" . counsel-projectile-find-file)
         ("s-g" . counsel-projectile-rg)
         ("s-p" . counsel-projectile-switch-project)))
(use-package ivy :ensure t
  :delight ivy-mode
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume))
  :init (setf projectile-completion-system 'ivy))
(use-package swiper :ensure t
  :bind (("C-r" . counsel-grep-or-swiper)
         ("C-s" . counsel-grep-or-swiper)))

(provide 'init)
;;; init.el ends here
