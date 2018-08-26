;;; theme.el --- theme everything -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setf frame-title-format nil
      show-paren-delay 0
      ns-use-proxy-icon nil)
(setq-default cursor-type 'bar
              display-line-numbers-type 'relative
              truncate-lines t)
(push '(font . "Menlo-12") default-frame-alist)
(push '(font . "Menlo-12") initial-frame-alist)
(push '(height . 25) default-frame-alist)
(push '(height . 25) initial-frame-alist)
(push '(width . 80) default-frame-alist)
(push '(width . 80) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) initial-frame-alist)

(use-package doom-themes :ensure t
  :init (load-theme 'doom-one t)
  :config
  (doom-themes-visual-bell-config)
  (use-package solaire-mode :ensure t
    :demand
    :hook ((after-revert change-major-mode ediff-prepare-buffer) . turn-on-solaire-mode)
    :config
    (solaire-mode-swap-bg)
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))
  (use-package smart-mode-line :ensure t
    :demand
    :init (setf sml/replacer-regexp-list nil
                sml/theme nil)
    :config (sml/setup)))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)

(provide 'theme)
;;; theme.el ends here
