;;; init-ui --- UI & Appearance Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ayu-theme
  :ensure t
  :config (load-theme 'ayu-dark t))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-face-attribute 'default nil :height 100)
(setq-default line-spacing 0.2)

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(blink-cursor-mode 1)
(set-cursor-color "#cccccc")
(setq ring-bell-function 'ignore)

(provide 'init-ui)
;;; init-ui ends here
