;;; init-navigation --- Navigation configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Projectile is a project interaction library for Emacs
(use-package projectile
  :ensure t
  :defer 1
  :preface
  :custom
  (projectile-keymap-prefix (kbd "C-c C-p"))
  (projectile-mode-line '(:eval (projectile-project-name)))
  (projectile-completion-system 'ivy)
  :config (projectile-mode))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enable move point from window to window using Shift and the arrow keys
(windmove-default-keybindings)

(use-package dired
  :ensure nil
  :delight "Dired "
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(use-package rg
  :defer t
  :init
  (rg-enable-default-bindings))

;;; Global
;; Ivy is a generic completion tool
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :defer 0.9
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (use-package swiper
    :ensure t
    :bind (("C-s" . swiper)))
  (use-package counsel
    :ensure t
    :diminish counsel-mode
    :config (counsel-mode)
    :bind (("C-c s" . counsel-rg) ; ripgrep
           ("M-x"   . counsel-M-x)
           ("C-c b" . counsel-bookmark)
           :map ivy-minibuffer-map
            ("<return>" . ivy-alt-done)))
  (use-package ivy-avy
    :ensure t)
  (ivy-mode))

;; A convenient interface to your recently and most frequently used commands
;; Will work with ivy out of the box
;; A fork of Smex
(use-package amx
  :ensure t
  :config
  (amx-mode 1))

;; A minor mode that builds a list of recently opened files.
;; This list is is automatically saved across sessions on exiting Emacs
(require 'recentf)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(use-package ivy
  :ensure t)

;; (use-package ivy-prescient
;;   :ensure t
;;   :after counsel
;;   :config
;;   (ivy-prescient-mode 1))

;; (use-package company-prescient
;;   :ensure t)

(provide 'init-navigation)
;;; init-navigation ends here
