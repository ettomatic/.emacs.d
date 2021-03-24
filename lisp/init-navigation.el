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
  (projectile-completion-system 'default)
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

(use-package orderless
  :ensure t)

(setq completion-styles '(orderless))

(use-package marginalia
  :ensure
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit 'keep-selected)))))

(use-package selectrum
  :ensure t)

;;; Prescient as default Sorting & Filtering tool
(use-package prescient
  :ensure t)

(use-package selectrum-prescient
  :ensure t
  :after selectrum)

(setq selectrum-prescient-enable-filtering nil)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(setq orderless-skip-highlighting (lambda () selectrum-is-active))
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

(recentf-mode 1)

(use-package consult
  :ensure t
  :config
  :bind (([C-tab] . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-s" . consult-line)
         ("C-x C-a" . consult-ripgrep)
         ("M-g M-g" . consult-goto-line)
         ("M-y" . consult-yank-pop)))

(require 'consult)

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))
(setq prescient-sort-length-enable nil)

;;; Avy
(use-package avy
  :ensure t)

(global-set-key (kbd "s-;") 'avy-goto-char-2)

(provide 'init-navigation)
;;; init-navigation ends here
