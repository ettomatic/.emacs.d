;;; init-navigation --- Navigation configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Projectile is a project interaction library for Emacs
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

;;; Enable move point from window to window using Shift and the arrow keys
;;; doesn't play well with Org
;; (windmove-default-keybindings)
(use-package ace-window
  :ensure t
  :defer t)
(global-set-key (kbd "M-o") 'ace-window)


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

;;; Recent Files

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
;;; exclude all of the files in the no-littering directories
;(setq recentf-exclude '("/\\.emacs\\.d/elpa/" "recentf"))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
;;; If Emacs exits abruptly for some reason the recent file list will be lost
;;;  therefore you may wish to call `recentf-save-list` periodically, e.g. every 5
(run-at-time nil (* 5 60) 'recentf-save-list)

(use-package consult
  :ensure t
  :config
  :bind (([C-tab] . consult-buffer)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-s" . consult-line)
         ("C-x C-a" . consult-ripgrep)
         ("M-g M-g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         ("C-c h" . consult-history)
         ("s-#" .  consult-project-imenu)
         ("M-g o" . consult-outline)))

(require 'consult)

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))
(setq prescient-sort-length-enable nil)

(use-package embark
  :ensure t

  :bind
  (("C-S-a" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;;; Avy
(use-package avy
  :ensure t)

(global-set-key (kbd "C-'") 'avy-goto-char-2)

(provide 'init-navigation)
;;; init-navigation ends here
;;
