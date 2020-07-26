(require 'ido)
(require 'recentf)
(require-package 'ido-completing-read+)
(require-package 'smex)

;; Projectile is a project interaction library for Emacs
(use-package projectile
  :ensure t
  :defer 1
  :preface
  :custom
  (projectile-keymap-prefix (kbd "C-c C-p"))
  (projectile-mode-line '(:eval (projectile-project-name)))
  :config (projectile-global-mode))


(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)

(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Enable move point from window to window using Shift and the arrow keys
(windmove-default-keybindings)

(provide 'init-navigation)
