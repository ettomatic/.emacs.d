;;; init --- startup conf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-defaults)

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

;;  Ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'init-theme)
(require 'init-ui)
(require 'init-shell)
(require 'init-editing)
(require 'init-navigation)
(require 'init-org)
(require 'init-org-agenda)
(require 'init-company-mode)
(require 'init-docker)
(require 'init-langs)
(require 'init-ocaml)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-version-control)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(selectrum-mode +1)

(provide 'init)

;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(esh-autosuggest eshell-syntax-highlighting fish-completion alchemist shx eshell-bookmark exunit excorporate git-timemachine dashboard diff-hl undo-tree marginalia doom-modeline all-the-icons moody whole-line-or-region use-package-ensure-system-package solo-jazz-theme selectrum-prescient rspec-mode rg rainbow-delimiters projectile org-roam org-cliplink org-bullets org-brain orderless olivetti ob-elixir move-text modus-themes markdown-mode magit kaolin-themes inf-ruby golden-ratio flycheck fish-mode exec-path-from-shell enh-ruby-mode elpher elixir-mode eglot dockerfile-mode docker-compose-mode docker delight deft crystal-mode consult company-prescient ayu-theme avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "B612 Mono" :height 110 :weight normal :width normal))))
 '(variable-pitch ((t (:family "Spectral" :height 80 :weight thin)))))
