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

(require 'init-navigation)
(require 'init-editing)
(require 'init-markup)
(require 'init-writing)

(require 'init-org)
(require 'init-org-agenda)
;(require 'init-presentation)

(require 'init-langs)
;(require 'init-ocaml)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-company-mode)
(require 'init-version-control)
(require 'init-docker)
(require 'init-shell)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)
;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(zoom whole-line-or-region use-package-ensure-system-package undo-tree solo-jazz-theme selectrum-prescient rspec-mode rg rainbow-delimiters racket-mode projectile org-roam org-present org-cliplink org-bullets org-brain org-appear orderless olivetti ob-elixir move-text modus-themes markdown-mode marginalia magit langtool kaolin-themes inf-ruby golden-ratio git-timemachine flycheck fish-mode fish-completion fira-code-mode exunit exec-path-from-shell excorporate eshell-syntax-highlighting eshell-bookmark esh-autosuggest enh-ruby-mode embark-consult elpher eglot doom-modeline dockerfile-mode docker-compose-mode docker diff-hl delight deft dashboard crystal-mode company-prescient ayu-theme alchemist ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
