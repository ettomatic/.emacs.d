;;; init --- startup conf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

(require 'init-elpa)
(require 'init-defaults)
(require 'init-ui)
(require 'init-editing)
(require 'init-navigation)
(require 'init-org)
(require 'init-company-mode)
(require 'init-langs)
(require 'init-ocaml)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-version-control)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fish-mode use-package-hydra use-package-ensure-system-package smex rspec-mode rg rainbow-delimiters projectile polymode org-journal org-cliplink org-brain ob-elixir magit inf-ruby ido-completing-read+ idle-highlight-mode golden-ratio flycheck enh-ruby-mode elixir-mode eglot doom-themes delight crystal-mode company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init ends here
