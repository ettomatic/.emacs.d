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
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (eglot org-brain use-package-ensure-system-package smex rainbow-delimiters projectile magit ido-completing-read+ golden-ratio flycheck delight crystal-mode company atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init ends here
