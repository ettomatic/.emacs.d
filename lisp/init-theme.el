;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :ensure t
  ;; Customizations must be set prior to loading the theme.
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  ;; Maybe define some palette overrides, such as by using our presets
  ;; modus-themes-preset-overrides-intense
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)))
  :config
  (load-theme 'modus-operandi-tritanopia :no-confirm)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;;   :config
;;   (load-theme 'leuven t))

;; (setq leuven-scale-outline-headlines nil)
;; (setq leuven-scale-org-agenda-structure nil)
;; (setq leuven-scale-volatile-highlight nil)

;; (use-package ef-themes
;;   :config
;;   (ef-themes-select 'ef-spring))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'color-theme-sanityinc-tomorrow-day t))

;; (use-package acme-theme
;;   :config
;;   (load-theme 'acme t))

;; (use-package catppuccin-theme)
;; (setq catppuccin-flavor 'mocha) ;; or 'frappe, 'macchiato, 'latte or 'mocha
;; (catppuccin-reload)

;; ;; (use-package dakrone-light-theme
;; ;;   :ensure t
;; ;;   :init
;; ;;   (load-theme 'dakrone-light t))

(provide 'init-theme)
;;; init-theme ends here