;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package modus-themes
;;   :init
;;   (setq modus-themes-tuo-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
;;   ;; ... setting all variable that need to
;;   ;; be present before loading the theme ...
;;   :config
;;   (load-theme (car modus-themes-to-toggle) t t)
;;   :bind ("<f5>" . modus-themes-toggle))

;; (setq modus-themes-italic-constructs t
;;       modus-themes-bold-constructs t
;;       modus-themes-variable-pitch-ui t
;;       modus-themes-mixed-fonts t)

;; ;; Remove the border
;; (setq modus-themes-common-palette-overrides
;;       '((border-mode-line-active unspecified)
;;         (border-mode-line-inactive unspecified)))

;; ;; Color customizations
;; (setq modus-themes-prompts '(bold))
;; (setq modus-themes-completions nil)
;; (setq modus-themes-org-blocks 'gray-background)

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

(use-package catppuccin-theme)
(setq catppuccin-flavor 'macchiato) ;; or 'frappe, 'macchiato, or 'mocha
(catppuccin-reload)

(provide 'init-theme)
;;; init-theme ends here
