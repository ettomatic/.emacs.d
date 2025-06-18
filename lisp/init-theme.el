;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  ;; (setq modus-themes-common-palette-overrides
  ;;       modus-themes-preset-overrides-intense)

  (setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tritanopia :no-confirm)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-variable-pitch-ui t
      modus-themes-mixed-fonts t)

;; Remove the border
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

;; Color customizations
(setq modus-themes-prompts '(bold))
(setq modus-themes-completions nil)
(setq modus-themes-org-blocks 'gray-background)

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