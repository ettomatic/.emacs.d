;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :ensure t
  ;; Customizations must be set prior to loading the theme.
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-disable-other-themes t)
  ;; Maybe define some palette overrides, such as by using our presets
  ;; modus-themes-preset-overrides-intense
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)))
  :config
;;(load-theme 'modus-operandi-tritanopia :no-confirm)
  (load-theme 'modus-vivendi-deuteranopia :no-confirm)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  (set-fringe-mode 0))

(provide 'init-theme)
;;; init-theme.el ends here
