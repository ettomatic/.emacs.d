;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package modus-themes
;;   :init
;;   (setq modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia))
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

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

(setq leuven-scale-outline-headlines nil)
(setq leuven-scale-org-agenda-structure nil)
(setq leuven-scale-volatile-highlight nil)

(provide 'init-theme)
;;; init-theme ends here
