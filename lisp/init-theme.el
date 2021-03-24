;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Some themes
;; (use-package ayu-theme
;;   :ensure t)

;; (use-package kaolin-themes
;;   :ensure t)

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mode-line 'borderless
        modus-themes-lang-checkers 'subtle)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;; (use-package solo-jazz-theme
;;   :ensure t)

;; disabled themes setup

;; (load-theme 'modus-operandi t)          ; Light theme
;; (load-theme 'modus-vivendi t)           ; Dark theme

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;;(setq *theme-dark* 'doom-acario-dark)
;;(setq *theme-dark* 'doom-snazzy)
;;(setq *theme-dark* 'kaolin-galaxy)
;(setq *theme-dark* 'modus-vivendi)

;;(setq *theme-light* 'ayu-light)
;;(setq *theme-light* 'doom-acario-light)
;;(setq *theme-light* 'solo-jazz)
;;(setq *theme-light* 'modus-operandi)

;; (setq *current-theme* *theme-dark*)

;; (load-theme *current-theme* t)

;; (defun my-fn/next-theme (theme)
;;   (disable-theme *current-theme*)
;;   (load-theme theme t)
;;   (setq *current-theme* theme))

;; (defun my-fn/toggle-theme ()
;;   "Toogle them between light and dark."
;;   (interactive)
;;   (cond ((eq *current-theme* *theme-dark*) (my-fn/next-theme *theme-light*))
;; 	((eq *current-theme* *theme-light*) (my-fn/next-theme *theme-dark*))))

;; (global-set-key (kbd "<f5>") #'my-fn/toggle-theme)

(provide 'init-theme)
;;; init-theme ends here
