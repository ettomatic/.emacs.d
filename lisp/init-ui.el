;;; init-ui --- UI & Appearance Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Some themes
(use-package ayu-theme
  :ensure t)

(use-package kaolin-themes
  :ensure t)

(use-package modus-themes
  :ensure t)

(use-package solo-jazz-theme
  :ensure t)

;; disabled themes setup

;;(load-theme 'modus-operandi t)          ; Light theme
;(load-theme 'modus-vivendi t)           ; Dark theme

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
;; (setq *theme-dark* 'doom-snazzy)
;;(setq *theme-dark* 'modus-operandi)
(setq *theme-dark* 'kaolin-galaxy)

;;(setq *theme-light* 'ayu-light)
;;(setq *theme-light* 'doom-acario-light)
(setq *theme-light* 'solo-jazz)

(setq *current-theme* *theme-dark*)

(load-theme *current-theme* t)

(defun my-fn/next-theme (theme)
  (disable-theme *current-theme*)
  (load-theme theme t)
  (setq *current-theme* theme))

(defun my-fn/toggle-theme ()
  (interactive)
  (cond ((eq *current-theme* *theme-dark*) (my-fn/next-theme *theme-light*))
	((eq *current-theme* *theme-light*) (my-fn/next-theme *theme-dark*))))

(global-set-key (kbd "<f5>") #'my-fn/toggle-theme)

;; Automatic resizing of Emacs windows to the golden ratio
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1))

;; Menu Bar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Fonts
(if *is-a-mac*
    (set-face-attribute 'default nil
                        :family "JuliaMono"
                        :height 130)
  (set-face-attribute 'default nil
                      :height 100))

(setq-default line-spacing 0.2)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Spectral" :height 150 :weight thin))))
 '(fixed-pitch ((t ( :family "B612 Mono":height 110 :weight normal :width normal)))))

(when (or window-system (daemonp))
  (setq default-frame-alist '((width . 160)
                              (height . 50))))

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(blink-cursor-mode 1)

;;; The fringe is a thin strip down the left and/or right edge of a window.
;;; They can contain glyphs to indicate various things
(fringe-mode '(10 . 1))

;;; Display dividers between windows
;;; Window dividers are bars that can be dragged with the mouse, thus allowing
;;; you to easily resize adjacent windows.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 2)
(add-hook 'window-setup-hook #'window-divider-mode)

;;; When you try to align your Emacs frame flush on macOS and it just doesn’t quite reach the edge
(if *is-a-mac*
    (setq frame-resize-pixelwise t))

(provide 'init-ui)
;;; init-ui ends here
