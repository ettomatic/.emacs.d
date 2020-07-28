;;; init-ui --- UI & Appearance Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package ayu-theme
;;   :ensure t
;;   :config (load-theme 'ayu-light t))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-snazzy t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if *is-a-mac*
    (set-face-attribute 'default nil
                        :height 120)
  (set-face-attribute 'default nil
                      :height 100))

(setq-default line-spacing 0.2)

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

(provide 'init-ui)
;;; init-ui ends here
