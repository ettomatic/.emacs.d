;;; init-ui --- UI & Appearance Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Menu Bar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Fonts
(if is-mac
    (set-face-attribute 'default nil
                        :family "JuliaMono"
                        :height 130)
  (set-face-attribute 'default nil
                      :family "iA Writer Mono V"
                      :height 100))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "iA Writer Quattro V"
                    :height 120
                    :weight 'normal)

;(copy-face 'default 'fixed-pitch)

;;( custom-theme-set-faces
 ;; 'user
 ;; '(org-meta-line ((t (:inherit fixed-pitch))))
 ;; '(org-special-keyword ((t (:inherit fixed-pitch))))
 ;; '(org-date ((t (:inherit fixed-pitch))))
 ;; '(org-block ((t (:inherit fixed-pitch))))
 ;; '(org-block-begin-line ((t (:inherit fixed-pitch)))))


;; size & scaling
(setq text-scale-mode-step 1.05)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(setq-default line-spacing 0)

(if is-gui
    (setq default-frame-alist '((width . 160)
                                (height . 60))))

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(blink-cursor-mode 1)


(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))
(global-prettify-symbols-mode +1)


;;; The fringe is a thin strip down the left and/or right edge of a window.
;;; They can contain glyphs to indicate various things
;(fringe-mode '(10 . 1))

;;; Display dividers between windows
;;; Window dividers are bars that can be dragged with the mouse, thus allowing
;;; you to easily resize adjacent windows.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 2)
(add-hook 'window-setup-hook #'window-divider-mode)

(setq default-frame-alist '((cursor-color . "white")))

;;; When you try to align your Emacs frame flush on macOS and it just doesn’t quite reach the edge
(if is-mac
    (setq frame-resize-pixelwise t))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-height 10)

;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 1)
  (setq dashboard-projects-switch-function 'projectile-switch-project)
  (setq dashboard-footer-messages '("Emacs is LISP!")))

;; Show a Dashboard at startup
(require 'dashboard)
(dashboard-setup-startup-hook)

(use-package mode-line-bell
  :defer t
  :init
  (mode-line-bell-mode))

;; Automatic resizing of Emacs windows to the golden ratio
;; (use-package golden-ratio
;;   :ensure t
;;   :diminish golden-ratio-mode
;;   :config
;;   (golden-ratio-mode 1))
;; (add-to-list 'golden-ratio-extra-commands 'ace-window)

;; (use-package zoom
;;   :config
;;   ;; At least 100 cols wide and 3/4 of frame size in height
;;   (setq zoom-size '(100 . 0.75))
;;   (zoom-mode t))

(provide 'init-ui)
;;; init-ui ends here
