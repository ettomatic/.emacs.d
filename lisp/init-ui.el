;;; init-ui --- UI & Appearance Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
(if is-mac
    (set-face-attribute 'default nil
                        :family "JuliaMono"
                        :height 130)
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 100))

(custom-theme-set-faces
 'user
 '(org-meta-line ((t (:inherit default))))
 '(org-special-keyword ((t (:inherit default))))
 '(org-date ((t (:inherit default))))
 '(org-block ((t (:inherit default))))
 '(org-block-begin-line ((t (:inherit default)))))

;; size & scaling
(setq text-scale-mode-step 1.05)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(setq-default line-spacing 0.2)

(if is-gui
    (setq default-frame-alist '((width . 160)
                                (height . 60))))

(setq select-enable-clipboard t
      select-enable-primary t
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

(setq default-frame-alist '((cursor-color . "white")))

;;; When you try to align your Emacs frame flush on macOS and it just doesn’t quite reach the edge
(if is-mac
    (setq frame-resize-pixelwise t))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-height 10)

;; Show a Dashboard at startup
(require 'dashboard)
(dashboard-setup-startup-hook)
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

(provide 'init-ui)
;;; init-ui ends here
