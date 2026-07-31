;;; init-ui --- UI & Appearance Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Menu Bar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(if is-gui
    (setq default-frame-alist '((width . 160)
                                (height . 60))))

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(blink-cursor-mode 1)

;;; Display dividers between windows
;;; Window dividers are bars that can be dragged with the mouse, thus allowing
;;; you to easily resize adjacent windows.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 2)
(add-hook 'window-setup-hook #'window-divider-mode)

;(setq default-frame-alist '((cursor-color . "white")))

;;; When you try to align your Emacs frame flush (normally on macOS) and it just doesn’t quite reach the edge
(setq frame-resize-pixelwise t)

(use-package all-the-icons
  :ensure t)

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 32)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-vcs-max-length 24)
;;   (doom-modeline-time-icon nil)
;;   (doom-modeline-icon t))

(use-package flycheck
  :ensure t
  :defer t)

(use-package mood-line
  ;; Enable mood-line
  :config
  (mood-line-mode)
  ;; Use pretty Fira Code-compatible glyphs
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))


(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner 1)
  (dashboard-projects-switch-function 'projectile-switch-project)
  (dashboard-footer-messages '("Emacs is LISP!"))
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

(use-package mode-line-bell
  :defer t
  :init
  (mode-line-bell-mode))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
   '( :internal-border-width 2
      :header-line-width 4
      :mode-line-width 6
      :custom-button-width 3
      :tab-width 4
      :right-divider-width 4
      :scroll-bar-width 8
      :fringe-width 8))
  :config
  (spacious-padding-mode 1)
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

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
