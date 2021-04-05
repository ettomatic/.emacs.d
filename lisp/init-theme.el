;;; init-theme --- Theme Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(provide 'init-theme)
;;; init-theme ends here
