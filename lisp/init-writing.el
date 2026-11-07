;;; init-writing --- Markups Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; Writing space NOT USED!
;;

;; Distraction-free screen
(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100)
  (olivetti-margin-width 10)
  :config
  (defvar-local writing-mode--saved-window-config nil)
  (defvar-local writing-mode--saved-text-scale 0)
  (defvar-local writing-mode--saved-hl-line nil)

  (define-minor-mode writing-mode
    "Toggle a distraction-free writing environment."
    :init-value nil
    :lighter " Write"
    :global nil
    (if writing-mode
        (progn
          (setq writing-mode--saved-window-config (current-window-configuration)
                writing-mode--saved-text-scale text-scale-mode-amount
                writing-mode--saved-hl-line global-hl-line-mode)

          (delete-other-windows)
          (variable-pitch-mode 1)
          (visual-line-mode 1)
          (auto-fill-mode 1)
          (global-hl-line-mode -1)
          (text-scale-set 2)
          (olivetti-mode 1))
      (when writing-mode--saved-window-config
        (set-window-configuration writing-mode--saved-window-config))

      (text-scale-set writing-mode--saved-text-scale)
      (variable-pitch-mode -1)
      (visual-line-mode -1)
      (auto-fill-mode -1)
      (olivetti-mode -1)

      (when writing-mode--saved-hl-line
        (global-hl-line-mode 1))

      (setq writing-mode--saved-window-config nil
            writing-mode--saved-text-scale 0
            writing-mode--saved-hl-line nil)))

  (global-set-key (kbd "<f9>") #'writing-mode))

(provide 'init-writing)
;;; init-writing ends here
