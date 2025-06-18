;;; init-writing --- Markups Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; Writing space
;;

;; Distraction-free screen
(use-package olivetti
  :ensure t
  ;:defer t
  :init
  (setq olivetti-body-width 74)
  :config
  (defun writing-mode ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (variable-pitch-mode t)
          (visual-line-mode t)
          (auto-fill-mode t)
          (global-hl-line-mode 0)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (variable-pitch-mode 0)
        (global-hl-line-mode 1)
        (text-scale-decrease 2))))
    :bind
    (("<f9>" . writing-mode)))

;;; Should I use Spell-fu? See init-editing
;; (customize-set-variable 'ispell-program-name "aspell")
;; (customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))

;; (add-hook
;;  'olivetti-mode-hook
;;  'flyspell-mode)

;; (use-package langtool
;;   :defer t
;;   :config
;;   (setq langtool-http-server-host "localhost"
;;         langtool-http-server-port 8010)
;;   (setq langtool-default-language "en-GB")
;;   (setq langtool-mother-tongue "it"))

(provide 'init-writing)
;;; init-writing ends here
