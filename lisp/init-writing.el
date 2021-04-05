;;; init-writing --- Markups Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; Writing space
;;
(use-package olivetti
  :ensure t
  :defer t)

(add-hook
 'olivetti-mode-hook
 'auto-fill-mode)

(add-hook
 'olivetti-mode-hook
 'olivetti-mode)

(add-hook
 'olivetti-mode-hook
 'variable-pitch-mode)

(add-hook
 'olivetti-mode-hook
 'toggle-word-wrap 1)

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'olivetti-mode-hook 'flyspell-mode)

(provide 'init-writing)
;;; init-writing ends here
