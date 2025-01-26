;;; init-langs-other --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Crystal Lang
(use-package crystal-mode
  :ensure t
  :defer t)

(use-package paredit
  :ensure t
  :defer t)

;; (use-package racket-mode
;;   :ensure t
;;   :defer t)

;; (use-package geiser-guile
;;   :ensure t
;;   :defer t)

;; (use-package php-mode
;;   :ensure t
;;   :defer t)

;; (require 'roc-mode)


;; F#
(use-package fsharp-mode
 :defer t
 :ensure t)
(use-package eglot-fsharp
 :defer t
 :ensure t)
(require 'eglot-fsharp)

(add-hook 'fsharp-mode-hook 'eglot-ensure)
(add-hook 'fsharp-mode-hook
          (lambda () (setq-local eldoc-echo-area-display-truncation-message nil)))

(use-package highlight-indentation
  :defer t
  :ensure t)
(add-hook 'fsharp-mode-hook 'highlight-indentation-mode)

(provide 'init-langs-other)
;;; init-langs-other ends here
