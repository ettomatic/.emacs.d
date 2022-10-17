;;; init-clojure --- Clojure & ClojureScript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package inf-clojure
  :ensure t
  :defer t)

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(provide 'init-clojure)
;;; init-clojure ends here
