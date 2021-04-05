;;; init-elixir --- Elixir & Erlang Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exunit
  :ensure t
  :defer t
  :after (elixir-mode))

(use-package elixir-mode
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook 'exunit-mode))

;;; org-mode
(use-package ob-elixir
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)))
  :after org)

;;; Mostly to have a working iex console
(use-package alchemist
  :ensure t
  :defer t)

(provide 'init-elixir)
;;; init-elixir ends here
