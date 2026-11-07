;;; init-elixir --- Elixir & Erlang Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exunit
  :ensure t
  :defer t
  :after (elixir-ts-mode))

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :hook (elixir-ts-mode . exunit-mode))

;;; org-mode
(use-package ob-elixir
  :ensure t
  :defer t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append '((elixir . t)) org-babel-load-languages))
  :after org)

(provide 'init-elixir)
;;; init-elixir ends here
