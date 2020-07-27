;;; init-elixir --- Elixir & Erlang Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :defer t)

;;; org-mode
(use-package ob-elixir
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)))
  :after org)

(provide 'init-elixir)
;;; init-elixir ends here
