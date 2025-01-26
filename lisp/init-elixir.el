;;; init-elixir --- Elixir & Erlang Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exunit
  :ensure t
  :defer t
  :after (elixir-mode))

(use-package elixir-ts-mode
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-ts-mode-hook 'exunit-mode))


;; Language Servers
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(elixir-ts-mode . "~/code/ls-servers/elixir-ls/release/language_server.sh"))

;;; org-mode
(use-package ob-elixir
  :ensure t
  :defer t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (elixir . t)))
  :after org)

(provide 'init-elixir)
;;; init-elixir ends here
