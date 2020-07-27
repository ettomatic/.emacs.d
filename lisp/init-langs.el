;;; init-lang --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ls-server
(use-package eglot
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'eglot-ensure)
  )

(add-to-list 'eglot-server-programs `(elixir-mode "~/code/elixir-ls/release/language_server.sh"))
(add-to-list 'eglot-server-programs '(caml-mode . ("~/.opam/default/bin/ocamllsp" "")))

;;; Crystal Lang
(use-package crystal-mode
  :ensure t)

(provide 'init-langs)
;;; init-langs ends here
