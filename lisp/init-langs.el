;;; init-lang --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ls-server
(use-package eglot
  :ensure t)

(add-to-list 'eglot-server-programs '(elixir-mode "~/code/ls-servers/elixir-ls/release/language_server.sh"))
(add-to-list 'eglot-server-programs '(tuareg-mode . ("~/.opam/default/bin/ocamllsp" "")))

(add-hook 'tuareg-mode-hook 'eglot-ensure)
(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-hook 'fsharp-mode-hook 'eglot-ensure)

;;; Crystal Lang
(use-package crystal-mode
  :ensure t
  :defer t)

;;; Fish Sell
(use-package fish-mode
  :ensure t
  :defer t)

;;; F#
(use-package fsharp-mode
 :defer t
 :ensure t)
(require 'eglot-fsharp)

(provide 'init-langs)
;;; init-langs ends here
