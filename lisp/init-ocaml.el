;;; init-ocaml --- Ocaml Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load "~/.emacs.d/opam-user-setup.el")
(load "~/.opam/default/share/emacs/site-lisp/tuareg-site-file.el")
(add-to-list 'load-path "~/.opam/default/share/emacs/site-lisp/")

(require 'ocp-indent)

(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

(provide 'init-ocaml)
;;; init-ocaml ends here
