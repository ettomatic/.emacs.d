(load "~/.emacs.d/opam-user-setup.el")
(load "~/.opam/default/share/emacs/site-lisp/tuareg-site-file.el")
(add-to-list 'load-path "~/.opam/default/share/emacs/site-lisp/")

(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;(add-to-list 'eglot-server-programs '(caml-mode . ("~/.opam/default/bin/ocamllsp" "")))

(provide 'init-ocaml)
