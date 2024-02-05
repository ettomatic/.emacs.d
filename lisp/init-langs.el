;;; init-lang --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ls-server
(use-package eglot
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook 'eglot-ensure)
  (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
  (add-hook 'ruby-ts-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook 'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/code/ls-servers/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs '(ruby-mode enh-ruby-mode ruby-ts-mode . ("solargraph" "socket" "--port" "7658")))
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("~/.opam/default/bin/ocamllsp")))
  (put 'tuareg-mode 'eglot-language-id "ocaml"))

;;; Crystal Lang
(use-package crystal-mode
  :ensure t
  :defer t)

;;; Fish Sell
(use-package fish-mode
  :ensure t
  :defer t)

(use-package paredit
  :ensure t
  :defer t)

(use-package racket-mode
  :ensure t
  :defer t)

(use-package geiser-guile
  :ensure t
  :defer t)

(use-package php-mode
  :ensure t
  :defer t)

(use-package devdocs)
(global-set-key (kbd "C-h C-d") 'devdocs-lookup)
(add-hook 'enh-ruby-mode-hook
          (lambda () (setq-local devdocs-current-docs '("ruby~3.2"))))
(add-hook 'elixir-mode-hook
          (lambda () (setq-local devdocs-current-docs '("elixir~1.14"))))


(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; F#
;; (use-package fsharp-mode
;;  :defer t
;;  :ensure t)
;; (require 'eglot-fsharp)

(provide 'init-langs)
;;; init-langs ends here
