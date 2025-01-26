;;; init-langs-utils --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package paredit
  :ensure t
  :defer t)

;;; ls-server
(use-package eglot
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook 'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("~/.opam/default/bin/ocamllsp")))
  (put 'tuareg-mode 'eglot-language-id "ocaml"))

(use-package devdocs
  :ensure t
  :defer t)

(global-set-key (kbd "C-h C-d") 'devdocs-lookup)
(add-hook 'enh-ruby-mode-hook
          (lambda () (setq-local devdocs-current-docs '("ruby~3.3"))))
(add-hook 'elixir-mode-hook
          (lambda () (setq-local devdocs-current-docs '("elixir~1.18"))))


(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(provide 'init-langs-utils)
;;; init-langs-utils ends here
