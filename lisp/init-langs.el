;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure t
  :init
  (add-hook 'elixir-mode-hook 'eglot-ensure)
  )

(add-to-list 'eglot-server-programs `(elixir-mode "~/code/elixir-ls/release/language_server.sh"))
;;(require 'eglot)

(use-package crystal-mode
  :ensure t)

;;; org-mode
(use-package ob-ruby :ensure nil :after org)
(provide 'init-langs)
;;; init-langs ends here
