;;; init-langs-utils --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)))

;;; ls-server
(use-package eglot
  :ensure t
  :init
  ;; Disable the per-server *EGLOT events* log buffers, which otherwise
  ;; default to 2MB each and are only useful for debugging the connection.
  (setq eglot-events-buffer-config '(:size 0 :format full))
  ;; Kill the LSP server process once its last managed buffer is closed,
  ;; instead of leaving it running in the background indefinitely.
  (setq eglot-autoshutdown t))

(use-package devdocs
  :ensure t
  :bind ("C-h C-d" . devdocs-lookup)
  ;; Ruby/Elixir buffers actually use `ruby-ts-mode'/`elixir-ts-mode' (see
  ;; init-ruby.el/init-elixir.el), not `enh-ruby-mode'/`elixir-mode'.
  :hook ((ruby-ts-mode . (lambda () (setq-local devdocs-current-docs '("ruby~3.3"))))
         (elixir-ts-mode . (lambda () (setq-local devdocs-current-docs '("elixir~1.20"))))))

(use-package treesit-auto
  :ensure t
  :custom
  ;; Ask before installing a missing grammar (the actual cause of the earlier
  ;; slowdown was `treesit-auto-add-to-auto-mode-alist' with `all', which forces
  ;; every language into its ts-mode -- and thus an install -- even without a
  ;; grammar; that's not used here, so this alone won't trigger it unprompted).
  (treesit-auto-install 'prompt)
  ;; `treesit-auto-langs' defaults to EVERY language treesit-auto knows (60+),
  ;; and `global-treesit-auto-mode' recomputes readiness for all of them on
  ;; every file visit -- that's what made opening a new file take ~2s;
  ;; scoping it to what's actually installed drops that to ~0.2s. Add a
  ;; language here once its grammar is installed.
  (treesit-auto-langs '(elixir heex ruby))
  :config
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

(provide 'init-langs-utils)
;;; init-langs-utils ends here
