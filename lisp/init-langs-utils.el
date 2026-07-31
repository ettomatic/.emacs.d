;;; init-langs-utils --- Languages miscellaneous -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Bug with move text up and down via M-up or M-down
;; paredit-splice-sexp: Can’t splice top level
;; see https://emacsredux.com/blog/2026/03/27/paredit-keybinding-conflicts/
;; shoudl move to Smartparens?
;; (use-package paredit
;;   :ensure t
;;   :hook ((emacs-lisp-mode . paredit-mode)
;;          (lisp-interaction-mode . paredit-mode)))

;;; ls-server
(use-package eglot
  :ensure t
  :bind
  (("C-c l i" . eglot-find-implementation)
   ("C-c l e" . eglot)
   ("C-c l k" . eglot-shutdown-all)
   ("C-c l r" . eglot-rename)
   ("C-c l x" . eglot-reconnect)
   ("C-c l a" . eglot-code-actions)
   ("C-c l m" . eglot-menu)
   ("C-c l f" . eglot-format-buffer)
   ("C-c l h" . eglot-inlay-hints-mode))
  :init
  ;; Disable the per-server *EGLOT events* log buffers, which otherwise
  ;; default to 2MB each and are only useful for debugging the connection.
  (setq eglot-events-buffer-config '(:size 0 :format full))
  ;; Kill the LSP server process once its last managed buffer is closed,
  ;; instead of leaving it running in the background indefinitely.
  (setq eglot-autoshutdown t)
  (setq eglot-report-progress t)
  ;; Allows Emacs’ cross-re;; ferencing commands to smoothly transition into
  ;; external library files outside of workspace directory.
  (setq eglot-extend-to-xref t)
  :hook
  (elixir-ts-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (before-save . eglot-format-buffer)
  :config
  ;; Cleans up Emacs 31 visual noise
  (setopt eglot-code-action-indications nil)
  (add-to-list 'eglot-server-programs `(elixir-ts-mode . ("~/code/ls-servers/elixir-ls/language_server.sh")))
  (add-to-list 'eglot-server-programs `(ruby-mode enh-ruby-mode ruby-ts-mode . ("solargraph"
                                                                                "socket"
                                                                                "--port" "7658"))))

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
