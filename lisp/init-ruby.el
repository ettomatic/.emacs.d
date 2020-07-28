;;; init-ruby -- Ruby Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; RSpec
(use-package rspec-mode
  :diminish
  :commands rspec-install-snippets
  :hook (dired-mode . rspec-dired-mode)
  :config (with-eval-after-load 'yasnippet
            (rspec-install-snippets)))

;;; Run a Ruby process in a buffer
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(use-package robe
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

;;; org-mode
(use-package ob-ruby :ensure nil :after org)

(provide 'init-ruby)
;;; init-ruby ends here
