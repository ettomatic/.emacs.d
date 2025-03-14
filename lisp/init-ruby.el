;;; init-ruby -- Ruby Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package enh-ruby-mode
;;   :ensure t
;;   :defer t
;;   :mode (("\\.rb\\'"       . enh-ruby-mode)
;;          ("\\.ru\\'"       . enh-ruby-mode)
;;          ("\\.jbuilder\\'" . enh-ruby-mode)
;;          ("\\.gemspec\\'"  . enh-ruby-mode)
;;          ("\\.rake\\'"     . enh-ruby-mode)
;;          ("Rakefile\\'"    . enh-ruby-mode)
;;          ("Gemfile\\'"     . enh-ruby-mode)
;;          ("Guardfile\\'"   . enh-ruby-mode)
;;          ("Capfile\\'"     . enh-ruby-mode)
;;          ("Vagrantfile\\'" . enh-ruby-mode))
;;   :config (progn
;; 	    (setq enh-ruby-indent-level 2
;; 		  enh-ruby-add-encoding-comment-on-save nil
;; 		  enh-ruby-deep-indent-paren nil
;; 		  enh-ruby-bounce-deep-indent t
;; 		  enh-ruby-hanging-indent-level 2)))

(if is-mac
    (setq enh-ruby-program "~/.asdf/shims/ruby")
  (setq enh-ruby-program "/usr/bin/ruby"))

;; Language Server
(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(ruby-mode enh-ruby-mode ruby-ts-mode . ("solargraph" "socket" "--port" "7658")))

;;; RSpec
(use-package rspec-mode
  :ensure t
  :defer t
  :diminish
  :hook (dired-mode . rspec-dired-mode))

;;; Run a Ruby process in a buffer
(use-package inf-ruby
  :ensure t
  :defer t
  :hook ((enh-ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

;;; Rubocop
(use-package rubocop
  :ensure t
  :defer t)

;;; org-mode
(use-package ob-ruby :ensure nil :after org)

(provide 'init-ruby)
;;; init-ruby ends here
