;;; init --- startup conf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)
(require 'init-defaults)

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

;;  Ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'init-theme)
(require 'init-ui)

(require 'init-navigation)
(require 'init-editing)
(require 'init-markup)
(require 'init-writing)

(require 'init-org)
(require 'init-org-agenda)
(require 'init-org-roam)
(require 'init-presentation)

(require 'init-langs)
;(require 'init-ocaml)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-company-mode)
(require 'init-version-control)
(require 'init-docker)
(require 'init-shell)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)
;;; init ends here
