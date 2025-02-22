;;; init --- startup conf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-defaults)
(require 'init-utils)
(require 'init-elpa)

;;  Ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'init-theme)
(require 'init-fonts)
(require 'init-ui)

(require 'init-navigation)
(require 'init-editing)
(require 'init-completion)
(require 'init-markup)
(require 'init-writing)

(require 'init-org)
(require 'init-notes)
(require 'init-org-extra)
(require 'init-org-agenda)
(require 'init-org-roam)
(require 'init-presentation)

(require 'init-shell)
(require 'init-ai)

(require 'init-langs-utils)
(require 'init-version-control)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-langs-other)

;(require 'init-docker)

(require 'init-help)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)
;;; init ends here
