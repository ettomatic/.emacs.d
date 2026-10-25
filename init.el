;;; init --- startup conf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  :config
  (dolist (var '("CALDAV_URL" "CALDAV_ID"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Package management must load first: later files use `use-package' with `:ensure'.
(require 'init-elpa)
(require 'init-utils)
(require 'init-defaults)

(require 'init-theme)
(require 'init-fonts)
(require 'init-ui)

(require 'init-navigation)
(require 'init-editing)
(require 'init-completion)
(require 'init-markup)
;; (require 'init-writing)

(require 'init-org)
(require 'init-notes)
;; (require 'init-org-extra)
(require 'init-org-agenda)
;; (require 'init-org-roam)
;; (require 'init-presentation)

(require 'init-shell)
;; (require 'init-ai)

(require 'init-langs-utils)
(require 'init-version-control)
(require 'init-elixir)
(require 'init-ruby)
;; (require 'init-langs-other)

;(require 'init-docker)

(require 'init-help)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)
;;; init ends here
