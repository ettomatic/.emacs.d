;;; init-shell --- Shells management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell-bookmark
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

(defun eb/eshell-prompt ()
  (concat
   (propertize "λ" 'face `(:foreground "#aece4a"))
   (propertize " " 'face `(:foreground "black"))))

(setq eshell-prompt-function      'eb/eshell-prompt
      eshell-prompt-regexp        "^λ "
      eshell-history-size         10000
      eshell-buffer-maximum-lines 10000
      eshell-hist-ignoredups t
      eshell-highlight-prompt t
      eshell-scroll-to-bottom-on-input t
      eshell-prefer-lisp-functions nil)


;; We want to use xterm-256color when running interactive commands
;; in eshell but not during other times when we might be launching
;; a shell command to gather its output.
(add-hook 'eshell-pre-command-hook
          '(lambda () (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-post-command-hook
          '(lambda () (setenv "TERM" "dumb")))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.8)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package eat)
;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)
;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)


(provide 'init-shell)
;;; init-shell ends here
