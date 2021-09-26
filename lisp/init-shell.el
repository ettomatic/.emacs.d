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
   (propertize " " 'face `(:foreground "white"))))

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


;;; my attempt(s) to work on a fix to use REPLs inside eshell

;; (defun eshell-interactive-output-readonly ()
;;   "Make output of interactive commands in eshell read-only.
;; This should be the last entry in eshell-output-filter-functions!"
;;   (let ((end eshell-last-output-end))
;;     (save-excursion
;;       (goto-char end)
;;       (end-of-line 0)
;;       (setq end (point)))
;;     (when (< eshell-last-output-block-begin end)
;;       (put-text-property eshell-last-output-block-begin end 'read-only "Read-only interactive eshell output"))))

;; (defun eshell-make-interactive-output-readonly ()
;;   (add-hook 'eshell-output-filter-functions 'eshell-interactive-output-readonly 'append))

;; (add-hook 'eshell-mode-hook 'eshell-make-interactive-output-readonly)

;; (add-hook
;;  'eshell-mode-hook
;;  (lambda ()
;;    (setq pcomplete-cycle-completions nil)))

;; (add-hook
;;  'eshell-mode-hook
;;  (lambda ()
;;    (setq eshell-cmpl-cycle-completions nil)))

;; (defadvice comint-output-filter (after output-readonly activate)
;;   "Set last process output read-only."
;;   (add-text-properties comint-last-output-start (line-end-position 0)
;;                '(read-only "Process output is read-only."
;;                    rear-nonsticky (inhibit-line-move-field-capture))))

;; (defun my-eshell-remove-pcomplete ()
;;   (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t))

;; (add-hook 'eshell-mode-hook #'my-eshell-remove-pcomplete)

;; (use-package shx
;;   :ensure t
;;   :defer t)

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))
(provide 'init-shell)
;;; init-shell ends here
