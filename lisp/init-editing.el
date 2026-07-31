;;; init-editing --- Editing configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable Mouse Support from terminal
(xterm-mouse-mode +1)

;; Spell checker
(use-package jinx
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  ;; avoid M-$ (Cmd-Shift-4 via mac-command-modifier), clashes with macOS screenshot
  :bind (("C-c s" . jinx-correct))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.1))

;;; Highlights delimiters such as parentheses, brackets
;;; or braces according to their dept
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; get rid of white spaces at the end of the line
(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

;;; move the current line using M-up / M-down (or any other bindings
;;; you choose) if a region is marked, it will move the region
;;; instead.
(use-package move-text
  :ensure t
  :init
  (move-text-default-bindings))

;; operate on the current line if no region is active
(use-package whole-line-or-region
  :ensure t
  :init (whole-line-or-region-global-mode))

(global-set-key (kbd "C-c k") 'kill-whole-line)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; remember last position
(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom
  (save-place-file (concat no-littering-var-directory "places")))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Duplicate the current line, or the active region N times (prefix arg).
(global-set-key (kbd "C-c d") 'duplicate-dwim)

(when is-mac
  (setq mac-command-modifier       'meta)
  (setq mac-option-modifier        'meta)
  ;; (setq mac-option-key-is-meta t)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-function-modifier      'super))

;; (use-package kkp
;;   :ensure t
;;   :config
;;   ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
;;   (global-kkp-mode +1))

(use-package vundo
  :ensure t
  :config
  (global-set-key (kbd "C-x u") 'vundo))

(provide 'init-editing)
;;; init-editing ends here
