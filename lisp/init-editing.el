;;; init-editing --- Editing configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Spell checker
(use-package jinx
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind (("M-$" . jinx-correct))
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


;;; Highlights matching parenthesis
(show-paren-mode 1)

;;; Highlight current line
(global-hl-line-mode 1)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; remember last position
(use-package saveplace
  :hook (after-init . save-place-mode))
(setq save-place-file (concat no-littering-var-directory "places"))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(when is-mac
  (setq mac-option-modifier        'super)
  (setq mac-command-modifier       'meta)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-function-modifier      'super))

;; undo tree (q to exit)
;; (use-package undo-tree
;;   :ensure t
;;   :bind ("C-x u" . undo-tree-visualize)
;;   :diminish undo-tree-mode
;;   :hook (after-init . global-undo-tree-mode)
;;   :init
;;   (setq undo-tree-visualizer-relative-timestamps t
;;         undo-tree-visualizer-diff t))

(provide 'init-editing)
;;; init-editing ends here
