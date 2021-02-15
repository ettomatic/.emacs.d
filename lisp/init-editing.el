;;; init-editing --- Editing configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'saveplace)

;;; Flycheck lints warnings and errors directly within buffers.
(use-package flycheck
  :defer 2
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  :config)

;;; Highlights delimiters such as parentheses, brackets
;;; or braces according to their dept
(use-package rainbow-delimiters
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

;; (use-package popup-kill-ring
;;   :ensure t
;;   :bind (("M-y" . popup-kill-ring)))

;; operate on the current line if no region is active
(use-package whole-line-or-region
  :ensure t
  :init (whole-line-or-region-global-mode))

;; YAML
(use-package yaml-mode
  :ensure t
  :defer t)

;;
;; Writing space
;;
(use-package olivetti
  :ensure t
  :defer t)

(add-hook
 'olivetti-mode-hook
 'auto-fill-mode)

(add-hook
 'olivetti-mode-hook
 'olivetti-mode)

(add-hook
 'olivetti-mode-hook
 'variable-pitch-mode)

(add-hook
 'olivetti-mode-hook
 'toggle-word-wrap 1)

(customize-set-variable 'ispell-program-name "aspell")
(customize-set-variable 'ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'olivetti-mode-hook 'flyspell-mode)

;; Markdown with preview
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;; Highlights matching parenthesis
(show-paren-mode 1)

;;; Highlight current line
(global-hl-line-mode 1)

;;; Interactive search key bindings. By default, C-s runs
;;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key global-map (kbd "RET") 'newline-and-indent)

;;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; When you visit a file, point goes to the last place where it
;;; was when you previously visited the same file.
;;; http://www.emacswiki.org/emacs/SavePlace
(setq-default save-place t)

;;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;;; Emacs can automatically create backup files. This tells Emacs to
;;; put all backups in ~/.emacs.d/backups. More info:
;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
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

(when *is-a-mac*
  (setq mac-option-modifier        'meta)
  (setq mac-command-modifier       'super)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-function-modifier      'super))

(provide 'init-editing)
;;; init-editing ends here
