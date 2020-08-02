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

(when *is-a-mac*
  (setq mac-option-modifier        'meta)
  (setq mac-command-modifier       'super)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-function-modifier      'super))

(provide 'init-editing)
;;; init-editing ends here
