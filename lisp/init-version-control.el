;;; init-version-control --- Git Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :defer 0.3
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;;; highlights uncommitted changes on the left side of the window
;;; allows you to jump between and revert them selectively.
(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; https://github.com/dgutov/diff-hl#magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :ensure t
  :defer t
  :commands (git-timemachine))

(provide 'init-version-control)
;;; init-version-control ends here
