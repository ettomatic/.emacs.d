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
  :init
  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-delete ((t (:background "#ee6363")))))
  ;; On-the-fly diff updates
  (diff-hl-flydiff-mode)
  ;; Enable diff-hl globally
  (global-diff-hl-mode 1)
  :config
  ;; https://github.com/dgutov/diff-hl#magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'diff-hl-mode-on-hook
            (lambda ()
              (unless (display-graphic-p)
                (diff-hl-margin-local-mode)))))

(use-package git-timemachine
  :ensure t
  :defer t
  :commands (git-timemachine))

(use-package magit-todos
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t)

(use-package browse-at-remote
  :ensure t)

(setq auth-sources '("~/.authinfo"))

;; (use-package forge
;;   :after magit)

(provide 'init-version-control)
;;; init-version-control ends here
