;;; init-version-control --- Git Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :defer 0.3
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(provide 'init-version-control)
;;; init-version-control ends here
