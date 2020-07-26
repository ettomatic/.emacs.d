(use-package magit
  :ensure t
  :defer 0.3
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(provide 'init-version-control)
