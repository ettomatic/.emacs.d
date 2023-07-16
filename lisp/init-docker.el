;;; init-docker --- Docker management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package docker
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker-compose-mode
  :ensure t
  :defer t)

(provide 'init-docker)
;;; init-docker ends here
