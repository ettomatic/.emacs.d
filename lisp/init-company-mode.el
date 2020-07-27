;;; init-company-mode ---  modular in-buffer completion framework  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;  modular in-buffer completion framework
(use-package company
  :defer 0.5
  :delight
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(provide 'init-company-mode)
;;; init-company-mode ends here
