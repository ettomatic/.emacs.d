;;; init-elpa --- Package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package delight :ensure t)
(use-package use-package-ensure-system-package :ensure t)

(provide 'init-elpa)
;;; init-elpa ends here
