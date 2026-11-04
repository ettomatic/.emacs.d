;;; init-databases --- Databases & SQL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package clutch
     :ensure t
     :defer t)

(use-package ob-clutch
  :vc "https://github.com/LuciusChen/ob-clutch.git"
  :after clutch)

(setq org-babel-default-header-args:sqlite
      '((:db . "~/code/status/priv/belfrage_repo/db.sqlite3")))

(provide 'init-databases)
;;; init-databases ends here
