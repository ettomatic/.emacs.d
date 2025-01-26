;;; init-org-extra --- Additional Org Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Org Clip

(use-package org-cliplink
  :ensure t
  :config
  (bind-key "C-x p i" 'org-cliplink))

(use-package org-rich-yank
  :ensure t
  :demand t
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("C-x p c" . org-download-clipboard)
         ("C-x p s" . org-download-screenshot)
         ("s-y" . org-download-yank)))
  :config
  (setq org-download-screenshot-method "spectacle")
  (setq-default org-download-image-dir "~/org/img"))

;;; Org Brain
;; (use-package org-brain :ensure t
;;   :ensure t
;;   :init
;;   (setq org-brain-path "~/org/brain")
;;   :config
;;   (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
;;   (setq org-id-track-globally t))

;; (use-package todoist
;;   :ensure t
;;   :config
;;   (setq todoist-token (getenv "TODOIST_API")))

;; (add-to-list 'load-path "~/.emacs.d/org-readwise/")
;; (require 'org-readwise)
;; ;; Ensure auth-source is configured to find your Readwise token
;; (setq auth-sources '("~/.authinfo"))
;; ;; Set the output location for your highlights (buffer or file)
;; (setq org-readwise-output-location "~/org/readwise-highlights.org")
;; ;; Optionally set the debug level (0 = no debug, 1 = basic debug, 2 = detailed debug)
;; (setq org-readwise-debug-level 1)

(provide 'init-org-extra)
;;; init-org-extra ends here
