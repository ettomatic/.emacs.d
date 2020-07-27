;;; init-org --- org-mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-directory "~/org")

(use-package org
  :init
  (add-hook 'org-mode-hook #'toggle-word-wrap)
  :custom
  (org-startup-truncated nil)
  (org-tags-column 10)
  (org-tag-alist '(
                   (:startgroup . nil)
                   ("home" . ?h)
                   ("work" . ?w)
                   (:endgroup . nil)
                   (:startgroup . nil)
                   ("@til" . ?t)
                   ("@link" . ?l)
                   ("@phone" . ?p)
                   ("@emacs" . ?e)
                   ("@research" . ?r)
                   (:endgroup . nil)
                   ))
  (org-archive-location "~/org/archives/%s::"))

(use-package org-capture
  :ensure nil
  :after org
  :custom
  (org-capture-templates
   '(("t" "Todo [inbox]" entry
      (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %i%?\n%a")
     ("n" "Todo [inbox, no link]" entry
      (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %i%?\n")
     ("s" "Someday [inbox]" entry
      (file+headline "~/org/inbox.org" "Someday")
      "* %i%?\n%a")
     ("p" "Personal [inbox]" entry
      (file+headline "~/org/inbox.org" "Personal")
      "* TODO %i%?\n%a")
     ("l" "ClipLink [inbox]" entry
      (file+headline "~/org/inbox.org" "Links")
      "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)
     ("b" "Backlog" entry
      (file+headline "~/org/backlog.org" "Backlog")
      "* %i%?\n%a"))))

(use-package org-journal
  :ensure t
  :defer nil
  :after org
  :custom
  (org-journal-dir "~/org/journal")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-enable-agenda-integration t)
  (org-journal-date-prefix "#+TITLE: Daily Notes "))

(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-directory "~/org")
  (org-agenda-tags-column -100)
  (org-agenda-show-log t)
  (org-agenda-span 14)
  (org-agenda-start-on-weekday 1))

;; Org Brain
(use-package org-brain :ensure t
  :ensure t
  :init
  (setq org-brain-path "~/org/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

;; Org Cliplink
(use-package org-cliplink
  :ensure t
  :config
  (bind-key "C-x p i" '' org-cliplink))

(provide 'init-org)
;;; init-org ends here
