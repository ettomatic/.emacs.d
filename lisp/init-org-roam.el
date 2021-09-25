;;; init-org-roam --- org-roam Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t) ;; acknowledge upgrade and remove warning at startup
  :custom
  (org-roam-directory (file-truename "~/org/zettel"))
  (org-roam-dailies-directory "journal/")
  (org-roam-open-at-point t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n r" . eb/org-roam-rg-search)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("M-RET" . eb/open-backlink)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;(add-hook 'after-init-hook 'org-roam-mode)
  (org-roam-setup))

(defun eb/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

;; (setq +org-roam-open-buffer-on-find-file nil)  ;; with +roam, it defaults to t
;; (setq org-roam-buffer-window-parameters nil)  ;; with +roam, it has some value
;; (setq org-open-at-point-functions '(org-roam-open-id-at-point)) ;; Looks like Doom puts something related to `jump` something, which I don't fully understand

(provide 'init-org-roam)
;;; init-org ends here
