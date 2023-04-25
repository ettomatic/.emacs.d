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
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("w" "people" plain "%?"
         :if-new
         (file+head "people/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "projects/${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . eb/org-roam-node-insert-immediate)
         ;; Dailies
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n o" . org-roam-dailies-goto-tomorrow)
         ("C-c n j" . org-roam-dailies-capture-today))
         ;; Hacks...
         ("M-RET"   . eb/open-backlink)
  :config
  (org-roam-db-autosync-mode))


(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   ;(consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

;; (setq +org-roam-open-buffer-on-find-file nil)  ;; with +roam, it defaults to t
;; (setq org-roam-buffer-window-parameters nil)  ;; with +roam, it has some value
;; (setq org-open-at-point-functions '(org-roam-open-id-at-point)) ;; Looks like Doom puts something related to `jump` something, which I don't fully understand

(defun eb/org-roam-node-insert-immediate (arg &rest args)
  "Allows to quickly create new notes for topics you're mentioning while writing."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;;; Automatically move completed tasks to dailies

(defun eb/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep nil) ;; Set this to t to keep the original
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (eb/org-roam-copy-todo-to-today))))

(use-package websocket
    :after org-roam)
(use-package org-roam-ui
    :after org-roam ;; or :after org
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;; So much knowledge is still there.....
(use-package obsidian
  :ensure t
  :demand t
  :config
  (obsidian-specify-path "~/Storage/zettel/zettel")
  (global-obsidian-mode t)
  :bind (:map obsidian-mode-map
		;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
		("C-c C-o" . obsidian-follow-link-at-point)
		;; If you prefer you can use `obsidian-insert-link'
		("C-c C-l" . obsidian-insert-wikilink)))

(provide 'init-org-roam)
;;; init-org ends here
