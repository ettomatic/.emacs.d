;;; init-org --- org-mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook #'visual-line-mode)
  :custom
  (org-startup-truncated nil)
  (org-indent-indentation-per-level 1)
  (org-adapt-indentation nil)
  (org-hide-leading-stars 't)
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
  (org-archive-location "~/org/archives/%s::")
  (org-src-fontify-natively t)
  (org-log-done 'time)
  (org-return-follows-link t))

(setq org-directory "~/org")

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
	      (sequence "TASK(T)")
	      (sequence "WAITING(w@/!)" "INACTIVE(i)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)")))
;; Custom colors for the keywords
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	      ("TASK" :foreground "#5C888B" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("PROJ" :foreground "magenta" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("INACTIVE" :foreground "magenta" :weight bold)
	      ("SOMEDAY" :foreground "cyan" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold)))

(require 'org)

;;; Open org link in the same window
(defun org-force-open-current-window ()
  (interactive)
  (let ((org-link-frame-setup (quote
                               ((vm . vm-visit-folder)
                                (vm-imap . vm-visit-imap-folder)
                                (gnus . gnus)
                                (file . find-file)
                                (wl . wl)))
                              ))
    (org-open-at-point)))

;;; Depending on universal argument try opening link
(defun org-open-maybe (&optional arg)
  (interactive "P")
  (if arg
      (org-open-at-point)
    (org-force-open-current-window)
    )
  )

;;; Redefine file opening without clobbering universal argumnet
(define-key org-mode-map "\C-c\C-o" 'org-open-maybe)
(define-key org-mode-map "RET" 'org-open-maybe)


;;; Do not ask for confirmation before evaluating
;;; Ruby or Elixir Babel scripts with C-C C-,
(defun eb/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ruby") (string= lang "elixir"))))
(setq org-confirm-babel-evaluate 'eb/org-confirm-babel-evaluate)


(use-package org-capture
  :ensure nil
  :after org
  :init
  (global-set-key (kbd "C-c e") 'org-capture)
  :custom
  (org-capture-templates
   '(("i" "Inbox [inbox, nolink]" entry
      (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %?\n/Entered on/ %U")
     ("L" "Inbox No Link [inbox, link]" entry
      (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %i%?\n%a")
     ("n" "Note [inbox]" entry
      (file+headline "~/org/inbox.org" "Notes")
      "* %i%?\n%a")
     ("m" "Meeting" entry  (file+headline "agenda.org")
      (concat "* %? :meeting:\n"
         "<%<%Y-%m-%d %a %H:00>>"))
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

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(define-key global-map (kbd "C-c i") 'org-capture-inbox)

;; (use-package org-journal
;;   :ensure t
;;   :defer f
;;   :after org
;;   :init
;;   (global-set-key (kbd "C-c C-j") 'org-journal-new-entry)
;;   :custom
;;   (org-journal-dir "~/org/journal")
;;   (org-journal-date-format "%A, %d %B %Y")
;;   (org-journal-file-format "%Y%m%d.org")
;;   (org-journal-enable-agenda-integration t)
;;   (org-journal-date-prefix "#+TITLE: Daily Notes "))

;; Org Brain
(use-package org-brain :ensure t
  :ensure t
  :init
  (setq org-brain-path "~/org/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t))

;; Allows you to edit entries directly from org-brain-visualize
;;(use-package polymode
;;  :config
;;  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

;; Org Cliplink
(use-package org-cliplink
  :ensure t
  :config
  (bind-key "C-x p i" '' org-cliplink))

;;; beutify it
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Show org-mode bullets as UTF-8 characters.
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/zettel")
  (org-roam-db-location "~/.org-roam/org-roam.db")
  (org-roam-dailies-directory "journal/")
  (org-roam-open-at-point t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "journal/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d>\n\n")))
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "~/org/zettel/"))

(provide 'init-org)

;;; init-org ends here
