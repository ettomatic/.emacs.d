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
  (org-adapt-indentation t)
  (org-startup-indented t)
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
  (org-log-done 'time)
  (org-return-follows-link t))

(setq org-directory "~/org")

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "PROJ(p)" "|" "DONE(d)")
	      (sequence "TASK(T)")
	      (sequence "WAITING(w)" "INACTIVE(i)" "SOMEDAY(S)" "|" "CANCELLED(c)")))
;; Custom colors for the keywords
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	      ("TASK" :foreground "#5C888B" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("STARTED" :foreground "cyan" :weight bold)
	      ("PROJ" :foreground "magenta" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("WAITING" :foreground "orange" :weight bold)
	      ("INACTIVE" :foreground "brown" :weight bold)
	      ("SOMEDAY" :foreground "dark cyan" :weight bold)
	      ("CANCELLED" :foreground "forest green" :weight bold)))

(set-face-attribute 'org-headline-done nil :strike-through t)

;;; Enables auto-saving of org files while emacs is running
;;; That will put auto-saving all open org files on a timer.
;;; Performing changes to buffers from the org agenda overview, for example,
;;; doesn’t mark the buffer as needing to auto-save, as far as I understand.
;;; So this setting helps to auto-save all org buffers regularly.
;;; https://christiantietze.de/posts/2019/03/sync-emacs-org-files/
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

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

;;; Babel

;;; Syntax highlightning in code blocks
(setq org-src-fontify-natively t)

;;; Trying to fix indentation behaviour within code blocks.
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

;;; Do not ask for confirmation before evaluating
;;; Ruby or Elixir Babel scripts with C-C C-,
(defun eb/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "ruby") (string= lang "elixir"))))
(setq org-confirm-babel-evaluate 'eb/org-confirm-babel-evaluate)


;;; Capture

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

;;; Org Brain

(use-package org-brain :ensure t
  :ensure t
  :init
  (setq org-brain-path "~/org/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t))

;;; Org Cliplink

(use-package org-cliplink
  :ensure t
  :config
  (bind-key "C-x p i" '' org-cliplink))

;;; beutify org-mode
(setq org-hide-emphasis-markers t ; Show actually italicized text instead of /italicized text/.
      org-tags-column 0           ; Show tags directly after headings (not on the right), which plays nicer with line-wrapping.
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;;; This package makes it much easier to edit Org documents when org-hide-emphasis-markers is turned on.
;;; It temporarily shows the emphasis markers around certain markup elements when you place your cursor inside of them.
;;; No more fumbling around with = and * characters!
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Show org-mode bullets as UTF-8 characters.
(use-package org-superstar
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  :custom
  (org-superstar-headline-bullets-list '("⁖" "⁘" "⁝" "⊙" "☊" "☋" "☌" "☍" "∞")))


(defun eb/open-backlink ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'org-roam-preview-visit)))

(provide 'init-org)
;;; init-org ends here
