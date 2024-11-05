;;; init-org-agenda --- org-mode Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org-agenda
  :ensure nil
  :after org
  :init
  (global-set-key (kbd "C-c a") 'org-agenda)
  :custom
  (org-agenda-tags-column -100)
  (org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (search . " %i %-12:c")))
  (org-agenda-show-log t)
  (org-agenda-start-day "-1d")
  (org-agenda-span 5)
  (org-agenda-start-on-weekday nil)
  (org-agenda-hide-tags-regexp ".")
  (org-agenda-restore-windows-after-quit t))

(setq org-agenda-files (list "~/org/agenda.org" "~/org/inbox.org" "~/org/calendar.org", "~/org/personal.org"))
;;(setq org-agenda-files (list "~/org/agenda.org" "~/org/inbox.org" "~/org/projects.org" "~/org/backlog.org" "~/org/exchange.org" "~/org/zettel/projects"))

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
        "......" "----------------")))

;; not sure why need to set it here as well
(setq org-agenda-restore-windows-after-quit t)

;;  Pressing Tab while the cursor is on a task will expand that task in a separate buffer
(add-hook 'org-agenda-mode-hook
          (lambda () (local-set-key [tab] 'org-agenda-tree-to-indirect-buffer)))

(defvar eb-org-agenda-block--today-schedule
  '(agenda "" ((org-agenda-overriding-header "Today's Schedule:\n")
	       (org-agenda-span 'day)
	       (org-agenda-ndays 1)
	       (org-agenda-start-on-weekday nil)
	       (org-agenda-start-day "+0d")))
  "A block showing a 1 day schedule.")

(defvar eb-org-agenda-block--next-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-ARCHIVE/!NEXT"
	            ((org-agenda-overriding-header
                "Next Tasks:\n")))
  "Next tasks.")

(defvar eb-org-agenda-block--upcoming-week
  '(agenda ""
           ((org-agenda-overriding-header "Upcoming Week\n")
	          (org-agenda-start-day "+1d")
	          (org-agenda-span 7)
	          (org-agenda-start-on-weekday nil)))
  "A block showing my schedule for the next couple weeks.")

(defvar eb-org-agenda-block--inbox
  '(tags-todo "inbox"
              ((org-agenda-prefix-format "  %?-12t% s")
               (org-agenda-overriding-header "Inbox\n"))))

;; still not working
;; see https://github.com/gjstein/emacs.d/blob/master/config/gs-org-agenda.el
;; and https://github.com/rougier/emacs-gtd/blob/master/GTD.org
(defvar eb-org-agenda-block--deadlines
  '(agenda nil
          ((org-agenda-entry-types '(:deadline))
           (org-agenda-format-date "")
           (org-deadline-warning-days 7)
           (org-agenda-skip-function
            '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
           (org-agenda-overriding-header "Deadlines"))))

(defvar eb-org-agenda-block--closed-today
  '(tags "CLOSED>=\"<today>\""
         ((org-agenda-overriding-header "Completed today\n"))))

(setq org-agenda-custom-commands
      `(("i" "Today"
	       (,eb-org-agenda-block--today-schedule
          ,eb-org-agenda-block--next-tasks
          ,eb-org-agenda-block--upcoming-week
          ,eb-org-agenda-block--inbox
          ,eb-org-agenda-block--closed-today))))

(defun organised-exchange ()
  "Sync Outlook Calendar ics with Org Agenda."
  (interactive)
  (if (get-buffer "exchange.org")
      (kill-buffer "exchange.org"))
  (shell-command "~/code/organised-exchange/bin/eto")
  (message "calendar imported!"))

(provide 'init-org-agenda)
;;; init-org-agenda ends here
