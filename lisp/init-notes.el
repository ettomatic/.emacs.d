;;; init-notes-notes --- org-notes Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/org/notes/"))
  (setq denote-save-buffer-after-creation nil)
  (setq denote-known-keywords '("emacs" "elixir" "bbc" "journal" "culture" "hiking" "travel" "shopping"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(subdirectory title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-no-confirm nil) ; Set to t if you are familiar with `denote-rename-file'

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t))

(require 'howm-org)
(use-package howm
  :ensure t
  :init
  ;; Where to store the files?
  (setq howm-directory "~/org/howm")
  ;; What format to use for the files?
  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
  (setq howm-view-title-header "*")
  (setq howm-dtime-format "<%Y-%m-%d %a %H:%M>")
  ;; Avoid conflicts with Org-mode by changing Howm's prefix from "C-c ,".
  (setq howm-prefix (kbd "C-c ;"))
  (setq howm-follow-theme t)
  ;; Use ripgrep for fast searching.
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)
  ;; Make the "comefrom links" case-insensitive.
  (setq howm-keyword-case-fold-search t)
  ;; Performance
  ;(setq howm-menu-expiry-hours 1) ;; Cache menu N hours. (*4)
  ;(setq howm-menu-refresh-after-save nil) ;; Speed up note saving.
  :bind*
  ;; Conveniently open the Howm menu with "C-c ; ;".
  ("C-c ; ;" . howm-menu))

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (setq consult-notes-file-dir-sources
        '(("Denote"  ?d  "~/org/notes/")
          ("Journal" ?j  "~/org/notes/journal")))
  :bind
  ("C-x C-m" . consult-notes-search-in-all-notes )
  ("C-x C-n" . consult-notes))

;; Insert current date for Journal
;; alternatively use M-x org-time-stamp
(defun today ()
  "Insert string for today's date nicely formatted e.g. Sunday 17 September 2000"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A %e %B %Y")))

(global-set-key (kbd "C-c t") 'today)

(provide 'init-notes)
;;; init-notes ends here
