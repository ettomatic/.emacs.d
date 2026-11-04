;;; init-notes-notes --- org-notes Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package denote
  :ensure t
  :init
  (setq denote-directory (expand-file-name "~/org/notes/")
        denote-file-type 'markdown-yaml
        denote-excluded-directories-regexp
        (rx "/" (or "assets" "images") "/"))
  :custom
  (denote-save-buffers t)
  (denote-known-keywords '("bbc" "emacs" "elixir" "people" "team" "meeting" "research"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-date-prompt-use-org-read-date t)
  (xref-search-program 'ripgrep)
  :hook
  ((markdown-mode . denote-fontify-links-mode-maybe)
   (org-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-dired)
   ("C-c n G" . denote-grep)
   ("C-c n l" . denote-link)
   ("C-c n L" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n q c" . denote-query-contents-link)
   ("C-c n q f" . denote-query-filenames-link)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-dired-link-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (denote-rename-buffer-mode 1)
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c n i") #'denote-link-or-create)
    (define-key markdown-mode-map (kbd "C-c n o") #'denote-open-or-create))
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c n i") #'denote-link-or-create)
    (define-key org-mode-map (kbd "C-c n o") #'denote-open-or-create)))

(use-package consult-denote
  :ensure t
  :bind
  (:map global-map
        ("C-c n f" . consult-denote-find)
        ("C-c n g" . consult-denote-grep))
  :config
  (setq consult-denote-grep-command #'consult-ripgrep)
  (consult-denote-mode 1))

(defun my/denote-olivetti-setup ()
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name denote-directory)
                              (file-name-directory (expand-file-name buffer-file-name)))
             (derived-mode-p 'org-mode 'markdown-mode))
    (olivetti-mode 1)))

(add-hook 'find-file-hook #'my/denote-olivetti-setup)

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :bind
  (:map global-map
        ("C-c n j n" . denote-journal-new-or-existing-entry))
  :hook (calendar-mode . denote-journal-calendar-mode)
  :custom
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (denote-journal-directory (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (denote-journal-title-format 'day-date-month-year))

;; (require 'howm-org)
;; (use-package howm
;;   :ensure t
;;   :init
;;   ;; Where to store the files?
;;   (setq howm-directory "~/org/howm")
;;   ;; What format to use for the files?
;;   (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
;;   (setq howm-view-title-header "*")
;;   (setq howm-dtime-format "<%Y-%m-%d %a %H:%M>")
;;   ;; Avoid conflicts with Org-mode by changing Howm's prefix from "C-c ,".
;;   (setq howm-prefix (kbd "C-c ;"))
;;   (setq howm-follow-theme t)
;;   ;; Use ripgrep for fast searching.
;;   (setq howm-view-use-grep t)
;;   (setq howm-view-grep-command "rg")
;;   (setq howm-view-grep-option "-nH --no-heading --color never")
;;   (setq howm-view-grep-extended-option nil)
;;   (setq howm-view-grep-fixed-option "-F")
;;   (setq howm-view-grep-expr-option nil)
;;   (setq howm-view-grep-file-stdin-option nil)
;;   ;; Make the "comefrom links" case-insensitive.
;;   (setq howm-keyword-case-fold-search t)
;;   ;; Performance
;;   ;(setq howm-menu-expiry-hours 1) ;; Cache menu N hours. (*4)
;;   ;(setq howm-menu-refresh-after-save nil) ;; Speed up note saving.
;;   :bind*
;;   ;; Conveniently open the Howm menu with "C-c ; ;".
;;   ("C-c ; ;" . howm-menu))

;; (use-package consult-notes
;;   :ensure t
;;   :commands (consult-notes
;;              consult-notes-search-in-all-notes)
;;   :config
;;   (setq consult-notes-file-dir-sources
;;         '(("Denote"  ?d  "~/org/notes/")
;;           ("Journal" ?j  "~/org/notes/journal")))
;;   :bind
;;   ("C-x C-m" . consult-notes-search-in-all-notes )
;;   ("C-x C-n" . consult-notes))

;; Insert current date for Journal
;; alternatively use M-x org-time-stamp
(defun today ()
  "Insert string for today's date nicely formatted e.g. Sunday 17 September 2000"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A %e %B %Y")))

(global-set-key (kbd "C-c t") 'today)

(provide 'init-notes)
;;; init-notes ends here
