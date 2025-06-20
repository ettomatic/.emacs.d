;;; init-notes-notes --- org-notes Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/org/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("bbc" "emacs" "elixir" "people" "team" "meeting"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

;; (use-package denote-markdown
;;   :ensure t)

(use-package consult-denote
  :ensure t
  :bind
  (:map global-map
    ("C-c n f" . consult-denote-find)
    ("C-c n g" . consult-denote-grep)))

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
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))

  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

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
