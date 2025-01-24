;;; init-shell --- Shells management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ansi-color)

(use-package eshell-bookmark
  :ensure t
  :defer t
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

(setq eshell-history-size         10000
      eshell-buffer-maximum-lines 10000
      eshell-hist-ignoredups t
      eshell-highlight-prompt t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-prefer-lisp-functions nil)

;; We want to use xterm-256color when running interactive commands
;; in eshell but not during other times when we might be launching
;; a shell command to gather its output.
(add-hook 'eshell-pre-command-hook
          #'(lambda () (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-post-command-hook
          #'(lambda () (setenv "TERM" "dumb")))

(add-hook 'eshell-mode-hook (lambda ()
    (eshell/alias "f" "find-file $1")
    (eshell/alias "fo" "find-file-other-window $1")
    (eshell/alias "d" "dired $1")
    (eshell/alias "ll" "ls -AlohG --color=always $1")))

(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

(defun eshell/ff (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/ff (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/ff (concat "*" filename) dir 2))
       (t "")))))

(defun eshell/ffo (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar 'abbreviate-file-name
                                (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell
                                          consult-dir-sources)))
          (eshell/cd (substring-no-properties
                      (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                            (completing-read "cd: " eshell-dirs)))))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key "C-!" 'eshell-here)

;; (use-package eshell
;;   :bind (("C-c e" . eshell)
;;          :map eshell-hist-mode-map
;;                  ("<down>" . 'next-line)
;;                  ("<up>" . 'previous-line)))

(use-package eshell-prompt-extras
  :ensure t
  :defer t)

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;; a real terminal when necessary
(use-package eat
  :ensure t
  :defer t
  :init
  (setenv "SHELL" "/opt/homebrew/bin/fish")
  :config
  (setq eat-shell "/opt/homebrew/bin/fish"))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)
;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)


(provide 'init-shell)
;;; init-shell ends here
