;;; init-navigation --- Navigation configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Projectile is a project interaction library for Emacs
(use-package projectile
  :ensure t
  :defer 1
  :preface
  :custom
  (projectile-keymap-prefix (kbd "C-c C-p"))
  (projectile-mode-line '(:eval (projectile-project-name)))
  (projectile-completion-system 'default)
  :config (projectile-mode))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Enable move point from window to window using Shift and the arrow keys
;;; doesn't play well with Org
;; (windmove-default-keybindings)
(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package dirvish
  :ensure t
  :defer t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("c" "~/code/"                     "Code")
     ("b" "~/code/belfrage/"            "Belfrage")))
  :config
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-size))
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("u"   . dired-up-directory)))

(setq dired-kill-when-opening-new-dired-buffer t)

(use-package rg
  :defer t
  :init
  (rg-enable-default-bindings))

(use-package marginalia
  :ensure
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 25))))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;;; Recent Files

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(add-to-list 'recentf-exclude "~/.emacs.d/elpa/")
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
;; ;;; If Emacs exits abruptly for some reason the recent file list will be lost
;; ;;;  therefore you may wish to call `recentf-save-list` periodically, e.g. every 5min
(run-at-time nil (* 5 60) 'recentf-save-list)

(use-package consult
  :ensure t
  :bind (([C-tab] . consult-buffer)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-s" . consult-line)
         ("M-g M-g" . consult-goto-line)
         ("C-x C-a" . consult-ripgrep)
         ("C-x C-l" . consult-find)
         ("M-y" . consult-yank-pop)
         ("C-c h" . consult-history)
         ("s-#" .  consult-project-imenu)
         ("M-g o" . consult-outline))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
   (setq register-preview-delay 0.5)
  ;;       register-preview-function #'consult-register-format)
  :config
  (consult-customize
   ;; consult-theme :preview-key '(:debounce 0.2 any)
   ))
(require 'consult)

;; I have a tendency of using C-x 0 from the minibuffer, this leaves it
;; open and require C-] to abort the current operation.
(define-key minibuffer-local-map (kbd "C-x o") 'abort-minibuffers)

(use-package deadgrep
  :ensure t
  :bind (("<f5>" . deadgrep)))

;;; Avy
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-2)))

;;; buffer placement algorithm
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; (use-package guru-mode)
;; (add-hook 'eshell-mode-hook 'guru-mode)
;; (guru-global-mode 0)


(setq tramp-default-method "ssh")

;; makes C-n insert newlines if the point is at the end of the buffer
;(setq next-line-add-newlines t)

;; Having observed beginners struggle with C-g not closing the open minibuffer, I know that the following is a quality-of-life refinement:

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(provide 'init-navigation)
;;; init-navigation ends here
;;
