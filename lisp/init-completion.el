;;; init-completion ---  modular in-buffer completion framework  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Configure Tempel
;; (use-package tempel
;;   ;; Require trigger prefix before template name when completing.
;;   ;; :custom
;;   ;; (tempel-trigger-prefix "<")

;;   :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;;          ("M-*" . tempel-insert))

;;   :init

;;   ;; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;;                       completion-at-point-functions)))

;;   (add-hook 'conf-mode-hook 'tempel-setup-capf)
;;   (add-hook 'prog-mode-hook 'tempel-setup-capf)
;;   (add-hook 'text-mode-hook 'tempel-setup-capf)

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   ;; (global-tempel-abbrev-mode)
;; )

;; ;; Optional: Add tempel-collection.
;; ;; The package is young and doesn't have comprehensive coverage.
;; (use-package tempel-collection)
;; (use-package eglot-tempel)

(use-package corfu-terminal
  :ensure t
  :defer t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                  ;; Enable auto completion
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 3)
  (corfu-separator ?\s)             ;; Orderless field separator
  (corfu-quit-at-boundary nil)      ;; Never quit at completion boundary
  (corfu-preview-current 'insert)   ;; Do not preview current candidate
  (corfu-preselect-first nil)       ;; Disable candidate preselection
  :bind
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (unless (display-graphic-p)
  (corfu-terminal-mode +1)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Add extensions
(use-package cape
  :defer 10
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(provide 'init-completion)
;;; init-completion ends here
