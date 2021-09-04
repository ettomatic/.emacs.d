;;; init-presentation --- org-mode Presentation -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  ;; (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
  ;;                                    (header-line (:height 4.5) variable-pitch)
  ;;                                    (org-document-title (:height 1.75) org-document-title)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    (org-block (:height 1) org-block)
  ;;                                    (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . dw/org-present-next)
         ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

;; Ensure that anything that should be default in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'default)
(set-face-attribute 'org-table nil  :inherit 'default)
(set-face-attribute 'org-formula nil  :inherit 'default)
(set-face-attribute 'org-code nil   :inherit '(shadow default))

;(add-to-list 'default-frame-alist '(internal-border-width . 24))

(provide 'init-presentation)
;;; init-presentation ends here
