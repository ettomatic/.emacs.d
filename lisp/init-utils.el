;;; init-utils --- Utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar is-mac (eq system-type 'darwin)
  "Whether Emacs is running in mac or not.")

(defvar is-win (eq system-type 'windows-nt)
  "Whether Emacs is running in Windows or not.")

(defvar is-gui (display-graphic-p)
  "Whether Emacs is running in gui mode or not.")

(defvar is-term (not is-gui)
  "Whether Emacs is running in a terminal or not.")

(provide 'init-utils)
;;; init-utils ends here
