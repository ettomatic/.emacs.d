;;; init-defaults --- Basic Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 calendar-location-name "London"                  ; Calendar Location
 calendar-latitude 51.509865                      ; lendar Lat
 calendar-longitude  -0.118092                    ; Calendar Long
 calendar-week-start-day 1                        ; starts on Monday
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 display-time-default-load-average nil            ; Don't display load average
 fill-column 120                                  ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Prefers spaces over tabs
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-ring-max 128                                ; Maximum length of kill ring
 load-prefer-newer t                              ; Prefers the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 2                                      ; Set width for tabs
 use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 user-full-name "Ettore Berardi"                  ; Set the full name of the current user
 user-mail-address "ettore.berardi@outlook.com"   ; Set the email address of the current user
 bidi-paragraph-direction 'left-to-right          ; Enable left-to-right as a default to get faster rendering
 bidi-inhibit-bpa t                               ; Disabling he Bidirectional Parentheses Algorithm makes redisplay faster
 auto-save-interval 5                             ; Auto save every 5secs
 make-backup-files nil                            ; Turn Off Backup
 auto-save-visited-file-name t                    ;
 savehist-mode 1                                  ; Preserve commands preserved between sessions
 vc-follow-symlinks t)                            ; Always follow the symlinks
(cd "~/")                                         ; Move to the user directory
(column-number-mode 1)                            ; Show the column number
(display-time-mode 1)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent

;;; Automatically Kill Running Processes on Exit
(setq confirm-kill-processes nil)

(setq create-lockfiles nil)

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; Emacs will save customizations on etc/ instead of your init.el file by default.
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))


;;; Avoid constant errors on Windows about the coding system by setting the default to UTF-8.
(set-default-coding-systems 'utf-8)

(defvar is-mac (eq system-type 'darwin)
  "Whether Emacs is running in mac or not.")

(defvar is-gui (display-graphic-p)
  "Whether Emacs is running in gui mode or not.")

(defvar is-term (not is-gui)
  "Whether Emacs is running in a terminal or not.")

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;; Displaying World Time
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Rome" "Rome")
        ("Europe/Athens" "Athens")
        ("Asia/Tokyo" "Tokyo")
        ("Pacific/Auckland" "Auckland")))

(provide 'init-defaults)
;;; init-defaults ends here
