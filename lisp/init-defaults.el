;;; init-defaults --- Basic Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 calendar-location-name "London"                  ; Calendar Location
 calendar-latitude 51.509865                      ; lendar Lat
 calendar-longitude  -0.118092                    ; Calendar Long
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
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
 user-mail-address "ettore.berardi@outlook.com"  ; Set the email address of the current user
 vc-follow-symlinks t)                            ; Always follow the symlinks
(cd "~/")                                         ; Move to the user directory
(column-number-mode 1)                            ; Show the column number
(display-time-mode 1)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent

(setq create-lockfiles nil)

(defconst *is-a-mac* (eq system-type 'darwin))

(provide 'init-defaults)
;;; init-defaults ends here
