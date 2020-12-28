;;; init --- startup conf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Make startup faster by reducing the frequency of garbage
;; collection.
(setq gc-cons-threshold (* 50 1000 1000))

;;  Ensure environment variables inside Emacs look the same as in the user's shell.
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'init-elpa)
(require 'init-defaults)
(require 'init-ui)
(require 'init-editing)
(require 'init-navigation)
(require 'init-org)
(require 'init-org-agenda)
(require 'init-company-mode)
(require 'init-langs)
(require 'init-ocaml)
(require 'init-elixir)
(require 'init-ruby)
(require 'init-version-control)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'init)

;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "c1c459af570241993823db87096bc775506c378aa02c9c6cd9ccaa8247056b96" default))
 '(fci-rule-color "#778ca3")
 '(ispell-extra-args '("--sug-mode=ultra") t)
 '(ispell-program-name "aspell" t)
 '(nrepl-message-colors
   '("#00afef" "#778ca3" "#009c9f" "#778ca3" "#005cc5" "#fa1090" "#009c9f" "#778ca3"))
 '(org-agenda-files
   '("/Users/berare01/org/agenda.org" "/Users/berare01/org/backlog.org" "/Users/berare01/org/inbox.org" "/Users/berare01/org/log.org" "/Users/berare01/org/projects.org"))
 '(package-selected-packages
   '(modus-themes fountain-mode markdown-mode olivetti ivy-prescient prescient solo-jazz-theme modus-vivendi-theme fsharp-mode whole-line-or-region kaolin-themes amx exec-path-from-shell ivy-avy counsel swiper ivy popup-kill-ring use-package-hydra use-package-ensure-system-package smex rspec-mode rg rainbow-delimiters projectile polymode org-journal org-cliplink org-bullets org-brain ob-elixir move-text magit inf-ruby ido-completing-read+ idle-highlight-mode golden-ratio flycheck fish-mode enh-ruby-mode elixir-mode eglot doom-themes delight crystal-mode company))
 '(pdf-view-midnight-colors '("#778ca3" . "#eaeafa"))
 '(pos-tip-background-color "#2a2931")
 '(pos-tip-foreground-color "#d4d4d6")
 '(vc-annotate-background "#04c4c7")
 '(vc-annotate-color-map
   '((20 . "#778ca3")
     (40 . "#00afef")
     (60 . "#778ca3")
     (80 . "#778ca3")
     (100 . "#778ca3")
     (120 . "#009c9f")
     (140 . "#778ca3")
     (160 . "#778ca3")
     (180 . "#778ca3")
     (200 . "#778ca3")
     (220 . "#009c9f")
     (240 . "#005cc5")
     (260 . "#fa1090")
     (280 . "#778ca3")
     (300 . "#005cc5")
     (320 . "#778ca3")
     (340 . "#009c9f")
     (360 . "#778ca3")))
 '(vc-annotate-very-old-color "#778ca3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "B612 Mono" :height 110 :weight normal :width normal))))
 '(variable-pitch ((t (:family "Spectral" :height 150 :weight thin)))))
