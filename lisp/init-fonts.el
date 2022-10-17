;;; init-fonts --- Fonts & Text Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; size & scaling
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))
(global-prettify-symbols-mode +1)

(use-package fontaine
  :ensure t)

(setq fontaine-latest-state-file
      (locate-user-emacs-file "fontaine-latest-state.eld"))

(setq fontaine-presets
      '((fira
         :default-family "Fira Code"
         :default-height 110
         :italic-family "Hack")
        (hack
         :default-family "Hack"
         :default-weight medium
         :default-height 110)
        (Cascadia
         :default-family "Cascadia Code"
         :default-height 110)
        (Julia
         :default-family "Julia Mono"
         :default-height 105
         :default-width expanded)
        (Berkley
         :default-family "Berkeley Mono Trial"
         :default-height 105
         :default-width condensed)
        (writing
         :default-family "Writer"
         :default-height 110
         :line-spacing 4)
        (t
         :default-weight regular
         :default-height 110
         :default-weight normal
         :line-spacing: nil)))

;; Recover last preset or fall back to desired style from
;; `fontaine-presets'.
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'fira))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

;; enable ligatures

(use-package ligature
  :ensure t)

;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)


(require 'pixel-scroll)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolation-factor 30)

(defun joe/smooth-scroll-half-page-down ()
  "Smooth scroll down"
  (interactive)
  (let ((half-height (/ (window-height) 2)))
    (pixel-scroll-precision-interpolate (* 5 (- half-height)))))

(defun joe/smooth-scroll-half-page-up ()
  "Smooth scroll down"
  (interactive)
  (let ((half-height (/ (window-height) 2)))
    (pixel-scroll-precision-interpolate (* 5 half-height))))

;; scroll-up-command
(global-set-key (kbd "C-v") #'joe/smooth-scroll-half-page-down)
(global-set-key (kbd "M-v") #'joe/smooth-scroll-half-page-up)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(provide 'init-fonts)
;;; init-fonts ends here
