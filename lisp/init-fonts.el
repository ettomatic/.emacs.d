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
      '((Cascadia
         :default-family "Cascadia Code")
        (Julia
         :default-family "JuliaMono"
         :default-weight regular)
        (Berkley
         :default-family "Berkeley Mono Trial"
         :default-height 110
         :default-weight medium)
        (Iosevka
         :default-family "Iosevka Fixed SS03"
         :default-height 120
         :default-weight medium)
        (JetBrains
         :default-family "JetBrains Mono"
         :default-height 110)
        (writing
         :default-family "Writer"
         :line-spacing 4)
        (mac-ext
         :default-family "JuliaMono"
         :default-weight regular
         :default-height 135) ; 180 4k
        (t
         :default-weight regular
         :default-height 145 ; 180 4k
         :default-weight normal
         :line-spacing: 0)))
         ;:default-width condensed)

(if is-gui
    ;; Recover last preset or fall back to desired style from
    ;; `fontaine-presets'.
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'JuliaMono))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))



;; enable ligatures

;; (use-package ligature
;;   :ensure t)

;; ;; Enable the www ligature in every possible major mode
;; (ligature-set-ligatures 't '("www"))

;; ;; Enable ligatures in programming modes
;; (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
;;                                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
;;                                      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
;;                                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
;;                                      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
;;                                      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
;;                                      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
;;                                      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
;;                                      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;;                                      "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

;; (global-ligature-mode nil)


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
