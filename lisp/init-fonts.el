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
  :ensure t
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets
   '((Cascadia
      :default-family "Cascadia Code")
     (Julia
      :default-family "JuliaMono"
      :default-weight regular)
     (Berkley
      :default-family "Berkeley Mono Trial"
      :default-height 115
      :default-weight regular)
     (Iosevka
      :default-family "Iosevka Fixed SS03"
      :default-height 120
      :default-weight medium)
     (JetBrains
      :default-family "JetBrains Mono")
     (FiraCode
      :default-family "Fira Code")
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
      :line-spacing 0))))

(when is-gui
  ;; Recover last preset, but only if it's still a valid entry in
  ;; `fontaine-presets' -- `fontaine-latest-state-file' may be shared
  ;; across machines (e.g. via dotfiles) and reference a preset that
  ;; doesn't exist here, otherwise fall back to `Julia'.
  (let ((preset (fontaine-restore-latest-preset)))
    (fontaine-set-preset (if (assq preset fontaine-presets) preset 'Julia))))

;; Persist the latest font preset when closing/starting Emacs and
;; while switching between themes.
(fontaine-mode 1)

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

;; pixel-scroll.el: animates C-v/M-v as a smooth pixel-by-pixel scroll
;; instead of an instant jump.
(require 'pixel-scroll)
;; Default `factor' used by `pixel-scroll-precision-interpolate' below.
(setq pixel-scroll-precision-interpolation-factor 30)

(defun joe/smooth-scroll-half-page-down ()
  "Animate scrolling the buffer down by half a window height."
  (interactive)
  (let ((half-height (/ (window-height) 2)))
    (pixel-scroll-precision-interpolate (* 5 (- half-height)))))

(defun joe/smooth-scroll-half-page-up ()
  "Animate scrolling the buffer up by half a window height."
  (interactive)
  (let ((half-height (/ (window-height) 2)))
    (pixel-scroll-precision-interpolate (* 5 half-height))))

;; Replace the default instant-jump `scroll-up-command'/`scroll-down-command'
;; with the smooth half-page versions above.
(global-set-key (kbd "C-v") #'joe/smooth-scroll-half-page-down)
(global-set-key (kbd "M-v") #'joe/smooth-scroll-half-page-up)

;; Mouse wheel / trackpad scrolling (separate from pixel-scroll above,
;; since `pixel-scroll-precision-mode' is not enabled).
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(provide 'init-fonts)
;;; init-fonts ends here