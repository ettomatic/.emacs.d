(setq gcmh-high-cons-threshold (* 1024 1024 1024))
(setq gcmh-idle-delay-factor 20)
(setq jit-lock-defer-time 0.05)
(setq read-process-output-max (* 1024 1024))
(setq package-native-compile t)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
      (setq gc-cons-threshold (* 32 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; improves terminal emulator (vterm/eat) throughput
(setq read-process-output-max (* 2 1024 1024)
          process-adaptive-read-buffering nil)
