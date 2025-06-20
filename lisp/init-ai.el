;;; init-ai --- ai Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package gptel
;;   :ensure t
;;   :custom
;;   (gptel-model   'sonar-pro)
;;   (gptel-backend (gptel-make-openai "Perplexity"
;;                    :host "api.perplexity.ai"
;;                    :key (getenv "PERPLEXITY_API_KEY")
;;                    :endpoint "/chat/completions"
;;                    :stream t
;;                    :models '(sonar-pro)
;;                    )
;;                  )
;;   )

(use-package gptel
  :ensure t
  :custom
  (gptel-model   'meta-llama/llama-3.3-70b-instruct)
  (gptel-backend (gptel-make-openai "OpenRouter"
                   :host "openrouter.ai"
                   :key (getenv "OPENROUTER_API_KEY")
                   :endpoint "/api/v1/chat/completions"
                   :stream t
                   :models '(google/gemini-2.5-pro-preview-03-25
                             deepseek/deepseek-chat:free
                             anthropic/claude-4.0-sonnet
                             deepseek/deepseek-r1))))

(use-package aider
  :ensure nil
  :config
  (setenv "OPENROUTER_API_KEY" (getenv "OPENROUTER_API_KEY"))
  (setq aider-args '("--model" "openrouter/anthropic/claude-3.7-sonnet"))
  ;; (setq aider-args '("--model" "gemini/gemini-2.0-flash-001"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;; Or chatgpt model
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu)
  )


(add-to-list 'load-path "~/.emacs.d/codeium.el")
(require 'codeium)
(use-package codeium
    :ensure nil
    :init
    ;; use globally
    ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    (add-hook 'emacs-startup-hook
     (lambda () (run-with-timer 0.3 nil #'codeium-init)))

    (add-hook 'elixir-ts-mode-hook
        (lambda ()
            (setq-local completion-at-point-functions
                '(codeium-completion-at-point))))
                ;; (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    (setq codeium/metadata/api_key (getenv "CODEIUM_API_KEY"))

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(provide 'init-ai)
;;; init-ai ends here
