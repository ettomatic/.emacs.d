;;; init-ai --- ai Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t
  :custom
  (gptel-model   'sonar-pro)
  (gptel-backend (gptel-make-openai "Perplexity"
                   :host "api.perplexity.ai"
                   :key (getenv "PERPLEXITY_API_KEY")
                   :endpoint "/chat/completions"
                   :stream t
                   :models '(sonar-pro)
                   )
                 )
  )


(provide 'init-ai)
;;; init-ai ends here
