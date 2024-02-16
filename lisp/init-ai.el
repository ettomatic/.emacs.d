;;; init-ai --- ai Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package kagi
    :ensure t
    :custom
    (kagi-api-token (getenv "KAGI_API_TOKEN"))
    ;; or use a function, e.g. with the password-store package:
    ;; (kagi-api-token (lambda () (password-store-get "Kagi/API")))

    ;; Univernal Summarizer settings
    (kagi-summarizer-engine "cecil")
    (kagi-summarizer-default-language "EN")
    (kagi-summarizer-cache t)
    :custom-face
    ;; kagi-code defaults to fixed-pitch, but can be overridden as
    ;; follows:
    (kagi-code ((t (:inherit org-verbatim))))

    ;; likewise for kagi-bold:
    (kagi-bold ((t (:inherit modus-themes-bold)))))

(provide 'init-ai)
;;; init-ai ends here
