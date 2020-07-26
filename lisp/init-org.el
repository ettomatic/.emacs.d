
;; Org Brain
(use-package org-brain :ensure t
  :ensure t
  :init
  (setq org-brain-path "~/org/brain")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
  :config
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

;; Org Cliplink
(use-package org-cliplink
  :ensure t
  :config
  (bind-key "C-x p i" '' org-cliplink))

(provide 'init-org)
