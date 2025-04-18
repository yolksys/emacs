(use-package company
  :ensure t
  ;; Optional customizations
  :custom
  (company-minimum-prefix-length 2)                 ;; Allows cycling through candidates
  (company-idle-delay 0)                  ;; Enable auto completion
  (company-inhibit-inside-symbols t)            ;; No delay for completion
  
  :init
  (global-company-mode))
