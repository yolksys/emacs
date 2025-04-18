;; Install doom-themes
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

;; Load up doom-palenight for the System Crafters look
(load-theme 'doom-palenight t)

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

(column-number-mode 1)
