;;for package
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-user-dir (expand-file-name ".emacs.d/packages"))
(require 'package)
(package-initialize)
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia company corfu doom-themes flycheck go-mode lsp-ivy
              lsp-treemacs lsp-ui olivetti org-appear
              org-modern org-present org-superstar pdf-tools
              rainbow-delimiters saveplace-pdf-view svg-tag-mode
              treemacs-icons-dired visual-fill-column which-key
              yasnippet))
 '(treesit-font-lock-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:height 1.6))))
 '(outline-1 ((t (:height 1.25))))
 '(outline-2 ((t (:height 1.2))))
 '(outline-3 ((t (:height 1.15))))
 '(outline-4 ((t (:height 1.1))))
 '(outline-5 ((t (:height 1.1))))
 '(outline-6 ((t (:height 1.1))))
 '(outline-8 ((t (:height 1.1))))
 '(outline-9 ((t (:height 1.1)))))

;;for general init
(save-place-mode 1)
(setq eww-search-prefix  "https://cn.bing.com/search?q=")
;;(setq-default indent-tabs-mode nil)        ;; Disable indent with tabs
;;(setq default-tab-width 2)
(setq-default
 ;;tab-width 2
 standard-indent 2
 indent-tabs-mode nil)
;; auto close bracket insertion, auot pair
(electric-pair-mode 1)
;;for scroll bar
(scroll-bar-mode 0)
;;for tool bar
(tool-bar-mode 0)
;;for menu bar
(menu-bar-mode 0)

;; backup into one flat dir
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
;; make sure hard link and creation date, owner, etc is preserved
(setq backup-by-copying t)

;;for newline
(load "~/.emacs.d/.init/utils")
;;for newline
(load "~/.emacs.d/.init/newline")
;;for org-present
(load "~/.emacs.d/.init/doom-theme")
;;for treemacs
(load "~/.emacs.d/.init/treemacs")
;;for yasnippet
(load "~/.emacs.d/.init/yasnippet")
;;for pdf-tool
(load "~/.emacs.d/.init/pdf-tool")
;;for
(load "~/.emacs.d/.init/RainbowDelimiters")
;;for
(load "~/.emacs.d/.init/kill-other-buffers")
;;for
(load "~/.emacs.d/.init/treesit")
;;for
(load "~/.emacs.d/.init/combobulate")
;;for
;;(load "~/.emacs.d/.init/corfu")
;;for
(load "~/.emacs.d/.init/company")
;;for
(load "~/.emacs.d/.init/linter")
;;for
(load "~/.emacs.d/.init/eslint")
;;for
(load "~/.emacs.d/.init/flycheck")
;;for
(load "~/.emacs.d/.init/apheleia")
;;for lsp-mode
(load "~/.emacs.d/.init/lsp-mode")
;;for
(load "~/.emacs.d/.init/lsp-treemacs")

;;for
(load "~/.emacs.d/.init/ngx")
;;for
(load "~/.emacs.d/.init/go")
;;for
(load "~/.emacs.d/.init/scss")
;;for
(load "~/.emacs.d/.init/org")
