;;; ngx.el --- tree-sitter support for NGXHTML  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

;; Author     : cyf <theo@thornhill.no>
;; Maintainer : yf <theo@thornhill.no>
;; Created    : January 2025
;; Keywords   : ngxhtml languages tree-sitter

;; This file is not part of GNU Emacs.

;; This file is free software

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(use-package ngxhtml-ts-mode
  :load-path ("~/.emacs.d/packages/ngxhtml-ts-mode")
  )

(add-hook 'ngxhtml-ts-mode-hook #'lsp-deferred)

(defun ngxhtml-setting-hooks ()
  (apheleia-mode -1))
(add-hook 'ngxhtml-ts-mode-hook #'ngxhtml-setting-hooks)

;;;(defvar-local node-path (shell-command-to-string "which node"))

;;;
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(ngxhtml-ts-mode . "ngxhtml"))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     '("node"
                       "/home/cyf/.nvm/versions/node/v23.8.0/lib/node_modules/@angular/language-server"
                       "--ngProbeLocations"
                       "/home/cyf/.nvm/versions/node/v23.8.0/lib/node_modules"
                       "--tsProbeLocations"
                       "/home/cyf/.nvm/versions/node/v23.8.0/lib/node_modules/"
                       "--stdio"))
                    :activation-fn (lsp-activate-on "ngxhtml")
                    :server-id 'ngxhtml-ls
		    :notification-handlers (ht ("angular/projectLoadingStart" #'ignore)
                                               ("angular/projectLoadingFinish" #'ignore)))))

;;;###autoload
;;;(require 'combobulate-ngxhtml)

(provide 'ngx)
;;; ngx.el ends here
