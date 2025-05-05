;; Insert new line below current line
;; and move cursor to new line
;; it will also indent newline
(global-set-key (kbd "<C-return>") (lambda ()
                   (interactive)
                   (end-of-line)
                   (newline-and-indent)))
;; Insert new line above current line
;; and move cursor to previous line (newly inserted line)
;; it will also indent newline
;; TODO: right now I am unable to goto previous line, FIXIT
(global-set-key (kbd "<C-S-return>") (lambda ()
                       (interactive)
                       (previous-line)
                       ;;(beginning-of-line)
		       (end-of-line)
                       (newline-and-indent)))
