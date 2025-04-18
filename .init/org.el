;;;
(use-package org
  :defer t
  :hook (org-mode . olivetti-mode)
  :config
  ;; Resize Org headings
  (custom-set-faces
   '(org-document-title ((t (:height 1.6))))
   '(outline-1          ((t (:height 1.25))))
   '(outline-2          ((t (:height 1.2))))
   '(outline-3          ((t (:height 1.15))))
   '(outline-4          ((t (:height 1.1))))
   '(outline-5          ((t (:height 1.1))))
   '(outline-6          ((t (:height 1.1))))
   '(outline-8          ((t (:height 1.1))))
   '(outline-9          ((t (:height 1.1)))))
  (org-indent-mode -1)
  (setq org-startup-with-latex-preview t)
  (let ((png (cdr (assoc 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter '("dvipng -D %D -T tight -bg Transparent -o %O %F")))
  (setq org-startup-folded 'content)
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-pretty-entities t
        org-ellipsis "  ·")
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)
  (setq org-log-done                       t
        org-auto-align-tags                t
        org-tags-column                    -80
        org-fold-catch-invisible-edits     'show-and-error
        org-special-ctrl-a/e               t
        org-insert-heading-respect-content t))

(use-package org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ;; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ;; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ;; Show links
        org-appear-autosubmarkers t)) ;; Show sub- and superscripts

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package svg-tag-mode
  :after org
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
	(svg-image (svg-lib-concat
				(svg-lib-progress-bar (/ (string-to-number value) 100.0)
			      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
				(svg-lib-tag (concat value "%")
				  nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
	(let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
	  (svg-image (svg-lib-concat
				  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
				  (svg-lib-tag value nil
					:stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags
      `(;; Org tags
        ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
          (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
          (svg-progress-count (substring tag 1 -1)))))

        ;; TODO / DONE
        ;; ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo
		;; 									           :inverse t :margin 0))))
        ;; ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))


        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

(add-hook 'org-mode-hook 'svg-tag-mode)

(defun soph/prettify-symbols-setup ()
  "Beautify keywords"
  (setq prettify-symbols-alist
		(mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
				'(; Greek symbols
				  ("lambda" . ?λ)
				  ("delta"  . ?Δ)
				  ("gamma"  . ?Γ)
				  ("phi"    . ?φ)
				  ("psi"    . ?ψ)
				  ; Org headers
				  ("#+title:"  . "")
				  ("#+author:" . "")
                  ("#+date:"   . "")
				  ; Checkboxes
				  ("[ ]" . "")
				  ("[X]" . "")
				  ("[-]" . "")
				  ; Blocks
				  ("#+begin_src"   . "") ; 
				  ("#+end_src"     . "")
				  ("#+begin_QUOTE" . "‟")
				  ("#+begin_QUOTE" . "”")
				  ; Drawers
				  ;    ⚙️
				  (":properties:" . "")
				  ; Agenda scheduling
				  ("SCHEDULED:"   . "🕘")
				  ("DEADLINE:"    . "⏰")
				  ; Agenda tags  
				  (":@projects:"  . "☕")
				  (":work:"       . "🚀")
				  (":@inbox:"     . "✉️")
				  (":goal:"       . "🎯")
				  (":task:"       . "📋")
				  (":@thesis:"    . "📝")
				  (":thesis:"     . "📝")
				  (":uio:"        . "🏛️")
				  (":emacs:"      . "")
				  (":learn:"      . "🌱")
				  (":code:"       . "💻")
				  (":fix:"        . "🛠️")
				  (":bug:"        . "🚩")
				  (":read:"       . "📚")
				  ; Roam tags
				  ("#+filetags:"  . "📎")
				  (":wip:"        . "🏗️")
				  (":ct:"         . "➡️") ; Category Theory
                                  ; ETC
                                  (":verb:"       . "🌐") ; HTTP Requests in Org mode
				  )))
  (prettify-symbols-mode))
(add-hook 'org-mode-hook        #'soph/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'soph/prettify-symbols-setup)

(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords 'org-mode
                        `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
                           (1 `(face nil
                                     display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
                              prepend))) t)

(defun soph/org-present-prepare-slide ()
  ;; Show only top-level headlines
  (org-overview)
  ;; Unfold the current entry
  (org-fold-show-entry)
  ;; Show only direct subheadings of the slide but don't expand them
  (org-fold-show-children))

(defun soph/org-present-start ()
  ;; Tweak font sizes
  (setq-local
   face-remapping-alist '((default (:height 1.5) variable-pitch)
                          (header-line (:height 3.0) variable-pitch)
                          (org-document-title (:height 1.75) org-document-title)
                          (org-code (:height 1.55) org-code)
                          (org-verbatim (:height 1.55) org-verbatim)
                          (org-block (:height 1.25) org-block)
                          (org-block-begin-line (:height 0.7) org-block)))
  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " "))

(defun soph/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil))


(use-package org-present
  :defer t
  :hook
  ((org-present-after-navigate-functions . soph/org-present-prepare-slide)
  (org-present-mode                      . soph/org-present-start)
  (org-present-mode-quit                 . soph/org-present-end)))

(use-package org-modern
  :config
  (setq
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Don't style the following
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil
   org-ellipsis "."
   )

  (global-org-modern-mode))
