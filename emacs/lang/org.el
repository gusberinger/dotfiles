;;; org.el --- Org Mode Settings

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)

(setq org-hide-emphasis-markers t
      org-html-htmlize-output-type 'css
      org-src-preserve-indentation t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-hide-leading-stars nil
      org-highlight-latex-and-related '(native script entities))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)
			     (java   . t)
			     (R      . t)))


(setq-default prettify-symbols-alist
	      '(("#+BEGIN_SRC" . "λ")
                ("#+END_SRC" . "λ")
                ("#+begin_src" . "λ")
                ("#+end_src" . "λ")
		("#+BEGIN_QUOTE" . "“")
                ("#+END_QUOTE" . "”")
                ("#+begin_quote" . "“")
                ("#+end_quote" . "”")
		("---" . "—")
		("\\\\" . " ")
                (">=" . "≥")
                ("=>" . "⇨")))

;; Nice looking bullets
;; (use-package org-superstar
;;   :hook
;;   (org-mode . org-superstar-mode)
;;   :ensure t
;;   :config
;;   (setq org-superstar-leading-bullet ?\s
;; 	org-superstar-leading-fallback ?\s
;; 	org-superstar-item-bullet-alist "•"))


(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
				org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
				org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
				org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"
				bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
				bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
				bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

	;; open pdf with system pdf viewer (works on mac)
	(setq bibtex-completion-pdf-open-function
	  (lambda (fpath)
		  (start-process "open" "*open*" "open" fpath))))


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Better lists
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; Better fonts
(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; Code highlighting in html exported src blocks
(use-package htmlize
  :ensure t)

;; Google Calendar sync
(use-package org-gcal
  :ensure t
  :config
  (load-file "~/Dropbox/emacs/org-gcal-config.el")
  (setq org-gcal-file-alist
	'(("berin013@umn.edu" .  "~/Dropbox/notes/gcal.org"))))
;; Example Config for org-gcal-config.el
;; (setq org-gcal-client-id "client-id"
;;       org-gcal-client-secret "client-secret")

;; Bindings
(my-local-leader-def '(normal emacs) org-mode-map
  "s" 'org-edit-special
  "c" 'org-ref-cite
  "f" 'org-roam-find-file)

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-db-location "~/.emacs.d/roam.db")
  (org-roam-directory "~/Dropbox/notes/"))

(use-package company-org-roam
  :ensure t
  :config
  (push 'company-org-roam company-backends))
