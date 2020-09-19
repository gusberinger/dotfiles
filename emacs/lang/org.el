;;; org.el --- Org Mode Settings

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(setq org-hide-emphasis-markers t
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t
      org-hide-leading-stars nil)

(setq-default prettify-symbols-alist
	      '(("#+BEGIN_SRC" . "λ")
                ("#+END_SRC" . "λ")
                ("#+begin_src" . "λ")
                ("#+end_src" . "λ")
                (">=" . "≥")
                ("=>" . "⇨")))

;; Nice looking bullets
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :ensure t
  :config
  (setq org-superstar-leading-bullet ?\s
	org-superstar-leading-fallback ?\s
	org-superstar-item-bullet-alist "•"))

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
  "s" 'org-edit-special)

(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot"))
