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
