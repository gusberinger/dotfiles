;;; org.el --- Org Mode Settings

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(setq org-hide-emphasis-markers t
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t)

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :ensure t
  :config
  (setq org-superstar-leading-bullet ?\s
	org-superstar-leading-fallback ?\s
	org-superstar-item-bullet-alist "•"))
	;; org-hide-leading-stars nil))


; code highliting in html exported src blocks
(use-package htmlize
  :ensure t)

(use-package org-gcal
  :ensure t
  :config
  (load-file "~/Dropbox/emacs/org-gcal-config.el")
  (setq org-gcal-file-alist '(("berin013@umn.edu" .  "~/Dropbox/notes/gcal.org"))))
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
(setq org-hide-emphasis-markers t)

