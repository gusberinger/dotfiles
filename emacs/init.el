(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;;; Emacs Settings
(setq vc-follow-symlinks t
      ispell-program-name "/usr/local/bin/aspell"
      backup-directory-alist `(("." . "~/.saves"))
      create-lockfiles nil
      custom-file "~/.emacs.d/custom.el"
      recentf-max-menu-items 25
      recentf-max-saved-items 25
      user-full-name "Gus Beringer")

(if (eq system-type 'darwin)
    (load-file "~/dotfiles/emacs/macos.el"))

(recentf-mode 1)
(advice-add #'yes-or-no-p :override #'y-or-n-p)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Supress Startup Message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Evil Settings
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
  evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-magit
  :after evil
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))


;; Linting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Theme Settings
(global-visual-line-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-solarized-light t))

(use-package treemacs
  :ensure t)

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))

;; Auto-completion and Counsel
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package ivy
  :ensure t
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))

(use-package counsel
  :ensure t
  :config
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package swiper
  :ensure t)

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/dotfiles/emacs/custom-snippets"))
  (yas-global-mode 1))


;; Org Settings
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (add-hook 'prettify-symbols-mode (lambda () (setq-local org-hide-leading-stars nil)))
(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org")
	deft-directory "~/Dropbox/notes"
	deft-recursive t))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :ensure t
  :config
  (setq org-superstar-leading-bullet ?\s
	org-superstar-leading-fallback ?\s
	org-superstar-item-bullet-alist "•"))
	;; org-hide-leading-stars nil))

(use-package mixed-pitch
  :ensure t
  :hook
  (text-mode . mixed-pitch-mode))

; code highliting in html exported src blocks
(use-package htmlize
  :ensure t)

(setq org-hide-emphasis-markers t
      org-html-htmlize-output-type 'css
      org-src-fontify-natively t)

(use-package org-gcal
  :ensure t
  :config
  (load-file "~/Dropbox/emacs/org-gcal-config.el")
  (setq org-gcal-file-alist '(("berin013@umn.edu" .  "~/Dropbox/notes/gcal.org"))))
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
(setq org-hide-emphasis-markers t)

;; Epub Settings
(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"))
                                           ;; :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;; Spellcheck
(with-eval-after-load "ispell"
	(setq ispell-program-name "aspell")
	(ispell-set-spellchecker-params)
	(setq ispell-dictionary "en_US"))

;; Git Settings
(use-package magit
  :ensure t)

(use-package git-gutter
  :after magit
  :ensure t
  :config
  (global-git-gutter-mode 1)
  :hook
  (magit-post-refresh . #'git-gutter:update-all-windows))


;; Load Language Specific Settings
(load-file "~/dotfiles/emacs/latex.el")
(load-file "~/dotfiles/emacs/R.el")



;; Functions for Keybindings
(defun my-shell-setup ()
	(interactive)
	(setq buffer-face-mode-face '(:family "Hack Nerd Font" :height 160))
	(buffer-face-mode))
(add-hook 'eshell-mode-hook #'my-shell-setup)

(defun last-buffer ()
  "Open the last buffer."
  (interactive)
  (switch-to-buffer nil))

(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun shell-new-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;; Keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package general
  :ensure t)

(general-create-definer my-leader-def
  :prefix "SPC")

(general-create-definer my-local-leader-def
  :prefix "SPC m")

(my-leader-def '(normal emacs)
  "TAB" 'last-buffer
  "SPC" 'counsel-M-x
  "a" 'org-agenda
  "u" 'counsel-bookmark
  "c" 'org-capture
  "n" 'deft

  ;; Files
  "f" '(:ignore t :which-key "Files")
  "fo" 'counsel-recentf
  "fp" 'open-init-file

   ;; Display Settings
  "t" '(:ignore t :which-key "Toggles")
  "tl" 'display-line-numbers-mode
  "tw" 'toggle-truncate-lines
  "tt" 'counsel-load-theme
  "tv" 'variable-pitch-mode

  ;; Buffers
  "b" '(:ignore t :which-key "Buffers")
  "bb" 'ivy-switch-buffer
  "b]" 'next-buffer
  "b[" 'previous-buffer
  "bx" 'kill-buffer
  "bd" 'evil-window-delete

  ;; Windows
  "w" '(:ignore t :which-key "Workspace")
  "wn" 'make-frame-command
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "wK" 'evil-window-split
  "wJ" 'evil-window-split
  "wH" 'evil-window-vsplit
  "wL" 'evil-window-vsplit

  ;; Magit
  "g" '(:ignore t :which-key "Magit")
  "gg" 'magit-status
  "gc" 'magit-commit
  "gf" 'magit-fetch
  "gF" 'magit-pull
  "gs" 'magit-stage-file
  "gu" 'magit-unstage-file
  "gp" 'magit-push
  "gd" 'magit-diff

  ;; Documentation
  "h" '(:ignore t :which-key "Help")
  "hv" 'describe-variable
  "ho" 'describe-symbol

  ;; Applications
  "o" '(:ignore t :which-key "Open")
  "oe" 'eshell
  "ot" 'treemacs
  "of" 'reveal-in-osx-finder)
  
(my-local-leader-def '(normal emacs) emacs-lisp-mode-map
  "c" 'eval-buffer
  "g" 'elint-current-buffer)

(my-local-leader-def '(normal emacs) org-mode-map
  "s" 'org-edit-special)

(my-local-leader-def '(normal emacs) LaTeX-mode-map
  "a" 'TeX-command-run-all
  "b" 'LaTeX-build
  "c" 'reftex-citation

  "r" '(:ignore t :which-key "Reftex")
  "rc"    'reftex-citation
  "rg"    'reftex-grep-document
  "ri"    'reftex-index-selection-or-word
  "rI"    'reftex-display-index
  "r TAB" 'reftex-index
  "rl"    'reftex-label
  "rp"    'reftex-index-phrase-selection-or-word
  "rP"    'reftex-index-visit-phrases-buffer
  "rr"    'reftex-reference
  "rs"    'reftex-search-document
  "rt"    'reftex-toc
  "rT"    'reftex-toc-recenter
  "rv"    'reftex-view-crossref)
