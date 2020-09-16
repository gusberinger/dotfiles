(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(setq vc-follow-symlinks t
      ispell-program-name "/usr/local/bin/aspell"
      backup-directory-alist `(("." . "~/.saves"))
      create-lockfiles nil
      custom-file "~/.emacs.d/custom.el"
      recentf-max-menu-items 25
      recentf-max-saved-items 25
      user-full-name "Gus Beringer")

(advice-add #'yes-or-no-p :override #'y-or-n-p)
(recentf-mode 1)
(global-visual-line-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(if nil (string-equal system-type "darwin")
    (menu-bar-mode -1)
  (menu-bar-mode 1))

;; from doom
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-solarized-light t))

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

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/dotfiles/emacs/custom-snippets"))
  (yas-global-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package treemacs
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org")
	deft-directory "~/Dropbox/notes"
	deft-recursive t))

(use-package ess
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package mixed-pitch
  :ensure t
  :hook
  (text-mode . mixed-pitch-mode))

; code highliting in exported src blocks
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

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

; swap - with •
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-4 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-3 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-2 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-1 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-document-title ((t (,@headline ,@variable-tuple :underline t))))))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"))
                                           ;; :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(with-eval-after-load "ispell"
	(setq ispell-program-name "aspell")
	(ispell-set-spellchecker-params)
	(setq ispell-dictionary "en_US"))

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

(use-package magit
  :ensure t)

(use-package evil-magit
  :after magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

(use-package company-auctex
  :ensure t
  :after company)

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(defun LaTeX-build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))

(setq debug-on-error t)

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX"))
(defvar latex-enable-auto-fill t)
(defvar latex-enable-folding nil)
(defvar latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tikzpicture"))

(setq TeX-PDF-mode t
      auctex-latexmk-inherit-TeX-PDF-mode t
      reftex-plug-into-AUCTeX t)

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
  "gs" 'magit-stage

  ;; Applications
  "o" '(:ignore t :which-key "Open")
  "oe" 'eshell
  "ot" 'treemacs)

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
