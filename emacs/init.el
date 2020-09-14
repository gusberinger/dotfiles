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
      custom-file "~/.emacs.d/custom.el"
      org-src-tab-acts-natively t
      recentf-max-menu-items 25
      recentf-max-saved-items 25
      user-full-name "Gus Beringer")

(recentf-mode 1)
(global-visual-line-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(if nil (string-equal system-type "darwin")
    (menu-bar-mode -1)
  (menu-bar-mode 1))

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

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.0))
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

(use-package company-auctex
  :ensure t
  :after company)

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup))

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
      auctex-latexmk-inherit-TeX-PDF-mode t)
;; LaTeX
;; (setq TeX-command-default latex-build-command
;;       TeX-command-default latex-build-command
;;       TeX-auto-save t)

;; LaTeX Settings

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

  ;; Windows
  "w" '(:ignore t :which-key "Windows")
  "wj" 'split-window-below
  "wh" 'split-window-right
  "wn" 'make-frame-command

  ;; Magit
  "g" '(:ignore t :which-key "Magit")
  "gg" 'magit-status
  
  ;; Applications
  "o" '(:ignore t :which-key "Open")
  "oe" 'eshell
  "ot" 'treemacs)

(my-local-leader-def '(normal emacs) emacs-lisp-mode-map
  "c" 'eval-buffer
  "g" 'elint-current-buffer)

(my-local-leader-def '(normal emacs) LaTeX-mode-map
  "a" 'TeX-command-run-all)
  ;; "b" 'latex/build)
