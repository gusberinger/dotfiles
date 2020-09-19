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

;; Supress Startup Message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)


(load-file "~/dotfiles/emacs/evil.el")

;; Linting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
		flycheck-check-syntax-automatically '(save mode-enabled)
		flycheck-display-errors-delay 0.25))

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

;; Load Bindings
(load-file "~/dotfiles/emacs/bindings.el")

;; Shell Setup
(defun my-shell-setup ()
	(interactive)
	(setq buffer-face-mode-face '(:family "Hack Nerd Font" :height 160))
	(buffer-face-mode))
(add-hook 'eshell-mode-hook #'my-shell-setup)
 
(my-local-leader-def '(normal emacs) emacs-lisp-mode-map
  "c" 'eval-buffer
  "g" 'elint-current-buffer)

(my-local-leader-def '(normal emacs) org-mode-map
  "s" 'org-edit-special)


;; Load Language Specific Settings
(load-file "~/dotfiles/emacs/latex.el")
(load-file "~/dotfiles/emacs/R.el")
(load-file "~/dotfiles/emacs/writing.el")
(load-file "~/dotfiles/emacs/org.el")
(load-file "~/dotfiles/emacs/nov.el")
