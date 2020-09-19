;;; tools.el --- Editor's Tools

;; Linting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
		flycheck-check-syntax-automatically '(save mode-enabled)
		flycheck-display-errors-delay 0.25))

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

;; Git
(use-package magit
  :ensure t)

(use-package git-gutter
  :after magit
  :ensure t
  :config
  (global-git-gutter-mode 1)
  :hook
  (magit-post-refresh . #'git-gutter:update-all-windows))


;; Shell Setup
(defun my-shell-setup ()
	(interactive)
	(setq buffer-face-mode-face '(:family "Hack Nerd Font" :height 160))
	(buffer-face-mode))
(add-hook 'eshell-mode-hook #'my-shell-setup)
 
