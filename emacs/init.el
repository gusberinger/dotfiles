(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

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
  (load-theme 'base16-zenburn t))

(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

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
  (yas-global-mode 1))


(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package doom-snippets
  :load-path "~/.emacs.d/doom-snippets"
  :after yasnippet)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(defun last-buffer ()
  (interactive)
  (switch-to-buffer nil))


(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
   "TAB" 'last-buffer
   "SPC" 'counsel-M-x
   "o" 'counsel-recentf
   
   ;; Display Settings
   "t" '(:ignore t :which-key "Toggles")
   "tl" 'display-line-numbers-mode
   "tw" 'toggle-truncate-lines
   

   ;; Buffers
   "b" '(:ignore t :which-key "Buffers")
   "bb" 'ivy-switch-buffer

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'dired))

(cutom-set-variables
 ;;custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(general which-key yasnippet use-package key-chord evil-commentary counsel company base16-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
