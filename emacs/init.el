(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;;;; Emacs Settings
(advice-add #'yes-or-no-p :override #'y-or-n-p)
(recentf-mode 1)
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

;; Load Editor's Tools
(load-file "~/dotfiles/emacs/bindings.el")
(load-file "~/dotfiles/emacs/ui.el")
(load-file "~/dotfiles/emacs/evil.el")
(load-file "~/dotfiles/emacs/editing.el")
(load-file "~/dotfiles/emacs/writing.el")

;; Load Language Specific Settings
(load-file "~/dotfiles/emacs/lang/latex.el")
(load-file "~/dotfiles/emacs/lang/R.el")
(load-file "~/dotfiles/emacs/lang/elisp.el")
(load-file "~/dotfiles/emacs/lang/org.el")

;; Load Apps
(load-file "~/dotfiles/emacs/apps/nov.el")
