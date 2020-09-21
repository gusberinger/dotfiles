;;; ui.el -- User Interface

(global-visual-line-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(desktop-save-mode 1)


(use-package base16-theme
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))
