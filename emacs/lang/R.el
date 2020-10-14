;;; R.el --- Emacs Speaks Statistics Configuration

(use-package ess
  :ensure t
  :config
  (setq ess-directory "/tmp"
	ess-r-package-use-dir t
	ess-ask-for-ess-directory nil))
