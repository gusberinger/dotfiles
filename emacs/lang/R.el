;;; R.el --- Emacs Speaks Statistics Configuration

(use-package ess
  :ensure t
  :config
  (setq ess-directory "/tmp")
  (setq ess-ask-for-ess-directory nil))
