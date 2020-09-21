;;; bindings.el --- Key Bindings & Related Functions

(defun open-last-buffer ()
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

   ;; Toggles 
  "t" '(:ignore t :which-key "Toggles")
  "tl" 'display-line-numbers-mode
  "tw" 'toggle-truncate-lines
  "tt" 'counsel-load-theme
  "tv" 'variable-pitch-mode
  "tf" 'flycheck-mode
  "ts" 'flyspell-mode
  "tz" 'wc-mode

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
  "hv" 'counsel-describe-variable
  "ho" 'counsel-describe-symbol
  "hf" 'counsel-describe-function
  "hs" 'describe-syntax
  "hw" 'where-is

  ;; Applications
  "o" '(:ignore t :which-key "Open")
  "oe" 'eshell
  "ot" 'treemacs
  "of" 'reveal-in-osx-finder)
 
