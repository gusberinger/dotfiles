;;; writing.el --- Settings for Writing

(add-hook 'text-mode-hook 'flyspell-mode)

(defun add-word-to-personal-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))) 

(with-eval-after-load "ispell"
	(setq ispell-program-name "aspell")
	(ispell-set-spellchecker-params)
	(setq ispell-dictionary "en_US"))

;; Use fixed-pitch in tables and src code blocks
(use-package mixed-pitch
  :ensure t
  :hook
  (text-mode . mixed-pitch-mode))

;; Notes Management
(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org")
	deft-directory "~/Dropbox/notes"
	deft-recursive t))

(use-package wc-mode
  :ensure t)

This is a count of the amount of words sdfsdf 
