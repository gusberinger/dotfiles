;;; latex.el --- LaTeX Settings

(use-package company-auctex
  :ensure t
  :after company)

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq TeX-PDF-mode t
      auctex-latexmk-inherit-TeX-PDF-mode t
      reftex-plug-into-AUCTeX t
      TeX-master nil
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server t)
(eval-after-load "tex"
  '(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))
(require 'tex)
(defun TeX-synctex-output-page ()
  "Return the page corresponding to the current source position.
This method assumes that the document was compiled with SyncTeX
enabled and the `synctex' binary is available."
  (let ((synctex-output
	 (with-output-to-string
	   (call-process "synctex" nil (list standard-output nil) nil "view"
			 "-i" (format "%s:%s:%s" (line-number-at-pos)
				      (current-column)
				      ;; The real file name (not symbolic) fixed
				      ;; for the synctex path bug
                                      (concat (file-name-directory (file-truename (buffer-file-name)))
                                              "./"
                                              (file-name-nondirectory (buffer-file-name))))
			 "-o" (TeX-active-master (TeX-output-extension))))))
    (if (string-match "Page:\\([0-9]+\\)" synctex-output)
	(match-string 1 synctex-output)
      "1")))

;; Bindings
(my-local-leader-def '(normal emacs) LaTeX-mode-map
  "a" 'TeX-command-run-all
  "b" 'LaTeX-build
  "c" 'reftex-citation
  "z" 'LaTeX-build-on-save-mode

  "r" '(:ignore t :which-key "Reftex")
  "rc"    'reftex-citation
  "rg"    'reftex-grep-document
  "ri"    'reftex-index-selection-or-word
  "rI"    'reftex-display-index
  "r TAB" 'reftex-index
  "rl"    'reftex-label
  "rp"    'reftex-index-phrase-selection-or-word
  "rP"    'reftex-index-visit-phrases-buffer
  "rr"    'reftex-reference
  "rs"    'reftex-search-document
  "rt"    'reftex-toc
  "rT"    'reftex-toc-recenter
  "rv"    'reftex-view-crossref)

;; PDF Tools
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
(pdf-tools-install)

;; Compilation Settings
(defun LaTeX-build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))
(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX"))

(define-minor-mode LaTeX-build-on-save-mode
  "Runs latexmk on file after save"
  :lighter "BoS"
  (if LaTeX-build-on-save-mode
  (progn  (make-local-variable 'after-save-hook)
      (add-hook 'after-save-hook 'LaTeX-build nil t))
    (kill-local-variable 'after-save-hook)))

