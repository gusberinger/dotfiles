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
      reftex-plug-into-AUCTeX t)
(defun LaTeX-build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX"))
(defvar latex-nofill-env '("equation"
                           "equation*"
                           "align"
                           "align*"
                           "tabular"
                           "tikzpicture"))

;; PDF Tools
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
(pdf-tools-install)

;; Bindings
(my-local-leader-def '(normal emacs) LaTeX-mode-map
  "a" 'TeX-command-run-all
  "b" 'LaTeX-build
  "c" 'reftex-citation

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
