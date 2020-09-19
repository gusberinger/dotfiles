;; Latex Settings
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
