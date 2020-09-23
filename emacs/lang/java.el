(use-package lsp-java
  :ensure t
  :hook
  ('java-mode . #'lsp))

(message "Worked!")
