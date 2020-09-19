(use-package reveal-in-osx-finder
  :ensure t)

;; Fix macOS Path Issues
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)


(use-package osx-trash
  :ensure t
  :commands osx-trash-move-file-to-trash
  :init
  ;; Delete files to trash on macOS, as an extra layer of precaution against
  ;; accidentally deleting wanted files.
  (setq delete-by-moving-to-trash t))

