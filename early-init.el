;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib/compat" dir))
  (add-to-list 'load-path (expand-file-name "lib/packed" dir))
  (add-to-list 'load-path (expand-file-name "lib/auto-compile" dir)))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq package-enable-at-startup nil)

(setq use-package-enable-imenu-support t
      use-package-expand-minimally t
      use-package-compute-statistics t)
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               (cons "melpa" "https://melpa.org/packages/")
               t))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
