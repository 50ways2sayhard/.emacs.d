;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

(setq load-prefer-newer t)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib/compat" dir))
  (add-to-list 'load-path (expand-file-name "lib/packed" dir)))

(setq menu-bar-mode -1
      tool-bar-mode -1
      scroll-bar-mode -1)

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      file-name-handler-alist nil
      site-run-file nil
      default-frame-alist
      '((vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (ns-transparent-titlebar . t))

      mode-line-format nil
      byte-compile-warnings nil
      native-comp-async-report-warnings-errors nil
      warning-suppress-log-types '((comp) (bytecomp))
      display-time-default-load-average nil

      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil

      frame-inhibit-implied-resize t
      frame-resize-pixelwise t

      load-prefer-newer nil
      auto-mode-case-fold nil
      ring-bell-function 'ignore
      visible-bell nil

      bidi-inhibit-bpa t
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil

      fast-but-imprecise-scrolling t
      ffap-machine-p-known 'reject
      redisplay-skip-fontification-on-input t

      idle-update-delay 1.0

      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000
      create-lockfiles nil
      make-backup-files nil

      compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))

(setq use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-always-defer t
      use-package-always-ensure t
      use-package-compute-statistics nil)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               (cons "melpa" "https://melpa.org/packages/")
               t))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
