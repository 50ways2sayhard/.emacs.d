;;; init-dart.el ---
;;
;; Filename: init-dart.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Tue Jun  1 16:36:05 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 126
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-and-compile
  (require 'init-const)
  (require 'init-func))

(use-package dart-mode
  :mode ("\\.dart\\'")
  :hook ((dart-mode . (lambda ()
                        (setq-local lsp-enable-imenu t)
                        (setq-local lsp-diagnostics-provider :flymake)
                        (add-hook 'lsp-mode-hook
                                  (lambda ()
                                    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                                    (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
                        ))
         (eglot-managed-mode . (lambda ()
                                 (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
                                 (add-hook 'before-save-hook '+eglot-organize-imports nil t)
                                 ))
         )
  :config
  (require 'prog/+flutter)
  ;; (defun project-try-dart (dir)
  ;;   (let ((project (or (locate-dominating-file dir "pubspec.yaml")
  ;;                      (locate-dominating-file dir "BUILD"))))
  ;;     (if project ;;         (cons 'dart project)
  ;;       (cons 'transient dir))))
  ;; (add-hook 'project-find-functions #'project-try-dart)
  ;; (cl-defmethod project-roots ((project (head dart)))
  ;;   (list (cdr project)))

  (local-leader-def
    :keymaps 'dart-mode-map
    "r" '(+my/flutter-run-or-hot-reload :wk "Run or hot reload")
    "R" '(+my/flutter-run-or-hot-restart :wk "Run or hot restart")

    "v" '(+my/flutter-open-devtools :wk "Open devtools")
    "Q" '(+my/flutter-quit :wk "Quit application")

    "s" '(+my/flutter-run-or-attach :wk "Run or Attach")
    )
  )

(use-package lsp-dart
  :after lsp-mode
  :custom
  (lsp-dart-dap-flutter-hot-reload-on-save t)
  (lsp-dart-flutter-widget-guides nil)
  (lsp-dart-flutter-fringe-colors nil)
  (lsp-dart-outline nil)
  (lsp-dart-flutter-outline nil)
  (lsp-dart-closing-labels nil)
  (lsp-dart-main-code-lens nil)
  (lsp-dart-test-code-lens nil)
  )

(provide 'init-dart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
