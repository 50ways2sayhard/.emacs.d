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
;;     Update #: 61
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
  :hook (dart-mode . (lambda ()
                       (setq-local lsp-enable-imenu t)
                       (setq-local lsp-diagnostics-provider :flycheck)
                       ;; (add-hook 'after-save-hook #'flutter-run-or-hot-reload nil t)
                       ))
  :config
  (setq dart-format-on-save t)
  (with-eval-after-load 'lsp
    (setq lsp-dart-dap-flutter-hot-reload-on-save t)
    (setq-local lsp-diagnostics-provider :flycheck)))

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp)
  :custom
  (lsp-dart-dap-flutter-hot-reload-on-save t)
  (lsp-dart-flutter-widget-guides nil)
  (lsp-dart-flutter-fringe-colors nil)
  :config
  (with-eval-after-load 'dap-mode
    (dap-register-debug-template "Flutter :: Attach"
                                 (list
                                  :request "attach"
                                  :type "dart"
                                  :dap-server-path lsp-dart-dap-flutter-debugger-program
                                  :program (lsp-dart-get-project-entrypoint)
                                  :output-filter-function #'lsp-dart-dap--output-filter-function
                                  )))
  (with-eval-after-load 'lsp
    (make-local-variable 'before-save-hook)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (local-leader-def
    :keymaps 'dart-mode-map
    "r" 'lsp-dart-dap-flutter-hot-reload
    "R" 'lsp-dart-dap-flutter-hot-restart

    "D" 'lsp-dart-open-devtools
    "o" 'lsp-dart-show-outline
    "p" 'lsp-dart-pub-get
    )
  )

(provide 'init-dart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
