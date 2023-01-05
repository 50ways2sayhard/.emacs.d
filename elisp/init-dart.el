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
                        (format-all-mode t)))
         (eglot-managed-mode . (lambda ()
                                 (add-hook 'before-save-hook (lambda ()
                                                               (call-interactively 'eglot-code-action-organize-imports)
                                                               ) nil t)))
         )
  :config
  (require 'prog/+flutter)

  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(dart-mode :types
                                                   ((?c "Class"    font-lock-type-face)
                                                    (?e "Enum" font-locl-type-face)
                                                    (?V "Constructor" font-lock-type-face)
                                                    (?C "Constant"    font-lock-constant-face)
                                                    (?f "Function"  font-lock-function-name-face)
                                                    (?m "Method"  font-lock-function-name-face)
                                                    (?p "Property" font-lock-variable-name-face)
                                                    (?F "Field"  font-lock-variable-name-face)))))

  (local-leader-def
    :keymaps 'dart-mode-map
    "r" '(+my/flutter-run-or-hot-reload :wk "Run or hot reload")
    "R" '(+my/flutter-run-or-hot-restart :wk "Run or hot restart")

    "v" '(+my/flutter-open-devtools :wk "Open devtools")
    "Q" '(+my/flutter-quit :wk "Quit application")

    "s" '(+my/flutter-run-or-attach :wk "Run or Attach")
    "p" '(+my/flutter-pub-get :wk "Pub get")
    )
  )

(provide 'init-dart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
