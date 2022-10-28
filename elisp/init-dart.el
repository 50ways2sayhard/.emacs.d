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
                                 (add-hook 'before-save-hook '+eglot-organize-imports nil t)
                                 ))
         )
  :config
  (require 'prog/+flutter)
  ;; (defun project-try-dart (dir)
  ;;   (let ((project (or (locate-dominating-file dir "pubspec.yaml")
  ;;                      (locate-dominating-file dir "BUILD"))))
  ;;     (if project ;;         (cons 'dart project)
  ;;         (cons 'transient dir))))
  ;; (add-hook 'project-find-functions #'project-try-dart)
  ;; (cl-defmethod project-roots ((project (head dart)))
  ;;   (list (cdr project)))
  (setq +my/flutter-pub-host "http://pub.futuoa.com")

  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(dart-mode :types
                                                   ((?c "Class"    font-lock-type-face)
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

  (require 'flycheck)
  (flycheck-define-checker dart
    "Dart static analyzer using dartanalyze.
https://github.com/dart-lang/sdk/tree/master/pkg/analyzer_cli#dartanalyzer"
    :command ("dart" "analyze" source)
    :error-patterns
    ((error line-start "ERROR" "|" (= 2 (+ (any "A-Z" "a-z" "0-9" "_")) "|")
            (file-name) "|" line "|" column "|" (one-or-more (any digit)) "|"
            (message) line-end)

     (warning line-start "WARNING" "|" (= 2 (+ (any "A-Z" "a-z" "0-9" "_")) "|")
              (file-name) "|" line "|" column "|" (one-or-more (any digit)) "|"
              (message) line-end)

     (info line-start "INFO" "|" (= 2 (+ (any "A-Z" "a-z" "0-9" "_")) "|")
           (file-name) "|" line "|" column "|" (one-or-more (any digit)) "|"
           (message) line-end))
    :modes dart-mode)

  (add-to-list 'flycheck-checkers 'dart)
  )

(provide 'init-dart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
