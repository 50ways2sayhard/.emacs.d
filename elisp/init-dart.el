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
;;     Update #: 27
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
                       (add-hook 'after-save-hook #'flutter-run-or-hot-reload nil t)))
  :config
  (setq dart-format-on-save t)
  (with-eval-after-load 'flutter
    (local-leader-def
      :keymaps 'flutter-mode-map
      "r" 'flutter-run-or-hot-reload))
  (with-eval-after-load 'lsp
    (setq-local lsp-diagnostics-provider :flycheck)))

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

(use-package flutter
  :defer t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :config
  (setq-local lsp-diagnostics-provider :flycheck)
  (if *sys/linux*
      (setq flutter-sdk-path "/usr/share/flutter/"))
  )

(provide 'init-dart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
