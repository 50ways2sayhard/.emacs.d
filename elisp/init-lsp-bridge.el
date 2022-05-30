;;; init-lsp-bridge.el --- summary -*- lexical-binding: t -*-

;; Author: John
;; Maintainer: John
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package lsp-bridge
  :straight nil
  :load-path "site-lisp/lsp-bridge-self/"
  :config
  (setq lsp-bridge-completion-provider 'corfu)
  (global-lsp-bridge-mode)

  (add-to-list 'lsp-bridge-completion-stop-commands #'evil-escape)

  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))

  (add-to-list 'lsp-bridge-enable-popup-predicates
               '((lambda ()
                   (and
                    (< corfu--index 0) ; not select a candidate
                    (or (not (featurep 'evil)) (evil-insert-state-p))))))
  )

(provide 'init-lsp-bridge)

;;; lsp-bridge.el ends here
