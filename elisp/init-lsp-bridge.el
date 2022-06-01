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
  :straight (:host github :repo "50ways2sayhard/lsp-bridge" :branch "dev" :files ("*.el" "*.py" "core/*" "langserver/*"))
  ;; :straight nil
  ;; :load-path "site-lisp/lsp-bridge-self/"
  :config
  (setq lsp-bridge-completion-provider 'corfu)
  (global-lsp-bridge-mode)

  (add-to-list 'lsp-bridge-completion-stop-commands #'evil-escape)

  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (leader-def :keymaps 'override
                "cr" '(lsp-bridge-rename :wk "Rename symbol")
                "cF" '(lsp-bridge-find-impl :wk "Find implementation")
                "cD" '(lsp-bridge-find-references :wk "Find references")
                "cd" '(lsp-bridge-find-def :wk "Find definition")
                "ck" '(lsp-bridge-lookup-documentation :wk "Lookup documentation")
                )

              (evil-collection-define-key 'normal 'global
                (kbd "K") 'lsp-bridge-lookup-documentation)

              (evil-define-key 'normal 'global
                "K" 'lsp-bridge-lookup-documentation)
              ))

  (add-to-list 'lsp-bridge-completion-popup-predicates
               '((lambda ()
                   (and
                    (< corfu--index 0)))))
  )

(provide 'init-lsp-bridge)

;;; lsp-bridge.el ends here
