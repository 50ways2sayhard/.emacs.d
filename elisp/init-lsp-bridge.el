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

(defun +acm-setup ()

  (global-corfu-mode -1)

  (when (boundp 'acm-mode-map)
    (define-key evil-insert-state-map (kbd "TAB") nil)
    (define-key acm-mode-map (kbd "<tab>") 'acm-select-next)
    (define-key acm-mode-map (kbd "<s-tab>") 'acm-select-prev)
    (define-key acm-mode-map (kbd "<backtab>") 'acm-select-prev)
    (define-key acm-mode-map (kbd "C-j") 'acm-complete)
    )


  (defvar acm-orderless-styles '(orderless-flex orderless-initialism))
  (defun acm-match-orderless (keyword candidate)
    (string-match-p (car (orderless-pattern-compiler (downcase keyword) acm-orderless-styles)) (downcase candidate)))

  (advice-add #'acm-candidate-fuzzy-search :override #'acm-match-orderless)
  )

(use-package lsp-bridge
  ;; :straight (:host github :repo "50ways2sayhard/lsp-bridge" :branch "dev" :files ("*.el" "*.py" "core/*" "langserver/*"))
  :commands (lsp-bridge-mode)
  :straight nil
  ;; :load-path "site-lisp/lsp-bridge/"
  :load-path "site-lisp/lsp-bridge-dev/"
  :hook (((python-mode dart-mode js-mode) . lsp-bridge-mode)
         (lsp-bridge-mode . (lambda ()
                              (my/set-lsp-bridge-capf)
                              (leader-def :keymaps 'override
                                "cr" '(lsp-bridge-rename :wk "Rename symbol")
                                "cF" '(lsp-bridge-find-impl :wk "Find implementation")
                                "cD" '(lsp-bridge-find-references :wk "Find references")
                                "cd" '(lsp-bridge-find-def :wk "Find definition")
                                "ck" '(lsp-bridge-lookup-documentation :wk "Lookup documentation"))
                              (setq-local corfu-auto-prefix 0)

                              (evil-define-key 'normal 'global
                                "K" 'lsp-bridge-lookup-documentation))))
  :config
  ;; (global-lsp-bridge-mode)
  (setq lsp-bridge-enable-diagnostics nil)
  (setq lsp-bridge-completion-provider 'corfu)
  (setq lsp-bridge-lookup-doc-tooltip-border-width 10)

  (add-to-list 'lsp-bridge-completion-stop-commands #'evil-escape)

  (add-to-list 'lsp-bridge-completion-popup-predicates
               '((lambda ()
                   (and
                    (< corfu--index 0)))))
  (add-hook 'lsp-bridge-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))
  )

(provide 'init-lsp-bridge)

;;; lsp-bridge.el ends here
