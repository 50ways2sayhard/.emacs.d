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

(defvar acm-orderless-matching-styles '(orderless-regexp orderless-initialism))

(defun acm-match-orderless (keyword candidate)
  (string-match-p (car (orderless-pattern-compiler (downcase keyword) orderless-matching-styles)) (downcase candidate)))

(defun +acm-setup ()

  (corfu-mode -1)
  (setq acm-candidate-match-function #'orderless-flex
        acm-enable-yas nil
        acm-enable-search-words nil
        acm-enable-tabnine-helper t)

  ;; (advice-add #'acm-candidate-fuzzy-search :override #'acm-match-orderless)


  (when (boundp 'acm-mode-map)
    (define-key evil-insert-state-map (kbd "TAB") nil)
    (define-key acm-mode-map (kbd "C-j") 'acm-complete)
    (define-key acm-mode-map (kbd "C-q") 'acm-complete-quick-access)))

(use-package lsp-bridge
  :disabled
  ;; :straight (:host github :repo "50ways2sayhard/lsp-bridge" :branch "dev" :files ("*.el" "*.py" "core/*" "langserver/*"))
  :commands (lsp-bridge-mode)
  :straight nil
  :load-path "site-lisp/lsp-bridge/"
  ;; :load-path "site-lisp/lsp-bridge-dev/"
  :hook (((python-mode dart-mode js-mode) . lsp-bridge-mode)
         (lsp-bridge-mode . (lambda ()
                              (if (boundp 'acm-mode-map)
                                  (+acm-setup)
                                (my/set-lsp-bridge-capf))
                              (leader-def :keymaps 'override
                                "ca" '(lsp-bridge-code-action :wk "Code Action")
                                "cF" '(lsp-bridge-find-impl :wk "Find implementation")
                                "cD" '(lsp-bridge-find-references :wk "Find references")
                                "cd" '(lsp-bridge-find-def :wk "Find definition")
                                "ck" '(lsp-bridge-lookup-documentation :wk "Lookup documentation"))
                              (setq-local corfu-auto-prefix 0)

                              (evil-define-key 'normal 'global
                                "K" 'lsp-bridge-lookup-documentation))))
  :config
  (setq lsp-bridge-enable-diagnostics nil)
  (setq lsp-bridge-enable-signature-help nil)
  (setq lsp-bridge-lookup-doc-tooltip-border-width 2)

  (add-to-list 'lsp-bridge-completion-stop-commands #'evil-escape)

  (add-to-list 'lsp-bridge-completion-popup-predicates
               '((lambda ()
                   (and
                    (< corfu--index 0)))))
  (add-hook 'lsp-bridge-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))
  )

(use-package lspce
  :disabled
  :straight nil
  :load-path "site-lisp/lspce/"
  :hook (((dart-mode python-mode) . lspce-mode)
         ((lspce-mode) . (lambda ()
                           (setq-local corfu-auto-delay 0)
                           (setq-local corfu-auto-prefix 1)
                           (leader-def :keymaps 'override
                             "ca" '(lspce-code-actions :wk "Code Actions")
                             "cr" '(lspce-rename :wk "Rename symbol")
                             "ck" '(lspce-help-at-point :wk "Documentation at point")
                             "cs" '(lspce-signature-at-point :wk "Signature at point")
                             )
                           (setq completion-category-defaults nil)
                           (setq-local completion-at-point-functions (my/convert-super-capf #'lspce-completion-at-point))
                           )))
  :config
  (add-to-list 'lspce-server-programs '("dart" "/usr/local/bin/dart" "language-server" lspce-dart-initializationOptions))
  (lspce-set-log-file "/Users/johngong/.emacs.d/.local/cache/lspce.log")
  (setq lspce-enable-flymake nil
        lspce-send-changes-idle-time 0.1
        lspce-eldoc-enable-signature t)


  (defun lspce-dart-initializationOptions ()
    (let ((options (make-hash-table :test #'equal)))
      (setq options (lspce--add-option "dart.completeFunctionCalls" t options))
      (setq options (lspce--add-option "dart.enableSnippets" t options))
      options
      )
    )
  )

(provide 'init-lsp-bridge)

;;; lsp-bridge.el ends here
