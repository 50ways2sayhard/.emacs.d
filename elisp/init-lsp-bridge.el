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

(defun acm-insert-common-or-complete()
  "Insert common prefix of menu or complete."
  (interactive)
  (let ((inhibit-message t)
        (num (length (acm-get-input-prefix))))
    (acm-insert-common)
    (when (= num (length (acm-get-input-prefix)))
      (acm-complete))))

(defun +acm-setup ()
  (corfu-mode -1)
  (setq
   ;; acm-candidate-match-function #'orderless-flex
   acm-enable-yas nil
   acm-enable-search-words t
   acm-enable-tabnine-helper t
   acm-enable-telega nil
   acm-enable-search-sdcv-words nil)

  (when (boundp 'acm-mode-map)
    (define-key acm-mode-map (kbd "TAB") 'acm-insert-common-or-complete)
    (define-key acm-mode-map (kbd "C-j") 'acm-complete)))

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
  (setq lsp-bridge-enable-diagnostics (not (boundp 'eglot-managed-mode)))
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

;; TODO Support didChangeConfiguration
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
                           (add-function :before-until (local 'imenu-create-index-function)
                                         #'lspce-imenu-create)
                           )))
  :init
  (advice-add #'lspce--clientCapabilities :around
              (lambda (fn)
                (let ((l (funcall fn)))
                  (plist-put (plist-get l :textDocument)
                             :documentSymbol
                             (list
                              :dynamicRegistration :json-false
                              :hierarchicalDocumentSymbolSupport t
                              :symbolKind `(:valueSet
                                            [,@(mapcar
                                                #'car lspce--symbol-kind-names)])))
                  l)))
  :config
  (add-to-list 'lspce-server-programs '("dart" "dart" "language-server"))
  (lspce-set-log-file (expand-file-name ".local/cache/lspce.log" user-emacs-directory))
  (setq lspce-enable-flymake nil
        lspce-send-changes-idle-time 0.1
        lspce-eldoc-enable-signature t)

  (defun lspce-imenu-create ()
    (cl-labels
        ((unfurl (obj)
           (if-let ((children (gethash "children" obj))
                    (name (gethash "name" obj)))
               (cons obj
                     (mapcar (lambda (c)
                               (puthash
                                "containerName"
                                (let ((existing (gethash "containerName" c)))
                                  (if existing (format "%s::%s" name existing)
                                    name)) c) c)
                             (mapcan #'unfurl children)))
             (list obj))))
      (mapcar
       (lambda (obj)
         (cons
          (cdr (assoc (car obj) lspce--symbol-kind-names))
          (mapcar
           (lambda (obj)
             (let ((content
                    (cons (gethash "name" obj)
                          (lspce--lsp-position-to-point
                           (gethash "start"
                                    (if-let ((range (gethash "selectionRange" obj)))
                                        range
                                      (gethash "range" (gethash "location" obj)))))))
                   (container (gethash "containerName" obj)))
               (if container (list container content)
                 content)))
           (cdr obj))))

       (seq-group-by
        (lambda (obj) (gethash "kind" obj))
        (mapcan #'unfurl
                (lspce--request "textDocument/documentSymbol" (list :textDocument (lspce--textDocumentIdenfitier (lspce--uri)))))))))

  (evil-collection-define-key 'normal 'lspce-mode-map
    "gd" 'xref-find-definitions
    "gD" 'xref-find-definitions-other-window
    "g5" 'xref-find-definitions-other-frame
    "gR" 'xref-find-implementations
    (kbd "C-t") 'xref-pop-marker-stack
    "K" 'eldoc-doc-buffer)

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'lspce-mode-map
      "gr" 'xref-find-references))
  )

(provide 'init-lsp-bridge)

;;; lsp-bridge.el ends here
