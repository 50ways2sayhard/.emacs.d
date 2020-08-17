;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:42:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: 二 8月 11 09:55:06 2020 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d lsp
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes lsp-mode and dap-mode
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


(eval-when-compile
  (require 'init-const))


;; ;; Ivy integration
(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))

(use-package lsp-mode
  :diminish
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)

                       ;; Format and organize imports
                       (unless (derived-mode-p 'c-mode 'c++-mode 'python-mode)
                         (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (add-hook 'rjsx-mode #'lsp-typescript-enable)

  (setq lsp-auto-guess-root t        ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-enable-indentation nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-enable-semantic-highlighting nil
        ;; lsp-prefer-capf t
        lsp-keep-workspace-alive nil
        lsp-idle-delay 0.5
        lsp-enable-on-type-formatting nil
        lsp-diagnostic-package :none
        lsp-flycheck-live-reporting nil
        lsp-enable-snippet nil
        lsp-enable-text-document-color nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-log-io nil
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers nil
        lsp-keymap-prefix "C-c l"
        lsp-eldoc-render-all nil
        )
  (setq gc-cons-threshold 100000000)

  :config
  (use-package lsp-pyright
    :after lsp-mode python
    :config
    (setq lsp-pyright-venv-path ".venv")
    (setq lsp-pyright-multi-root nil)
    )
  ;; (setq lsp-pyright-use-library-code-for-types t)
  ;; (setq lsp-pyright-venv ".venv")
  ;; (setq lsp-pyright-disable-organize-imports t)
  ;; (setq lsp-pylance-ms-executable "~/.local/pylance.sh")
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection (lambda () lsp-pylance-ms-executable)
  ;;                                         (lambda () (f-exists? lsp-pylance-ms-executable)))
  ;;   :major-modes '(python-mode)
  ;;   :server-id 'mspylance
  ;;   :priority 3
  ;;   :initialized-fn (lambda (workspace)
  ;;                     (with-lsp-workspace workspace
  ;;                       (lsp--set-configuration (lsp-configuration-section "python"))))
  ;;   ))
  )

;; LSPUI
(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (("C-c d" . lsp-ui-doc-show)
         ("C-c D" . lsp-ui-doc-hide)
         :map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c u" . lsp-ui-imenu)
         ("M-i" . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :preface
  (defun my/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1))
    )
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip)))))
;; -LSPUI

;; DAPPac
(use-package dap-mode
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle)))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (python-mode . (lambda () (require 'dap-python)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
         )
  )
;; -DAPPac

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
