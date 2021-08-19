;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:42:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Aug 18 20:06:32 2021 (+0800)
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
  (require 'init-const)
  (require 'lsp/+optimization))


;;;###autoload
(defun +lsp-clear-cache ()
  (lsp-completion--clear-cache))


;;;###autoload
(defun my-lsp-setup ()
  ;; Integrate `which-key'
  (lsp-enable-which-key-integration)
  (+lsp-optimization-mode +1)

  ;; Format and organize imports
  ;; (unless (derived-mode-p 'c-mode 'c++-mode 'python-mode 'web-mode 'js-mode)
  ;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (if (derived-mode-p 'dart-mode)
      (add-hook 'before-save-hook #'lsp-format-buffer)))

(use-package lsp-mode
  :diminish
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         ;; (lsp-mode . my-lsp-setup)
         )
  :init
  (require 'lsp/+optimization)
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (add-hook 'lsp-mode-hook #'my-lsp-setup)

  (with-no-warnings
    (setq lsp-auto-guess-root nil        ; Detect project root
          lsp-keep-workspace-alive nil ; Auto-kill LSP server
          lsp-restart 'auto-restart
          lsp-enable-indentation nil
          lsp-semantic-tokens-enable nil
          ;; lsp-diagnostics-provider :flycheck
          lsp-diagnostics-provider :none
          lsp-signature-auto-activate t
          lsp-idle-delay 0.5
          lsp-enable-imenu nil
          lsp-enable-on-type-formatting nil
          lsp-enable-text-document-color nil
          lsp-enable-symbol-highlighting nil
          lsp-log-io nil
          lsp-enable-folding nil
          lsp-enable-file-watchers nil
          lsp-keymap-prefix nil
          lsp-eldoc-enable-hover t
          lsp-eldoc-render-all nil
          lsp-session-file (concat user-emacs-directory ".local/cache/lsp-session")
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-modeline-workspace-status-enable nil
          lsp-headerline-breadcrumb-enable nil
          lsp-enable-links nil
          lsp-completion-show-detail nil
          lsp-completion-provider :none)
    (setq lsp-typescript-implementations-code-lens-enabled t
          lsp-typescript-references-code-lens-enabled t
          lsp-typescript-suggest-complete-function-calls t

          lsp-eslint-auto-fix-on-save t
          lsp-eslint-library-choices-file (concat user-emacs-directory ".local/cache/lsp-eslint-choices")

          lsp-vetur-format-enable nil
          lsp-vetur-validation-style nil
          lsp-vetur-validation-script nil
          lsp-vetur-validation-template nil))

  )

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
