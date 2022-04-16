;;; init-eglot.el ---
;;
;; Filename: init-eglot.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sat Apr 16 13:51:09 2022 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 6
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


(eval-when-compile
  (require 'init-const))

(use-package eglot
  :commands (+eglot-organize-imports)
  :hook (
         (eglot-managed-mode . (lambda ()
                                 (+lsp-optimization-mode)
                                 (leader-def :keymaps 'override
                                   "ca" '(eglot-code-actions :wk "Code Actions")
                                   "cr" '(eglot-rename :wk "Rename symbol")
                                   "cI" '(eglot-code-action-organize-imports :wk "Organize import")
                                   "ci" '(consult-imenu :wk "imenu")
                                   "cJ" '(consult-eglot-symbols :wk "Symbols in project")
                                   "cd" '(eglot-find-declaration :wk "Jump to definition")
                                   "cF" '(eglot-find-implementation :wk "Find implementation")
                                   "cD" '(eglot-find-typeDefinition :wk "Find type definition"))

                                 (evil-define-key 'normal 'global
                                   "K" 'eldoc-doc-buffer)
                                 ))
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lsp-mode 'makefile-mode)
                          (eglot-ensure))
                        )))
  :init
  (require 'lsp/+optimization)
  :config
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
  (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
  )


(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
