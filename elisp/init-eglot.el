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
;;     Update #: 32
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

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package eglot
  :commands (+eglot-organize-imports +eglot-help-at-point)
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
                                   "cD" '(eglot-find-typeDefinition :wk "Find type definition")
                                   ;; "ck" '(+eglot-help-at-point :wk "Lookup documentation")
                                   )

                                 (setq completion-at-point-functions (remove #'eglot-completion-at-point completion-at-point-functions))

                                 ;; (my/set-lsp-bridge-capf)

                                 ;; (evil-define-key 'normal 'global
                                 ;;   "K" 'lsp-bridge-lookup-documentation)
                                 ))
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lsp-mode 'makefile-mode 'python-mode)
                          (eglot-ensure))
                        )))
  :config
  (setq
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-confirm-server-initiated-edits nil
   eglot-sync-connect nil
   eglot-events-buffer-size 0)
  (setq eldoc-echo-area-use-multiline-p 5)
  (setq elgot-stay-out-of '(flymake))
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
  (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))

  ;; HACK Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
  ;;      more problematic approach of deferred to eldoc. Here, I've restored it.
  ;;      Doom's lookup handlers try to open documentation in a separate window
  ;;      (so they can be copied or kept open), but doing so with an eldoc buffer
  ;;      is difficult because a) its contents are generated asynchronously,
  ;;      making them tough to scrape, and b) their contents change frequently
  ;;      (every time you move your cursor).
  (defvar +eglot--help-buffer nil)
  (defun +eglot-lookup-documentation (_identifier)
    "Request documentation for the thing at point."
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (let ((blurb (and (not (seq-empty-p contents))
                        (eglot--hover-info contents range)))
            (hint (thing-at-point 'symbol)))
        (if blurb
            (with-current-buffer
                (or (and (buffer-live-p +eglot--help-buffer)
                         +eglot--help-buffer)
                    (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
              (with-help-window (current-buffer)
                (rename-buffer (format "*eglot-help for %s*" hint))
                (with-current-buffer standard-output (insert blurb))
                (setq-local nobreak-char-display nil)))
          (display-local-help))))
    'deferred)

  (defun +eglot-help-at-point()
    (interactive)
    (+eglot-lookup-documentation nil))
  )


(provide 'init-eglot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eglot.el ends here
