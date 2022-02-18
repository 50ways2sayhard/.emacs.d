;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:42:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Feb 16 14:20:23 2022 (+0800)
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
(defun my-lsp-setup ()
  ;; Integrate `which-key'
  (lsp-enable-which-key-integration)
  (+lsp-optimization-mode +1)

  ;; Format and organize imports
  (if (derived-mode-p 'dart-mode)
      (add-hook 'before-save-hook #'lsp-format-buffer t t))
  (if (derived-mode-p 'dart-mode)
      (add-hook 'before-save-hook #'lsp-organize-imports t t))
  )

(pcase my-lsp
  ('eglot
   (use-package eglot
     ;; :disabled
     :commands eglot eglot-ensure
     :hook ((eglot-managed-mode . +lsp-optimization-mode)
            (prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lsp-mode 'makefile-mode)
                             (eglot-ensure)))))
     :config
     (setq eglot-sync-connect 1
           eglot-connect-timeout 10
           eglot-autoshutdown t
           eglot-send-changes-idle-time 0.5
           ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
           ;;      its popup rule causes eglot to steal focus too often.
           eglot-auto-display-help-buffer nil)
     ;; (setq eglot-stay-out-of '(flymake company project))
     (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))

     (setq eldoc-echo-area-use-multiline-p nil)

     (add-to-list 'eglot-server-programs '(web-mode . ("vls" "--stdio")))
     (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
     (add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server" "--client-id" "emacs.eglot" "--client-version" "1.2")))

     (leader-def :keymaps 'override
       "ca" '(eglot-code-actions)
       "cr" '(eglot-rename)
       "cI" '(eglot-code-action-organize-imports)
       "ci" '(consult-imenu)
       "cJ" '(consult-eglot-symbols)
       "cd" '(eglot-find-declaration)
       "cF" '(eglot-find-implementation)
       "cD" '(eglot-find-typeDefinition))

     ;; From Eason0210/emacs.d
     ;; Hacky eglot support in flycheck
     ;; This file sets up flycheck so that, when eglot receives a publishDiagnostics method
     ;; from the server, flycheck updates the reports.
     ;;
     ;; It works by creating a bridge function which can be used as the argument of
     ;; `eglot-flymake-backend', which both consumes diagnostics and queue a call to
     ;; 'flycheck-buffer'

     (defvar-local +lsp--flycheck-eglot--current-errors nil)

     (defun +lsp--flycheck-eglot-init (checker callback)
       "CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
       (eglot-flymake-backend #'+lsp--flycheck-eglot--on-diagnostics)
       (funcall callback 'finished +lsp--flycheck-eglot--current-errors))

     (defun +lsp--flycheck-eglot--on-diagnostics (diags &rest _)
       (cl-labels
           ((flymake-diag->flycheck-err
             (diag)
             (with-current-buffer (flymake--diag-buffer diag)
               (flycheck-error-new-at-pos
                (flymake--diag-beg diag)
                (pcase (flymake--diag-type diag)
                  ('eglot-note 'info)
                  ('eglot-warning 'warning)
                  ('eglot-error 'error)
                  (_ (error "Unknown diagnostic type, %S" diag)))
                (flymake--diag-text diag)
                :end-pos (flymake--diag-end diag)
                :checker 'eglot
                :buffer (current-buffer)
                :filename (buffer-file-name)))))
         (setq +lsp--flycheck-eglot--current-errors
               (mapcar #'flymake-diag->flycheck-err diags))
         ;; Call Flycheck to update the diagnostics annotations
         (flycheck-buffer-deferred)))

     (defun +lsp--flycheck-eglot-available-p ()
       (bound-and-true-p eglot--managed-mode))

     (with-eval-after-load 'flycheck
       (flycheck-define-generic-checker 'eglot
         "Report `eglot' diagnostics using `flycheck'."
         :start #'+lsp--flycheck-eglot-init
         :predicate #'+lsp--flycheck-eglot-available-p
         :modes '(prog-mode text-mode))

       (push 'eglot flycheck-checkers)
       )


     (defun +lsp-eglot-prefer-flycheck-h ()
       (when eglot--managed-mode
         (flymake-mode -1)
         (when-let ((current-checker (flycheck-get-checker-for-buffer)))
           (when (memq current-checker (list 'c/c++-clang 'rust-cargo 'python-pycompile))
             (flycheck-disable-checker current-checker))
           (unless (equal current-checker 'eglot)
             (flycheck-add-next-checker 'eglot current-checker)))
         (flycheck-add-mode 'eglot major-mode)
         (flycheck-mode 1)
         ;; Call flycheck on initilization to make sure to display initial
         ;; errors
         (flycheck-buffer-deferred)))

     (add-hook 'eglot-managed-mode-hook #'+lsp-eglot-prefer-flycheck-h)

     (with-eval-after-load 'flymake
       (when (and
              (not (fboundp 'flymake--diag-buffer))
              (fboundp 'flymake--diag-locus))
         (defalias 'flymake--diag-buffer 'flymake--diag-locus)))
     )
   )
  ('lsp-mode
   (use-package lsp-mode
     :diminish
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                             (lsp-deferred))))
            (lsp-completion-mode . my/lsp-mode-setup-completion)
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
             lsp-signature-auto-activate nil
             ;; lsp-signature-function 'lsp-signature-posframe
             lsp-idle-delay 0.5
             lsp-lens-enable nil
             lsp-enable-imenu nil
             lsp-enable-on-type-formatting nil
             lsp-enable-text-documet-color nil
             lsp-enable-symbol-highlighting nil
             lsp-log-io nil
             lsp-enable-folding nil
             lsp-enable-file-watchers nil
             lsp-keymap-prefix nil
             lsp-eldoc-enable-hover nil
             lsp-eldoc-render-all nil
             lsp-session-file (concat user-emacs-directory ".local/cache/lsp-session")
             lsp-modeline-code-actions-enable nil
             lsp-modeline-diagnostics-enable nil
             lsp-modeline-workspace-status-enable nil
             lsp-headerline-breadcrumb-enable nil
             lsp-enable-links nil
             lsp-completion-show-detail nil
             lsp-completion-sort-initial-results t ; check if should keep as t
             lsp-completion-no-cache t
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
     (setq lsp-signature-posframe-params
           (list :poshandler #'posframe-poshandler-window-top-right-corner
                 :height 10
                 :width 80
                 :border-width 1
                 :min-width 60))
     :config
     (defun my-lsp--init-if-visible (fn &rest args)
       (unless (bound-and-true-p git-timemachine-mode)
         (apply fn args)))
     (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)
     (defun my-lsp-icons-all-the-icons-material-icon (icon-name face fallback &optional feature)
       (if (and (display-graphic-p)
                (functionp 'all-the-icons-material)
                (lsp-icons--enabled-for-feature feature))
           (all-the-icons-material icon-name
                                   :face face)
         (propertize fallback 'face face)))
     (advice-add #'lsp-icons-all-the-icons-material-icon
                 :override #'my-lsp-icons-all-the-icons-material-icon)
     (defun my/lsp-mode-setup-completion ()
       (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
             '(partial-completion))) ;; Configure flex
     )
   (use-package lsp-ui
     :after lsp-mode
     :custom-face
     (lsp-ui-doc-background ((t (:background nil))))
     (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
     :hook (lsp-mode . lsp-ui-mode)
     :bind
     (:map lsp-ui-doc-frame-mode-map
           ("C-g" . lsp-ui-doc-unfocus-frame))
     :custom
     (lsp-ui-doc-header nil)
     (lsp-ui-doc-include-signature t)
     (lsp-ui-doc-enable nil)
     (lsp-ui-doc-delay 1)
     (lsp-ui-doc-border (face-foreground 'default))
     (lsp-ui-sideline-enable nil)
     (lsp-ui-sideline-ignore-duplicate t)
     (lsp-ui-sideline-show-code-actions nil)
     (lsp-ui-sideline-show-diagnostics nil)
     (lsp-ui-doc-position 'at-point)
     :config
     (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
     (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
       (setq mode-line-format nil)))

   (use-package dap-mode
     :disabled
     :defines dap-python-executable
     :functions dap-hydra/nil
     :diminish
     :bind (:map lsp-mode-map
                 ("<f5>" . dap-debug)
                 ("M-<f5>" . dap-hydra))
     :hook ((lsp-mode . dap-auto-configure-mode)
            (dap-stopped . (lambda (_args) (dap-hydra)))

            (python-mode . (lambda () (require 'dap-python)))
            ((js-mode js2-mode) . (lambda () (require 'dap-chrome))))
     :init
     (when (executable-find "python3")
       (setq dap-python-executable "python3")))

   )
  )


(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
