;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:42:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun May 16 11:56:44 2021 (+0800)
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
                       (unless (derived-mode-p 'c-mode 'c++-mode 'python-mode 'web-mode)
                         (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (add-hook 'rjsx-mode #'lsp-typescript-enable)

  (setq lsp-auto-guess-root nil        ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-enable-indentation nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-semantic-tokens-enable nil
        lsp-keep-workspace-alive nil
        lsp-idle-delay 0.5
        lsp-enable-on-type-formatting nil
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
        lsp-session-file (concat user-emacs-directory ".local/cache/lsp-session")
        )
  (setq gc-cons-threshold 100000000)
  )

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
           :color amaranth :quit-key "q")
   ("Doc"
    (("d e" (progn
              (lsp-ui-doc-enable (not lsp-ui-doc-mode))
              (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("C-M-l" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face)))
  :config
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (add-hook 'after-load-theme-hook (lambda ()
                                     (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
                                     (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))
  )

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
