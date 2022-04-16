;;; init-corfu.el ---
;;
;; Filename: init-corfu.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sat Nov 27 21:36:42 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sat Apr 16 20:08:20 2022 (+0800)
;;           By: John
;;     Update #: 643
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

(use-package corfu
  :straight (corfu :includes (corfu-indexed corfu-quick) :files (:defaults "extensions/corfu-*.el"))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-echo-documentation 0.5)
  (corfu-max-width 120)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-SPC" . corfu-insert-separator)
        ("S-TAB" . corfu-previous)
        ("C-j" . corfu-insert)
        ("C-i" . nil)
        ;; ([?\r] . nil)
        ([backtab] . corfu-previous))
  :init
  (corfu-global-mode)
  :config
  (use-package corfu-quick
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)))
  (add-to-list 'corfu-auto-commands 'awesome-pair-open-round)
  (add-to-list 'corfu-auto-commands 'awesome-pair-open-bracket)
  (add-to-list 'corfu-auto-commands 'awesome-pair-open-curly)

  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)

  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'completion))

(use-package cape
  :after corfu
  ;; Bind dedicated completion commands
  :bind (("C-x C-f" . cape-file)
         ("C-x C-k" . cape-keyword)
         ("C-x C-s" . cape-symbol)
         ("C-x C-l" . cape-line)
         ("C-x C-w" . cape-dict))
  :hook ((prog-mode . my/set-basic-capf)
         (org-mode . my/set-basic-capf)
         (lsp-completion-mode . my/set-lsp-capf)
         (eglot-managed-mode . my/set-eglot-capf))
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (setq cape-dict-file "/usr/share/dict/words")
  (fset 'cape-tabnine (cape-interactive-capf (cape-company-to-capf #'company-tabnine)))
  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     (cape-capf-buster
      (cape-super-capf
       arg-capf
       #'cape-tabnine
       #'tempel-expand)
      )
     ;; #'cape-dabbrev
     ))
  (defun my/set-basic-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf (car completion-at-point-functions))))

  (defun my/set-lsp-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf #'lsp-completion-at-point)))

  (defun my/set-eglot-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf #'eglot-completion-at-point)))

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Add `completion-at-point-functions', used by `completion-at-point'.

  (use-package tempel
    :after cape)
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-mapping
        '((array "a" :icon "code-brackets" :face font-lock-type-face)
          (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
          (class "c" :icon "video-input-component" :face font-lock-type-face) ;
          (color "#" :icon "palette" :face success) ;
          (constant "co" :icon "square-circle" :face font-lock-constant-face) ;
          (constructor "cn" :icon "cube-outline" :face font-lock-function-name-face) ;
          (enum-member "em" :icon "format-align-right" :face font-lock-builtin-face) ;
          (enum "e" :icon "server" :face font-lock-builtin-face) ;
          (event "ev" :icon "zip-box-outline" :face font-lock-warning-face) ;
          (field "fd" :icon "tag" :face font-lock-variable-name-face) ;
          (file "f" :icon "file-document-outline" :face font-lock-string-face) ;
          (folder "d" :icon "folder" :face font-lock-doc-face) ;
          (interface "if" :icon "share-variant" :face font-lock-type-face) ;
          (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face) ;
          (macro "mc" :icon "lambda" :face font-lock-keyword-face)
          (method "m" :icon "cube-outline" :face font-lock-function-name-face) ;
          (function "f" :icon "cube-outline" :face font-lock-function-name-face) ;
          (module "{" :icon "view-module" :face font-lock-preprocessor-face) ;
          (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
          (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face) ;
          (param "pa" :icon "tag" :face default)
          (property "pr" :icon "wrench" :face font-lock-variable-name-face) ;
          (reference "rf" :icon "collections-bookmark" :face font-lock-variable-name-face) ;
          (snippet "S" :icon "format-align-center" :face font-lock-string-face) ;
          (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
          (struct "%" :icon "video-input-component" :face font-lock-variable-name-face) ;
          (text "tx" :icon "format-text" :face shadow)
          (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
          (unit "u" :icon "ruler-square" :face shadow)
          (value "v" :icon "format-align-right" :face font-lock-builtin-face) ;
          (variable "va" :icon "tag" :face font-lock-variable-name-face)
          (t "." :icon "file-find" :face shadow))) ;
  )

(use-package corfu-doc
  :after corfu
  :straight (:host github :repo "galeo/corfu-doc")
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-d" . corfu-doc-toggle))
  )

(use-package company-tabnine
  :defer 1
  :after corfu
  :hook (kill-emacs . company-tabnine-kill-process)
  :custom
  (company-tabnine-max-num-results 3))

(use-package copilot
  :after corfu
  :hook (prog-mode . copilot-mode)
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t
  :bind
  (("C-c n" . copilot-next-completion)
   ("C-c p" . copilot-previous-completion))
  :config
  (set-face-foreground 'copilot-overlay-face "pink") ;; TODO: find a better color

  (defun +my/corfu-candidates-p ()
    (not (eq corfu--candidates nil)))

  (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
  (customize-set-variable 'copilot-disable-predicates '(+my/corfu-candidates-p))

  (defun my/copilot-or-tempel-expand-or-next ()
    "Try tempel expand, if failed, try copilot expand."
    (interactive)
    (if tempel--active
        (tempel-next 1)
      (if (tempel-expand) ;; HACK: call `tempel-expand' twice
          (call-interactively #'tempel-expand)
        (copilot-accept-completion)
        ))
    (copilot-clear-overlay))

  (with-eval-after-load 'general
    (general-define-key
     :keymaps '(evil-insert-state-map)
     "C-i" 'my/copilot-or-tempel-expand-or-next))
  )


(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
