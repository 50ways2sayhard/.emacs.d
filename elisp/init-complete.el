;;; init-corfu.el --- ;; -*- lexical-binding: t -*-
;;
;; Filename: init-corfu.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sat Nov 27 21:36:42 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Thu Apr 21 10:57:16 2022 (+0800)
;;           By: John
;;     Update #: 646
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
        ("C-d" . corfu-info-documentation)
        ("M-." . corfu-info-location)
        ("C-i" . nil)
        ([?\r] . nil)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (evil-collection-define-key 'insert 'corfu-map
    (kbd "C-j") 'corfu-insert)
  :config
  (use-package corfu-quick
    :straight nil
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)))

  (use-package corfu-history
    :straight nil
    :config
    (corfu-history-mode))
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

  (with-eval-after-load 'all-the-icons
    (defvar kind-all-the-icons--cache nil
      "The cache of styled and padded label (text or icon).
An alist.")

    (defun kind-all-the-icons-reset-cache ()
      "Remove all cached icons from `kind-all-the-icons-mapping'."
      (interactive)
      (setq kind-all-the-icons--cache nil))

    (defun kind-all-the-icons--set-default-clear-cache (&rest args)
      (kind-all-the-icons-reset-cache)
      (apply #'set-default args))

    (defvar kind-all-the-icons--icons
      `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
        (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
        (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
        (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
        (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
        (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
        (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
        (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


    (defsubst kind-all-the-icons--metadata-get (metadata type-name)
      (or
       (plist-get completion-extra-properties (intern (format ":%s" type-name)))
       (cdr (assq (intern type-name) metadata))))

    (defun kind-all-the-icons-formatted (kind)
      "Format icon kind with all-the-icons"
      (or (alist-get kind kind-all-the-icons--cache)
          (let ((map (assq kind kind-all-the-icons--icons)))
            (let*  ((icon (if map
                              (cdr map)
                            (cdr (assq t kind-all-the-icons--icons))))
                    (half (/ (default-font-width) 2))
                    (pad (propertize " " 'display `(space :width (,half))))
                    (disp (concat pad icon pad)))
              (setf (alist-get kind kind-all-the-icons--cache) disp)
              disp))))

    (defun kind-all-the-icons-margin-formatter (metadata)
      "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
      (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
          (lambda (cand)
	          (if-let ((kind (funcall kind-func cand)))
	              (kind-all-the-icons-formatted kind)
	            (kind-all-the-icons-formatted t))))) ;; as a backup
    (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
    )
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
         ((lsp-completion-mode eglot-managed-mode) . my/set-lsp-capf)
         (lsp-bridge-mode . my/set-lsp-bridge-capf)
         )
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (setq cape-dict-file "/usr/share/dict/words")

  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     (cape-capf-buster
      (cape-super-capf
       arg-capf
       #'tabnine-completion-at-point
       #'tempel-expand)
      )
     ;; #'cape-dabbrev
     ))
  (defun my/set-basic-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf (car (last completion-at-point-functions 2)))))

  (defun my/set-lsp-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf (if (eq my-lsp 'eglot)
                                                                         #'eglot-completion-at-point
                                                                       #'lsp-completion-at-point))))
  (defun my/set-lsp-bridge-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf #'lsp-bridge-capf)))

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Add `completion-at-point-functions', used by `completion-at-point'.

  (use-package tempel
    :after cape)

  (use-package tabnine-capf
    :after cape
    :commands (tbanine-completion-at-point tabnine-capf-start-process)
    :straight (:host github :repo "theFool32/tabnine-capf" :files ("*.el" "*.sh" "*.py"))
    :hook ((+self/first-input . (lambda () (run-with-idle-timer 2 nil #'tabnine-capf-start-process)))
           (kill-emacs . tabnine-capf-kill-process))
    :config
    ;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
    )
  )

(use-package kind-icon
  :disabled
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
          (tmux . ,(all-the-icons-alltheicon "terminal-alt" :height 0.8 :v-adjust 0))
          (tabnine . ,(all-the-icons-material "cloud" :height 0.8))
          (t "." :icon "file-find" :face shadow))) ;
  )

(use-package corfu-doc
  :after corfu
  :straight (:host github :repo "galeo/corfu-doc")
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-d" . corfu-doc-toggle))
  )

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
    (or (not (eq corfu--candidates nil))
        tempel--active ;; diable copilot in tempel
        (not (looking-back "[\x00-\xff]"))))

  (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
  (customize-set-variable 'copilot-disable-predicates '(+my/corfu-candidates-p evil-ex-p minibufferp))

  ;;  HACK: workaround for node@16
  (cl-loop for node_path in '("/usr/local/opt/node@16/bin/node" "/opt/homebrew/opt/node@16/bin/node")
           when (file-exists-p node_path)
           return (setq copilot-node-executable node_path))

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
     "C-i" 'my/copilot-or-tempel-expand-or-next
     "M-i" 'copilot-accept-completion-by-word))
  )



(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
