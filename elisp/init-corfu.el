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
;; Last-Updated: Fri Dec 24 10:15:43 2021 (+0800)
;;           By: John
;;     Update #: 159
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
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  (corfu-echo-documentation 0.3)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

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
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (corfu-global-mode)
  :config
  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)
  (defun my/corfu-commit-predicate ()
    ;;     "Auto-commit candidates if:
    ;; 1. A "." is typed, except after a SPACE.
    ;; 2. A selection was made, aside from entering SPACE.
    ;; 3. Just one candidate exists, and we continue to non-symbol info.
    ;; 4. The 1st match is exact."
    (cond
     ((seq-contains-p (this-command-keys-vector) ?.)
      (or (string-empty-p (car corfu--input))
	      (not (string= (substring (car corfu--input) -1) " "))))

     ((/= corfu--index corfu--preselect) ; a selection was made
      (not (seq-contains-p (this-command-keys-vector) ? )))

     ((eq corfu--total 1) ;just one candidate
      (seq-intersection (this-command-keys-vector) [?: ?, ?\) ?\] ?\( ? ]))

     ((and corfu--input ; exact 1st match
	       (string-equal (substring (car corfu--input) corfu--base)
			             (car corfu--candidates)))
      (seq-intersection (this-command-keys-vector) [?: ?. ?, ?\) ?\] ?\" ?' ? ]))))
  ;; (setq corfu-commit-predicate #'my/corfu-commit-predicate)
  )

(use-package cape
  :after corfu
  ;; Bind dedicated completion commands
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p i" . cape-ispell)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              ;; (setq-local completion-at-point-functions (list (cape-super-capf #'lsp-completion-at-point #'cape-dabbrev)))
              (add-to-list 'completion-at-point-functions #'cape-file)
              ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
              ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
              ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
              ;; ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
              ;; ;;(add-to-list 'completion-at-point-functions #'cape-dict)
              ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
              ;; ;;(add-to-list 'completion-at-point-functions #'cape-line)
              ))
  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions (mapcar #'cape-company-to-capf (list #'company-tabnine)))))
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package company-tabnine
  :straight (:host github :repo "theFool32/company-tabnine" :depth 1)
  :custom
  (company-tabnine-max-num-results 3)
  :hook
  (kill-emacs . company-tabnine-kill-process)
  )

(use-package corfu-doc
  :after corfu
  :straight (:host github :repo "galeo/corfu-doc")
  :hook (corfu-mode . corfu-doc-mode)
  )

(provide 'init-corfu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-corfu.el ends here
