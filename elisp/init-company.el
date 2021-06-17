;;; init-company.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-company.el
;; Description: Initialize Company
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:02:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Jun 17 21:53:31 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d company company-tabnine
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes company
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

;;;###autoload
(defvar +company-backend-alist
  '((text-mode company-tabnine company-yasnippet company-dabbrev)
    (prog-mode company-files company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.
MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.
If the car of BACKENDS is nil, unset the backends for MODES.
Examples:
  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))
  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))
  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))


;;
;;; Library

(defun +company--backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))


;;
;;; Hooks

;;;###autoload
(defun +company-init-backends-h ()
  "Set `company-backends' for the current buffer."
  (if (not company-mode)
      (remove-hook 'change-major-mode-after-body-hook #'+company-init-backends-h 'local)
    (unless (eq major-mode 'fundamental-mode)
      (setq-local company-backends (+company--backends)))
    (add-hook 'change-major-mode-after-body-hook #'+company-init-backends-h nil 'local)))

(put '+company-init-backends-h 'permanent-local-hook t)


;; ComPac
(use-package company
  :diminish "C"
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :hook (after-init . global-company-mode)
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("TAB" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         ("<C-RET>" . company-complete)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-show-numbers t
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-backends '(company-capf company-files)
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
        )
  (add-hook 'company-mode-hook #'+company-init-backends-h)
  (unless *clangd* (delete 'company-clang company-backends))
  (global-company-mode 1)
  (company-tng-mode 1)
  (defun smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common)))

  ;; do not use orderless when completing
  (with-eval-after-load 'orderless
    (defvar-local +company-completion-styles '(partial-completion))
    (defvar-local +completion-styles nil)
    (defun set-company-completion-style (backend)
      (setq +completion-styles completion-styles)
      (setq completion-styles +company-completion-styles))
    (defun restore-company-completion-style ()
      (when +completion-styles
        (setq completion-styles +completion-styles)
        (setq +completion-styles nil)))

    (add-hook 'company-completion-started-hook #'set-company-completion-style)
    ;; (add-hook 'company-completion-cancelled-hook #'restore-company-completion-style)
    ;; (add-hook 'company-completion-finished-hook #'restore-company-completion-style)
    (add-hook 'evil-normal-state-entry-hook #'restore-company-completion-style))
  )
;; -ComPac

(use-package company-prescient
  :init (company-prescient-mode 1))

;; CompanyTabNinePac
;; TODO: whether or not to use tabnine for lsp or even prog-mode.
(use-package company-tabnine
  :straight (:host github :repo "theFool32/company-tabnine" :depth 1)
  :defer 1
  :after company
  :custom
  (company-tabnine-max-num-results 3)
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :config
  ;; Enable TabNine on default
  ;; (add-to-list 'company-backends #'company-tabnine)

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-lsp 2)
               (seq-take candidates-tabnine 2)
               (seq-drop candidates-lsp 2)
               (seq-drop candidates-tabnine 2)
               )))))
;; -Companytabninepac

(use-package company-quickhelp
  :defines company-quickhelp-delay
  :if (display-graphic-p)
  :bind (:map company-active-map
              ([remap company-show-doc-buffer] . company-quickhelp-manual-begin)
              ("C-c d" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.5))



(provide 'init-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
