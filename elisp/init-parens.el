;;; init-parens.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-parens.el
;; Description: Initialize Parenthesis
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:17:13 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr 16 19:53:23 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d parenthesis smartparens delete-block
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes parenthesis smartparens delete-block
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
  (require 'init-global-config))

(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            '(lambda ()
               (setq-local electric-pair-inhibit-predicate
                           `(lambda (c)
                              (if (char-equal c ?<) t
                                (,electric-pair-inhibit-predicate c))))))
  (add-to-list 'electric-pair-pairs '(?` . ?`)))

(use-package puni
  :defer t
  :hook ((prog-mode markdown-mode org-mode) . puni-mode)
  :init
  (general-def
    :keymaps 'puni-mode-map
    "DEL" 'puni-backward-delete-char
    "C-d" 'puni-forward-delete-char
    "C-k" 'puni-kill-line
    "M-h" 'puni-force-delete
    "C-u" 'puni-backward-kill-line
    "C-M-f" 'puni-forward-sexp
    "C-M-b" 'puni-backward-sexp
    "C-M-a" 'puni-beginning-of-sexp
    "C-M-e" 'puni-end-of-sexp
    "s-<backspace>" 'puni-force-delete)
  :config
  (setq puni-confirm-when-delete-unbalanced-active-region nil))

;; Show matching parens
(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t))

(provide 'init-parens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-parens.el ends here
