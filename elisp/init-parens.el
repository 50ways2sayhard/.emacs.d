;;; init-parens.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-parens.el
;; Description: Initialize Parenthesis
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:17:13 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Jul 24 11:28:26 2021 (+0800)
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
  (add-to-list 'electric-pair-pairs '(?< . ?>))
  (add-to-list 'electric-pair-pairs '(?` . ?`)))

(use-package awesome-pair
  :straight (:host github :repo "manateelazycat/awesome-pair")
  :hook (prog-mode . awesome-pair-mode)
  :config
  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
  )

(provide 'init-parens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-parens.el ends here
