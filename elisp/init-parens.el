;;; init-parens.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-parens.el
;; Description: Initialize Parenthesis
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:17:13 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun May 23 14:53:35 2021 (+0800)
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
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(provide 'init-parens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-parens.el ends here
