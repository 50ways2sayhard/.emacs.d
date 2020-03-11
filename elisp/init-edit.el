;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description: Initialize Editing Configuration
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 28 13:25:24 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: 三 3月 11 18:48:41 2020 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d iedit
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes iedit, awesome-pair, delete-block
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

;; IEditPac
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)
;; -IEditPac


;; DeleteBlockPac
(use-package delete-block
  :load-path (lambda () (expand-file-name "site-elisp/delete-block" user-emacs-directory))
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))
;; -DeleteBlockPac

(use-package origami
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config (face-spec-reset-face 'origami-fold-header-face)
  )

(defun +scroll-to-center ()
  (evil-scroll-line-to-center (line-number-at-pos))
  )

(advice-add #'format-all-buffer :after #'+scroll-to-center)
(advice-add #'format-all-buffer--from-hook :after #'+scroll-to-center)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
