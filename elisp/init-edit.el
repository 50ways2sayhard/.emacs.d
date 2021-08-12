;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description: Initialize Editing Configuration
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 28 13:25:24 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Aug 12 20:32:53 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d iedit
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes iedit, delete-block
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

;; DeleteBlockPac
(use-package delete-block
  :defer
  :straight (:host github :repo "manateelazycat/delete-block" :depth 1)
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

(use-package rime
  :defer t
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-after-ascii-char-p
     rime-predicate-space-after-cc-p))
  :config
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)
  (cond (*sys/mac* (setq rime-user-data-dir "~/.config/rime"
                         rime-librime-root "~/.local/share/librime/dist/"))
        (*sys/linux* (setq rime-user-data-dir "~/.rime")))
  (defun +rime-sync ()
    ;; HACK: force emacs-rime to use userdb.
    ;; I am not sure if it is safe as the deploy may delete the old userdb.
    (interactive)
    (when rime--lib-loaded
      (let ((lock-name (concat rime-user-data-dir "/luna_pinyin.userdb/LOCK")))
        (when (file-exists-p lock-name)
          (delete-file lock-name)
          (rime-deploy)))))
  (defun activate-default-input-method ()
    (interactive)
    (activate-input-method default-input-method))
  (add-hook 'text-mode-hook 'activate-default-input-method))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(defun nuke_trailing()
  (add-hook 'write-file-hooks 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'nuke_trailing)
(add-hook 'text-mode-hook 'nuke_trailing)

(use-package expand-region)

(use-package pinyinlib)


(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
