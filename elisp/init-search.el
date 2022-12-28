;;; init-search.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-search.el
;; Description: Initialize Packages for Searching
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:01:43 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Apr 13 21:16:07 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d color-rg rg
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes ivy swiper counsel color-rg snails
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
  (require 'init-global-config)
  (require 'init-const)
  (require 'init-func))

;; ColorRGPac
(use-package color-rg
  :commands (color-rg-search-input color-rg-search-project color-rg-search-symbol-in-project)
  :straight (:host github :repo "manateelazycat/color-rg")
  :if *rg*
  :bind
  (:map color-rg-mode-map
        ("q" . my-quit-color-rg))
  :init
  (setq color-rg-mac-load-path-from-shell nil)
  :config
  (fset 'color-rg-project-root-dir #'my-project-root)
  (evil-make-overriding-map color-rg-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'color-rg-mode-hook #'evil-normalize-keymaps)
  (defun my-quit-color-rg ()
    (interactive)
    (kill-current-buffer)
    (evil-quit))
  )
;; -ColorRGPac

(use-package multi-translate
  :straight (:host github :repo "twlz0ne/multi-translate.el")
  :commands (multi-translate multi-translate-at-point multi-translate-yank-at-point)
  :custom
  (multi-translate-sentence-backends '(google))
  (multi-translate-word-backends '(bing youdao))
  :config
  (defun multi-translate-yank-at-point (arg)
    "Used temporarily for thesis"
    (interactive "P")
    (let* ((bounds (if (region-active-p)
                       (cons (region-beginning) (region-end))
                     (bounds-of-thing-at-point 'word)))
           (text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))
      (kill-new (multi-translate--google-translation "en" "zh-CN" text))
      (evil-normal-state)
      (message "Translate Done")))
  )

(use-package pinyinlib
  :after orderless
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  )

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
