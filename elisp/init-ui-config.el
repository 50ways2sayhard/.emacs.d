;;; init-ui-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui-config.el
;; Description: Initialize UI Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 16:12:56 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Jun 19 16:49:02 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d ui
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes prettify-symbols-mode and other UI configurations
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
  (require 'init-const)
  (require 'init-func))


(mouse-avoidance-mode 'exile)

;; PreSym
(global-prettify-symbols-mode 1)
;; -PreSym

;; TitleBar
(setq-default frame-title-format '("EMACS" " - %b"))
;; -TitleBar

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...\n")
;; -StartupScreen

;; DisLineNum
;; Hook line numbers to only when files are opened, also use linum-mode for emacs-version< 26
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Display column numbers in modeline
(column-number-mode 1)
(setq display-line-numbers-type 'relative)
;; -DisLineNum

(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))
(setq blink-cursor-mode nil)
(setq word-wrap t
      word-wrap-by-category t
      require-final-newline t)

(add-hook 'prog-mode-hook #'(lambda () (visual-line-mode)))
(add-hook 'text-mode-hook #'(lambda () (visual-line-mode)))

(setq split-width-threshold 0
      split-height-threshold nil)

;; Optimization
(setq idle-update-delay 1.0)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
