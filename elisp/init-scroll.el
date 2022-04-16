;;; init-scroll.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-scroll.el
;; Description: Initialize Smooth Scroll
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 08:30:08 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr 16 13:45:51 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d smooth-scroll
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes smooth scroll
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

;; SmoothScroll
;; Vertical Scroll
(setq scroll-step 1
      ;; scroll-margin 0
      ;; scroll-conservatively 100000
      auto-window-vscroll t
      scroll-preserve-screen-position 'always)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-progressive-speed t)

(setq pixel-scroll-precision-mode t)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll


(provide 'init-scroll)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-scroll.el ends here
