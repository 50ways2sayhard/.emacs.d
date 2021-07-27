;;; init-format.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-format.el
;; Description: Initialize Formatter
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:27:40 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Jul 27 21:41:17 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d format-all
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes format-all
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

;; FormatAllPac
(use-package format-all
  :straight (:host github :repo "50ways2sayhard/emacs-format-all-the-code")
  :hook ((prog-mode) . (lambda ()
                         (unless (derived-mode-p 'web-mode 'dart-mode 'js-mode)
                           (format-all-ensure-formatter)
                           (format-all-mode)
                           )))
  :config
  (add-hook 'format-all-after-format-functions (lambda (a b) (call-interactively 'recenter)))
  (setq format-all-formatters '(("Vue" (prettier "--parser vue"))))
  )
;; -FormatAllPac

(provide 'init-format)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-format.el ends here
