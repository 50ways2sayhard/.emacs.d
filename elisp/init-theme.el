;;; init-theme.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-theme.el
;; Description: Initialize Doom Themes and Modeline
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:11:56 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr 23 18:20:00 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d doom-themes doom-modeline
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes doom-themes and doom-modeline
;; This is NOT Doom, but doom-themes and doom-modeine
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

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-height 15)
  (doom-modeline-buffer-modification-icon t)
  )


;; DoomThemes
(use-package doom-themes
  :disabled
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-one t)
  )
;; -DoomThemes

(use-package ef-themes
  :straight (:repo "protesilaos/ef-themes" :host github)
  :config
  (ef-themes-select 'ef-trio-light)
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          `(("TODO" . (:foreground ,(ef-themes-with-colors red-cooler) :weight bold))
            ("INPROCESS"  . ,(ef-themes-with-colors yellow-cooler))
            ("PROJ"  . ,(ef-themes-with-colors cyan-cooler))
            ("WAITING" . ,(ef-themes-with-colors green-faint))
            ("DONE" . (:foreground ,(ef-themes-with-colors fg-alt) :strike-through t))
            ("CANCELED" . (:foreground ,(ef-themes-with-colors fg-dim) :weight bold :strike-through t))))
    ))

(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
