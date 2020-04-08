;;; init-theme.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-theme.el
;; Description: Initialize Doom Themes and Modeline
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:11:56 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: 三 4月  8 09:29:41 2020 (+0800)
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
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-height 15)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-env-python-executable "python")
  :config
  (doom-modeline-mode))


;; DoomThemes
(use-package doom-themes
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :config
  ;; flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (load-theme 'doom-one t))
;; -DoomThemes

(use-package hide-mode-line
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; (use-package nord-theme
;;   :quelpa (nord-theme :fetcher github :repo "arcticicestudio/nord-emacs")
;;   :after (doom-modeline diff-hl)
;;   :config
;;   (set-face-background 'mode-line "#242832")
;;   (set-face-foreground 'diff-hl-change "#EBCB8B")
;;   (set-face-foreground 'diff-hl-insert "#A3BE8C")
;;   (set-face-foreground 'diff-hl-delete "#BF616A")
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (font-lock-add-keywords
;;                nil
;;                '(("^[[:space:]]*\\(@[^(#[:space:]\n]*\\)" 1 'font-lock-preprocessor-face)))))
;;   (set-face-attribute 'font-lock-preprocessor-face nil
;;                       :weight 'normal
;;                       :foreground "#D08770")
;;   )


(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
