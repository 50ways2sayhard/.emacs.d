;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 10:15:28 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Jun  1 18:23:31 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
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

(load (concat user-emacs-directory "early-init") nil t)

;; Speed up startup
(setq auto-mode-case-fold nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))
;; -LoadPath

;; InitPrivate
;; Load init-custom.el if it exists
(when (file-exists-p (expand-file-name "init-custom.el" user-emacs-directory))
  (load-file (expand-file-name "init-custom.el" user-emacs-directory)))
;; -InitPrivate

;; Constants
(require 'init-const)

;; Package Management
(require 'init-package)

;; Global Functionalities
(require 'init-global-config)
(require 'init-func)
(require 'init-mini-buffer)
(require 'init-evil)
(require 'init-search)
(when (featurep 'native-compile) ;;FIXME: tree-sitter not work in M1 now.
  (require 'init-tree-sitter))
(require 'init-winner)
(require 'init-which-key)
(require 'init-popup-kill-ring)
(require 'init-undo-tree)
(require 'init-discover-my-major)
(require 'init-dired)
(require 'init-buffer)
(require 'init-bindings)

;; User Interface Enhancements
(require 'init-ui-config)
(require 'init-theme)
(require 'init-dashboard)
(require 'init-fonts)
(require 'init-scroll)
(require 'init-hydra)
(require 'init-pretty-code)
(require 'init-highlight)

;; General Programming
(require 'init-magit)
(require 'init-projectile)
(require 'init-treemacs)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-parens)
(require 'init-indent)
(require 'init-format)
(require 'init-edit)
(require 'init-header)
(require 'init-lsp)
(require 'init-company)
(require 'init-prog)

;; Programming
(require 'init-cc)
(require 'init-python)
(require 'init-latex)
(require 'init-javascript)
(require 'init-webdev)
(require 'init-direnv)
(require 'init-ml)
(require 'init-dockerfile)
(require 'init-dart)


;; Miscellaneous
(require 'init-org)
(require 'init-restart-emacs)
(require 'init-shackle)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
