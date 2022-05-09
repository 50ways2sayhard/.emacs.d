;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 10:15:28 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr 23 18:24:15 2022 (+0800)
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

(defvar +self/first-input-hook nil)
(add-hook 'pre-command-hook #'(lambda ()
                                (when +self/first-input-hook
                                  (run-hooks '+self/first-input-hook)
                                  (setq +self/first-input-hook nil))))

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
(require 'init-tree-sitter)
(require 'init-which-key)
(require 'init-undo-tree)
(require 'init-bindings)
(require 'init-dired)

;; User Interface Enhancements
(require 'init-ui-config)
(require 'init-theme)
(require 'init-fonts)
;; (require 'init-dashboard)
(require 'init-scroll)
(require 'init-pretty-code)
(require 'init-highlight)

;; General Programming
(require 'init-magit)
(require 'init-project)
(require 'init-yasnippet)
;; (require 'init-flycheck)
(require 'init-flymake)
(require 'init-parens)
(require 'init-indent)
(require 'init-format)
(require 'init-edit)
(require 'init-lookup)
(if (eq my-lsp 'eglot)
    (require 'init-eglot)
  (require 'init-lsp))
(require 'init-lsp-bridge)
(require 'init-complete)
(require 'init-prog)
(require 'init-shell)

;; Programming
(require 'init-python)
(require 'init-javascript)
(require 'init-webdev)
(require 'init-ml)
;; (require 'init-dockerfile)
(require 'init-dart)


;; Miscellaneous
(require 'init-org)
(require 'init-restart-emacs)

(add-hook 'after-init-hook
          #'(lambda ()
              (+my/open-org-agenda)
              ))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
