;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description: Initialize Editing Configuration
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 28 13:25:24 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Apr 12 14:29:24 2022 (+0800)
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

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :defer t
  :diminish
  :hook (((web-mode js-mode python-mode) . aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode 'dart-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

(use-package origami
  :defer t
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

(defun nuke_trailing()
  (add-hook 'write-file-hooks 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'nuke_trailing)
(add-hook 'text-mode-hook 'nuke_trailing)

(use-package expand-region
  :defer t)

(use-package sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For MacOS
  (when *sys/mac*
    (setq sis-english-source "com.apple.keylayout.ABC")
    (setq sis-other-source "im.rime.inputmethod.Squirrel.Rime"))
  (when *sys/wsl*
    (setq sis-english-source "1033")
    (setq sis-other-source "2052")
    (setq sis-do-get (lambda ()
                       (sis--ensure-dir
                        (string-trim (shell-command-to-string "im-select.exe")))))
    (setq sis-do-set (lambda(source)
                       (sis--ensure-dir
                        (call-process "/bin/bash" nil t nil "-c" (concat "im-select.exe " source)))))
    (setq sis-external-ism "im-select.exe"))
  (add-hook 'focus-out-hook #'sis-set-other)
  (add-hook 'focus-in-hook #'sis-set-english)
  )



(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
