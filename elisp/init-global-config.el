;;; init-global-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-global-config.el
;; Description: Initialize Global Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 14:01:54 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Apr 13 19:27:12 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes global configurations
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

;; SudoEditPac
(use-package sudo-edit
  :commands (sudo-edit))
;; -SudoEditPac

;; DefBindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
;; -DefBindings

;; UTF8Coding
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -UTF8Coding

;; EditExp
;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

;; Replace selection on insert
(delete-selection-mode 1)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
;; -EditExp

;; History
(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'"))
  :config
  (defun recentd-track-opened-file ()
    "Insert the name of the directory just opened into the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-add-file default-directory))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (defun recentd-track-closed-file ()
    "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-remove-if-non-kept default-directory)))

  (add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
  (add-hook 'kill-buffer-hook 'recentd-track-closed-file)
  )

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
;; -History

;; SmallConfigs
;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -SmallConfigs

(unless *sys/mac*
  (setq command-line-ns-option-alist nil))
(unless *sys/linux*
  (setq command-line-x-option-alist nil))
;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(use-package gcmh
  :hook (+self/first-input . gcmh-mode)
  :diminish
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 64 1024 1024)))

(when *sys/wsl*
  (defun my/browse-url-generic (url &optional _new-window)
    ;; new-window ignored
    "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
    (interactive (browse-url-interactive-arg "URL: "))
    (if (not browse-url-generic-program)
        (error "No browser defined (`browse-url-generic-program')"))
    (apply 'call-process browse-url-generic-program nil
	         0 nil
	         (append browse-url-generic-args
                   (list (format "start %s"
                                 (replace-regexp-in-string "&" "^&" url))))))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c")
   browse-url-browser-function #'my/browse-url-generic))

(provide 'init-global-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-global-config.el ends here
