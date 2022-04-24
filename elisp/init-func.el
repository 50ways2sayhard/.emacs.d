;;; init-func.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-func.el
;; Description: Initialize Functions
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Sun Jun  9 17:53:44 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr 23 18:12:16 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes functions
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

;; BetterMiniBuffer
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; -BetterMiniBuffer

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun +open-configuration-folder ()
  "Open configuration folder."
  (interactive)
  (find-file (read-file-name ".emacs.d: " "~/.emacs.d/elisp/")))

(defun +my-rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory buffer-file-name))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      ;; (set-visited-file-name file-to)
      ;; (rename-buffer file-to)
      ;; (save-buffer)
      (kill-buffer)
      (find-file file-to))))

(defun +my-delete-file ()
  "Put current buffer file to top."
  (interactive)
  (delete-file
   (read-file-name "Delete: " default-directory buffer-file-name))
  (unless (file-exists-p (buffer-file-name))
    (kill-current-buffer)))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (doom-modeline-update-env)
      (force-mode-line-update))))

(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq doom-modeline-env--version
            (bound-and-true-p doom-modeline-load-string))))
  (force-mode-line-update t))


(defvar my-load-user-customized-major-mode-hook t)
(defvar my-force-buffer-file-temp-p nil)
(defun is-buffer-file-temp ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
    (cond
     ((not my-load-user-customized-major-mode-hook)
      (setq rlt t))

     ((and (buffer-name) (string-match "\\* Org SRc" (buffer-name)))
      ;; org-babel edit inline code block need calling hook
      (setq rlt nil))

     ((null f)
      (setq rlt t))

     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))

     ((and (string-match "\.html$" f)
           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
      ;; file is a html file exported from org-mode
      (setq rlt t))

     (my-force-buffer-file-temp-p
      (setq rlt t))

     (t
      (setq rlt nil)))
    rlt))

(defun +my-imenu (args)
  "Call 'consult-outline' in 'org-mode' else 'consult-imenu'."
  (interactive "P")
  (if (derived-mode-p 'org-mode)
      (consult-outline)
    (if (boundp 'lsp-mode)
        (if (and lsp-mode lsp-enable-imenu (not lsp-use-plists))
            (consult-lsp-file-symbols args)
          (consult-imenu))
      (consult-imenu)
      )
    )
  )


;;;###autoload
(defun +default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun +default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

(defun my-open-recent ()
  "Open recent directory in dired or file otherwise."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (if (derived-mode-p 'dired-mode)
      (find-file (completing-read "Find recent dirs: "
                                  (delete-dups
                                   (append (mapcar 'file-name-directory recentf-list)))))
    (consult-recent-file)))

;;;###autoload
(defun open-in-external-app ()
  "TODO: only for macos now."
  (interactive)
  (let ((candidate (+complete-get-current-candidate)))
    (when (eq (+complete--get-meta 'category) 'file)
      (shell-command (concat "open " candidate))
      (abort-recursive-edit))))


(provide 'init-func)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
