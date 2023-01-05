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

;;;###autoload
(defun +open-configuration-folder ()
  "Open configuration folder."
  (interactive)
  (find-file (read-file-name ".emacs.d: " "~/.emacs.d/elisp/")))

;;;###autoload
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

;;;###autoload
(defun +my-delete-file ()
  "Put current buffer file to top."
  (interactive)
  (delete-file
   (read-file-name "Delete: " default-directory buffer-file-name))
  (unless (file-exists-p (buffer-file-name))
    (kill-current-buffer)))

;;;###autoload
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

(defun +complete--get-meta (setting)
  "Get metadata SETTING from completion table."
  (completion-metadata-get
   (condition-case-unless-debug err
       (completion-metadata (minibuffer-contents)
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
     (error (message (error-message-string err)) nil))
   setting))


;;;###autoload
(defun open-in-external-app ()
  "TODO: only for macos now."
  (interactive)
  (let ((candidate (vertico--candidate)))
    (when (eq (+complete--get-meta 'category) 'file)
      (shell-command (concat "open " candidate))
      (abort-recursive-edit))))


;;;###autoload
(defun +my/find-project-root()
  (interactive)
  (let ((project (project-current)))
    (if project
        (project-root project) ;;  HACK: original repo breaks here
      nil)))

(defun my-project-root (&optional dir)
  "Return the project root of DIR."
  (when-let* ((default-directory (or dir default-directory))
              (project (project-current)))
    (expand-file-name (if (fboundp 'project-root)
                          (project-root project)
                        (cdr project)))))


(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
               (regexp-opt (x-defined-colors) 'words))
      (0 (let ((colour (match-string-no-properties 0)))
           (put-text-property
            (match-beginning 0) (match-end 0)
            'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                      "white" "black"))
                    (:background ,colour)))))))))


(defun +my/google-it (&optional word)
  "Google it."
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  (thing-at-point 'symbol))))
  (browse-url (concat "https://www.google.com/search?q=" word)))


(provide 'init-func)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
