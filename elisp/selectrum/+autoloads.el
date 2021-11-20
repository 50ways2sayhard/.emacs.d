;;; autoloads.el ---
;;
;; Filename: autoloads.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sat Nov 20 11:39:06 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Sat Nov 20 11:42:50 2021 (+0800)
;;           By: John
;;     Update #: 5
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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


;; Copy from selectrum
;;;###autoload
(defun +complete--metadata ()
  "Get completion metadata.
Demotes any errors to messages."
  (condition-case-unless-debug err
      (completion-metadata (minibuffer-contents)
                           minibuffer-completion-table
                           minibuffer-completion-predicate)
    (error (message (error-message-string err)) nil)))

;;;###autoload
(defun +complete--get-meta (setting)
  "Get metadata SETTING from completion table."
  (completion-metadata-get (+complete--metadata) setting))

;;;###autoload
(defun +complete-fido-backward-updir ()
  "Delete char before or go up directory, like `ido-mode'."
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (+complete--get-meta 'category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-delete-char)))

;;;###autoload
(defun +complete-fido-delete-char ()
  "Delete char or maybe call `dired', like `ido-mode'."
  (interactive)
  (let ((end (point-max)))
    (if (or (< (point) end) (not (eq (+complete--get-meta 'category) 'file)))
        (call-interactively 'delete-char)
      (dired (file-name-directory (minibuffer-contents)))
      (exit-minibuffer))))

;;;###autoload
(defun +vertico-directory-tidy ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (and (eq this-command #'self-insert-command)
             (bound-and-true-p rfn-eshadow-overlay)
             (overlay-buffer rfn-eshadow-overlay)
             (= (point) (point-max))
             (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
                 (eq ?/ (char-before (- (point) 2)))))
    (delete-region (overlay-start rfn-eshadow-overlay) (overlay-end rfn-eshadow-overlay))))


;;;###autoload
(defun +complete-get-current-candidate ()
  (selectrum-get-current-candidate))

;;;###autoload
(defun +complete-insert-current-candidate ()
  (selectrum-insert-current-candidate))

;;;###autoload
(defun +complete-fido-enter-dir ()
  (interactive)
  (let ((candidate (+complete-get-current-candidate))
        (current-input (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond
     ((and (eq (+complete--get-meta 'category) 'file)
           (string= (car (last (s-split "/" current-input))) ".."))
      (progn
        (insert "/")
        (+complete-fido-do-backward-updir)
        (+complete-fido-do-backward-updir)))

     ((and (eq (+complete--get-meta 'category) 'file)
           (file-directory-p candidate)
           (not (string= candidate "~/")))
      (+complete-insert-current-candidate))

     (t (insert "/")))))

;;;###autoload
(defun +complete-fido-do-backward-updir ()
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (+complete--get-meta 'category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))))

(provide 'selectrum/+autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autoloads.el ends here
