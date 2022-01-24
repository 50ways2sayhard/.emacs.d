;;; init-tabnine-capf.el ---
;;
;; Filename: init-tabnine-capf.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Mon Jan 24 11:45:34 2022 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 11
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

(defcustom company-tabnine-capf-threshold 60
  "the tabnine threshold for sorting two different backend"
  :group 'company-tabnine-capf
  :type 'integer)

(setq company-tabnine-binaries-folder "~/.TabNine/")

(defun company-tabnine-capf--annotation (candidate)
  "read annotation from candidate
company-tabnine's annotation is stored in text properties, so smart!
so if annotation cannot read from properties, just put into company-capf"
  (let ((annotation (get-text-property 0 'annotation candidate)))
    (if annotation
        annotation
      (company-capf--annotation candidate))))

(defun company-tabnine-capf--post-completion (candidate)
  "call post completion and return nil"
  (if (get-text-property 0 'old_suffix candidate)
      (company-tabnine--post-completion candidate)
    (company--capf-post-completion candidate)))

(defun company-tabnine-capf--mix-candidates (&optional tabnine-result capf-result)
  (cond
     ((eq (length tabnine-result) 0) capf-result)
     ((eq (length capf-result) 0) tabnine-result)
     ((> (company-tabnine-capf--extract-tabnine-confidence (car tabnine-result)) company-tabnine-capf-threshold)
      `(,(car tabnine-result) ,@(company-tabnine-capf--mix-candidates (cdr tabnine-result) capf-result)))
     (t `(,@capf-result ,@tabnine-result))))

(defun company-tabnine-capf--candidates (prefix)
  "combine and sort the company-tabnine and the company-capf results.
if company-tabnine's confidence is greater then `company-tabnine-capf-threshold',
tabnine's candidate have greater position then others."
  (let ((tabnine-result (company-tabnine--candidates prefix))
        (capf-result (company-capf--candidates prefix)))
    (company-tabnine-capf--mix-candidates tabnine-result capf-result)))

(defun company-tabnine-capf--extract-tabnine-confidence (candidate)
  "extract integer from company-tabnine's candidate"
  (string-to-number (get-text-property 0 'annotation candidate)))

(defun company-tabnine-capf--meta (candidate)
  "return meta data for candidate"
  (let ((tabnine-meta (company-tabnine--meta candidate)))
    (if tabnine-meta
        tabnine-meta
      (company-capf 'meta candidate))))

;;;###autoload
(defun company-tabnine-capf (command &rest args)
  "a company backend for combine tabnine and capf"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tabnine-capf))
    (prefix
     (apply 'company-tabnine `(,command ,@args))
     (apply 'company-capf `(,command ,@args)))
    (meta (company-tabnine-capf--meta (car args)))
    (annotation (company-tabnine-capf--annotation (car args)))
    (sorted t)
    (no-cache t)
    (kind (apply 'company-capf `(,command ,@args)))
    (post-completion (company-tabnine-capf--post-completion (car args)))
    (candidates (company-tabnine-capf--candidates (car args)))))

(defun toggle-company-tabnine-capf ()
  "toggle company-tabnine-capf backend"
  (interactive)
  (when (not (file-exists-p company-tabnine-binaries-folder))
    (company-tabnine-install-binary))
  (if (memq 'company-tabnine-capf company-backends)
      (progn
        (setq company-backends (remove 'company-tabnine-capf company-backends))
        (message "company-tabnine-capf disabled"))
    (add-to-list 'company-backends 'company-tabnine-capf)
    (message "company-tabnine-capf enabled!")))

(provide 'init-tabnine-capf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tabnine-capf.el ends here
