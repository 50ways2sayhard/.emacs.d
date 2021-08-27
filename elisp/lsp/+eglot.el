;;; +eglot.el ---
;;
;; Filename: +eglot.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Fri Aug 27 22:57:49 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 4
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




(defvar-local +lsp--flycheck-eglot--current-errors nil)

(defun +lsp--flycheck-eglot-init (checker callback)
  "CHECKER is the checker (eglot).
CALLBACK is the function that we need to call when we are done, on all the errors."
  (eglot-flymake-backend #'+lsp--flycheck-eglot--on-diagnostics)
  (funcall callback 'finished +lsp--flycheck-eglot--current-errors))

(defun +lsp--flycheck-eglot--on-diagnostics (diags &rest _)
  (cl-labels
      ((flymake-diag->flycheck-err
        (diag)
        (with-current-buffer (flymake--diag-buffer diag)
          (flycheck-error-new-at-pos
           (flymake--diag-beg diag)
           (pcase (flymake--diag-type diag)
             ('eglot-note 'info)
             ('eglot-warning 'warning)
             ('eglot-error 'error)
             (_ (error "Unknown diagnostic type, %S" diag)))
           (flymake--diag-text diag)
           :end-pos (flymake--diag-end diag)
           :checker 'eglot
           :buffer (current-buffer)
           :filename (buffer-file-name)))))
    (setq +lsp--flycheck-eglot--current-errors
          (mapcar #'flymake-diag->flycheck-err diags))
    ;; Call Flycheck to update the diagnostics annotations
    (flycheck-buffer-deferred)))

(defun +lsp--flycheck-eglot-available-p ()
  (bound-and-true-p eglot--managed-mode))

(flycheck-define-generic-checker 'eglot
  "Report `eglot' diagnostics using `flycheck'."
  :start #'+lsp--flycheck-eglot-init
  :predicate #'+lsp--flycheck-eglot-available-p
  :modes '(prog-mode text-mode))

(push 'eglot flycheck-checkers)

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (when eglot--managed-mode
                                       (flymake-mode -1)
                                       (when-let ((current-checker (flycheck-get-checker-for-buffer)))
                                         (unless (equal current-checker 'eglot)
                                           (flycheck-add-next-checker 'eglot current-checker)))
                                       (flycheck-add-mode 'eglot major-mode)
                                       (flycheck-mode 1)
                                       ;; Call flycheck on initilization to make sure to display initial
                                       ;; errors
                                       (flycheck-buffer-deferred))))


(provide 'lsp/+eglot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +eglot.el ends here
