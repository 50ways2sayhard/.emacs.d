;;; init-flymake.el ---
;;
;; Filename: init-flymake.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Tue Mar 15 10:12:53 2022 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 24
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

;; From purcell

(with-eval-after-load 'flymake
  (defun sanityinc/eldoc-flymake-first ()
    "Gives flymake's eldoc function priority in the minibuffer."
    (when flymake-mode
      (setq-local eldoc-documentation-functions
                  (cons 'flymake-eldoc-function
                        (delq 'flymake-eldoc-function eldoc-documentation-functions)))))

  (add-hook 'flymake-mode-hook 'sanityinc/eldoc-flymake-first))

(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path
              load-path))

(use-package flymake-proc
  :config
  (progn
    ;; flymake-proc adds this legacy backend automatically but (1) I
    ;; don't seem to use it and (2) it triggers warnings in *Flymake
    ;; log*.
    (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)))

(use-package flymake-flycheck
  :diminish
  :config
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append (default-value 'flycheck-disabled-checkers)
                          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))
  (defun sanityinc/enable-flymake-flycheck ()
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions))))

  (add-hook 'flymake-mode-hook 'sanityinc/enable-flymake-flycheck)
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'text-mode-hook 'flymake-mode)
  (with-eval-after-load 'general
    (leader-def
      :keymaps 'override
      "el" '(consult-flymake :wk "List error")
      "ef" '(consult-flymake :wk "Find error")))
  )

(setq flymake-no-changes-timeout nil)

(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
