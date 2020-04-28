;;; init-nox.el ---
;;
;; Filename: init-nox.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: 一 3月 30 08:52:26 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 43
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

(use-package jsonrpc)

(use-package nox
  :quelpa (nox :fetcher github :repo "manateelazycat/nox")
  ;; :hook ((python-mode) . nox-ensure)
  :custom
  (nox-put-doc-in-help-buffer t)
  (nox-auto-display-help-buffer t)
  :config
  ;; (setq nox-python-server "pyls")
  (setq nox-python-server-dir "~/.local/mspyls/")
  (defun push-tabnine ()
    (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
    )
  (add-hook 'nox-managed-mode-hook #'push-tabnine)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (setq nox-python-path (concat python-shell-virtualenv-path "/bin/python"))
                                          ))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda () (setq nox-python-path "/usr/bin/python")))
  )

(provide 'init-nox)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-nox.el ends here
