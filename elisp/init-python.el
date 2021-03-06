;;; init-python.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-python.el
;; Description: Initialize Python
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Jun 10 18:58:02 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Fri May  7 12:15:53 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: lsp-python-ms
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes lsp-python-ms
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
  (require 'init-flycheck)
  (require 'init-const)
  (require 'init-func)
  )

(use-package python
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  (add-hook 'python-mode-hook (lambda()
                                (flycheck-add-mode 'python-flake8 'python-mode)
                                ;; (flycheck-add-next-checker 'lsp '(t . python-flake8))
                                ))
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (setq python-indent-offset 4)
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; Live Coding in Python
  (use-package live-py-mode)

  (use-package pyvenv
    :config
    (with-eval-after-load 'exec-path-from-shell
      (exec-path-from-shell-copy-env "WORKON_HOME"))
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
    ;; (add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-restart-workspace)))
    ;; (add-hook 'pyvenv-post-deactivate-hooks (lambda () (lsp-restart-workspace)))
    (add-to-list 'global-mode-string
                 '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
                 'append)
    )

  (use-package py-isort
    :defer t
    :init
    (setq python-sort-imports-on-save t)
    (defun +python/python-sort-imports ()
      (interactive)
      (when (and python-sort-imports-on-save
                 (derived-mode-p 'python-mode))
        (py-isort-before-save)))
    (add-hook 'python-mode-hook
              (lambda() (add-hook 'before-save-hook #'+python/python-sort-imports)))
    )

  (use-package python-pytest
    :defer t)
  )

(use-package poetry
  :after python
  )

(use-package sphinx-doc
  :after python
  :config
  (setq sphinx-doc-python-indent 4)
  )


(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         ))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3"))
  (setq lsp-pyright-venv-path ".venv")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-auto-search-paths nil)
  (setq lsp-pyright-auto-import-completions nil)
  )

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
