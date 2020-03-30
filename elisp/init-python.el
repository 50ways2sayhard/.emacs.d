;;; init-python.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-python.el
;; Description: Initialize Python
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Jun 10 18:58:02 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: 一 3月 30 09:28:30 2020 (+0800)
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

;; PythonConfig
;; (use-package python-mode
;;   :ensure nil
;;   :after flycheck
;;   :mode "\\.py\\'"
;;   :custom
;;   (python-indent-offset 4)
;;   (flycheck-python-pycompile-executable "python3")
;;   (python-shell-interpreter "python3"))
;; -PythonConfig



(use-package python
  :ensure nil
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
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
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

  (use-package importmagic
    :defer t
    :hook (python-mode . importmagic-mode)
    :commands (importmagic-fix-imports importmagic-fix-symbol-at-point))

  (use-package python-pytest
    :defer t)
  )

;; LSPPythonPac
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :after lsp-mode python
  :if (or *python3* *python*)
  :custom
  (lsp-python-ms-nupkg-channel "beta")
  (lsp-python-ms-dir "~/.local/mspyls/")
  (lsp-python-executable-cmd "python")
  ;; :config
  ;; (defun find-vscode-mspyls-executable ()
  ;;   (let* ((wildcards ".vscode/extensions/ms-python.python-*/languageServer*/Microsoft.Python.LanguageServer")
  ;;          (dir-and-ext (if *sys/win32*
  ;;                           (cons (getenv "USERPROFILE") ".exe")
  ;;                         (cons (getenv "HOME") nil)))
  ;;          (cmd (concat (file-name-as-directory (car dir-and-ext))
  ;;                       wildcards (cdr dir-and-ext))))
  ;;     (file-expand-wildcards cmd t)))

  ;; (setq lsp-python-ms-executable
  ;;       (car (find-vscode-mspyls-executable)))
  ;; (setq lsp-python-ms-dir
  ;;       (file-name-directory lsp-python-ms-executable))
  )
;; -LSPPythonPac

(use-package poetry
  :after python
  )

(use-package sphinx-doc
  :after python
  :config
  (setq sphinx-doc-python-indent 4)
  )

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
