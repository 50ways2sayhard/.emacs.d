;;; init-prog.el ---
;;
;; Filename: init-prog.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sat Jul 24 02:54:32 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 19
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


(use-package devdocs
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :config
  (add-hook 'web-mode-hook (
                            lambda () ((setq-local devdocs-current-docs '("Javascript" "Less" "HTML" "Vue.js~2" "CSS")))))
  (defun +devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))
  (defun +devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol)))
  )


(use-package citre
  :straight (:host github :repo "universal-ctags/citre")
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :custom
  (citre-default-create-tags-file-location 'global-cache)
  :config
  (require 'citre-config)
  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))
  ;; Integrate with `lsp-mode' and `eglot'
  (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
    (let ((fetcher (apply fn args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (ignore xref-backend-functions)
             (apply fn args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))

  (defun lsp-citre-capf-function ()
    "A capf backend that tries lsp first, then Citre."
    (let ((lsp-result
           ('lsp-mode
            (and (fboundp #'lsp-completion-at-point)
                 (lsp-completion-at-point))))
          (if (and lsp-result
                   (try-completion
                    (buffer-substring (nth 0 lsp-result)
                                      (nth 1 lsp-result))
                    (nth 2 lsp-result)))
              lsp-result
            (citre-completion-at-point))))

    (defun enable-lsp-citre-capf-backend ()
      "Enable the lsp + Citre capf backend in current buffer."
      (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

    (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend))

  (defun my--push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))
  (dolist (func '(find-function
                  consult-imenu
                  projectile-grep
                  citre-jump))
    (advice-add func :before 'my--push-point-to-xref-marker-stack)))

(use-package imenu-list
  :defer t)

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
