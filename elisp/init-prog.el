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
;;     Update #: 49
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

(defvar devdocs-major-mode-docs-alist
  '((web-mode . ("Javascript" "Less" "HTML" "Vue.js~2" "CSS"))))


;;;###autoload
(defun +devdocs-lookup-at-point()
  (interactive)
  (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))

;;;###autoload
(defun +devdocs-search-at-point()
  (interactive)
  (devdocs-search (thing-at-point 'symbol)))

;;;###autoload
(defun devdocs-dwim()
  "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
  (interactive)
  ;; Install the doc if it's not installed
  (mapc
   (lambda (str)
     (let* ((docs (split-string str " "))
            (doc (if (length= docs 1)
                     (downcase (car docs))
                   (concat (downcase (car docs)) "~" (downcase (cdr docs))))))
       (unless (and (file-directory-p devdocs-data-dir)
                    (directory-files devdocs-data-dir nil "^[^.]"))
         (message "Installing %s..." str)
         (devdocs-install doc))))
   (alist-get major-mode devdocs-major-mode-docs-alist))

  ;; Lookup the symbol at point
  (devdocs-lookup nil (thing-at-point 'symbol t)))

(use-package devdocs
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :init
  (mapc
   (lambda (e)
     (add-hook (intern (format "%s-hook" (car e)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr e)))))
   devdocs-major-mode-docs-alist))

(use-package imenu
  :hook (imenu-after-jump . recenter))

(use-package separedit
  :defer t
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-default-mode 'markdown-mode)
  :config
  (add-to-list 'separedit-comment-delimiter-alist '(("///" "//") . (dart-mode)))
  )

(use-package xref
  :straight nil
  :init
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :hook ((xref-after-return xref-after-jump) . recenter))

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; FormatAllPac
(use-package format-all
  :hook (((emacs-lisp-mode) . format-all-mode)
         ((prog-mode) . format-all-ensure-formatter))
  :config
  ;; (setq format-all-formatters '(("Vue" (prettier "--parser vue"))))
  (setq format-all-show-errors 'never)
  )
;; -FormatAllPac


(use-package treesit
  :if (treesit-available-p)
  :straight (:type built-in)
  :defer t
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          ))
  :config
  (setq major-mode-remap-alist
        '((javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (python-mode . python-ts-mode)
          (js-json-mode . json-ts-mode)
          (sh-mode . bash-ts-mode)))
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))


(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
