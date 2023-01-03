;;; init-webdev.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-webdev.el
;; Description: Initialize Web, Emmet, JS2, TypeScript, Tide
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:03:43 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr  9 18:51:28 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d web-mode js2-mode typescript-mode emmet instant-rename-tag json-mode
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes web-mode js2-mode typescript-mode emmet instant-rename-tag instant-rename-tag
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

;; WebModePac
(use-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.vue\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.wxml\\'")
  :custom
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-block-padding 0)
  (web-mode-part-padding 0)
  :init
  (defun my/web-vue-setup()
    (with-eval-after-load 'general
      (local-leader-def
        :keymaps 'web-mode-map
        "f" 'lsp-eslint-fix-all))
    (setq-local lsp-enable-imenu t)
    (make-local-variable 'before-save-hook)
    (with-eval-after-load 'lsp-eslint
      (add-hook 'before-save-hook 'lsp-eslint-fix-all)))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-comment-annotation t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-comment-formats '(("java" . "//") ("javascript" . "//") ("php" . "//")))
  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  ;; "-" as word so company completes kabeb-case
  (modify-syntax-entry ?_ "w" web-mode-syntax-table)
  (modify-syntax-entry ?- "w" web-mode-syntax-table)
  (modify-syntax-entry ?# "_" web-mode-syntax-table)
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'")))
  (add-hook 'web-mode-hook
            (lambda ()
              (cond ((equal web-mode-content-type "vue")
                     (my/web-vue-setup)))))

  (local-leader-def
    :keymaps 'web-mode-map
    "d" '(web-mode-element-vanish :wk "Remove current element")
    "D" '(web-mode-element-kill :wk "Remove region")
    "r" '(instant-rename-tag :wk "Rename current tag")
    )
  )
;; -WebModePac


;; EmmetPac
(use-package emmet-mode
  :defer t
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  ;; :bind (:map web-mode-map
  ;;             ("C-j" . emmet-expand-yas))
  :config
  (add-hook 'emmet-mode-hook (lambda()
                               (setq emmet-indent-after-insert t))))
;; -EmmetPac

;; InstantRenameTagPac
(use-package instant-rename-tag
  :defer t
  :straight (:host github :repo "manateelazycat/instant-rename-tag" :depth 1)
  :commands instant-rename-tag
  :bind ("C-z <" . instant-rename-tag))
;; -InstantRenameTagPac

(use-package js-doc :defer t)

(provide 'init-webdev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-webdev.el ends here
