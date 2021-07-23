;;; init-javascript.el ---
;;
;; Filename: init-javascript.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Fri May  7 12:04:42 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 27
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

(eval-and-compile
  (require 'init-const)
  (require 'init-func)
  )

(use-package js2-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :hook ((js-mode . js2-minor-mode)
         (js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :hook ((js2-mode . (lambda()
                       (flycheck-add-mode 'javascript-eslint 'js2-mode))))
  :config
  (setq-default js2-use-font-lock-faces t
                js2-mode-must-byte-compile nil
                forward-sexp-function nil
                ;; {{ comment indention in modern frontend development
                javascript-indent-level 2
                js-indent-level 2
                css-indent-offset 2
                typescript-indent-level 2
                ;; }}
                js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
                js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
                js2-auto-indent-p nil
                js2-indent-on-enter-key nil ; annoying instead useful
                js2-skip-preprocessor-directives t
                js2-strict-inconsistent-return-warning nil ; return <=> return null
                js2-enter-indents-newline nil
                js2-bounce-indent-p t)

  (with-eval-after-load 'flycheck
    (when (or (executable-find "eslint_d")
              (executable-find "eslint")
              (executable-find "jshint"))
      (setq js2-mode-show-strict-warnings nil))
    (when (executable-find "eslint_d")
      ;; https://github.com/mantoni/eslint_d.js
      ;; npm -i -g eslint_d
      (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js-doc)

  (with-eval-after-load 'general
    (local-leader-def
      :keymaps 'js-mode-map
      "t" 'js-doc-insert-tag
      "f" 'js-doc-insert-function-doc
      "F" 'js-doc-insert-file-doc))
  )

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(with-eval-after-load 'js2-mode
  ;; I hate the hotkeys to hide things
  (define-key js2-mode-map (kbd "C-c C-e") nil)
  (define-key js2-mode-map (kbd "C-c C-s") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  (define-key js2-mode-map (kbd "C-c C-o") nil)
  (define-key js2-mode-map (kbd "C-c C-w") nil))



;; TypeScriptPac
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))
;; -TypeScriptPac

(provide 'init-javascript)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-javascript.el ends here
