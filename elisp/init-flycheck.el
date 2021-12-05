;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-flycheck.el
;; Description: Initialize Flycheck
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:08:22 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun Dec  5 23:01:09 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d flycheck
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes flycheck
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
  (require 'init-const))

;; FlyCheckPac
;; (use-package flycheck
;;   :defer t
;;   :diminish
;;   :hook ((prog-mode markdown-mode) . flycheck-mode)
;;   :custom
;;   (flycheck-global-modes
;;    '(not text-mode outline-mode fundamental-mode org-mode
;;          diff-mode shell-mode eshell-mode term-mode))
;;   (flycheck-emacs-lisp-load-path 'inherit)
;;   (flycheck-indication-mode 'right-fringe)
;;   :init
;;   (if *sys/gui*
;;       (use-package flycheck-posframe
;;         :hook (flycheck-mode . flycheck-posframe-mode)
;;         :custom
;;         (flycheck-posframe-border-width 1)
;;         (flycheck-posframe-inhibit-functions
;;          '((lambda (&rest _) (bound-and-true-p company-backend))))
;;         :config
;;         (flycheck-posframe-configure-pretty-defaults))
;;     (use-package flycheck-pos-tip
;;       :defines flycheck-pos-tip-timeout
;;       :hook (flycheck-mode . flycheck-pos-tip-mode)
;;       :custom (flycheck-pos-tip-timeout 30)))
;;   :config
;;   (when (fboundp 'define-fringe-bitmap)
;;     (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
;;       [16 48 112 240 112 48 16] nil nil 'center))
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (flycheck-add-mode 'typescript-tslint 'rjsx-mode))
;; -FlyCheckPac


(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (prog-mode . flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              flycheck-emacs-lisp-load-path 'inherit
              flycheck-indication-mode (if (display-graphic-p)
                                           'right-fringe
                                         'right-margin)
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)

  ;; Display Flycheck errors
  (if *sys/gui*
      (use-package flycheck-posframe
        :custom-face
        (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-background-face ((t (:inherit tooltip))))
        (flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :init
        (setq flycheck-posframe-border-width 1)
        (add-hook 'flycheck-posframe-inhibit-functions
                  (lambda (&rest _) (bound-and-true-p company-backend)))
        :config
        (with-no-warnings
          ;; FIXME: Add paddings to the child frame.
          ;; @see https://github.com/alexmurray/flycheck-posframe/issues/28
          (defun my-flycheck-posframe-show-posframe (errors)
            "Display ERRORS, using posframe.el library."
            (posframe-hide flycheck-posframe-buffer)
            (when (and errors
                       (not (run-hook-with-args-until-success 'flycheck-posframe-inhibit-functions)))
              (let ((poshandler (intern (format "posframe-poshandler-%s" flycheck-posframe-position)))
                    (str (flycheck-posframe-format-errors errors)))
                (unless (functionp poshandler)
                  (setq poshandler nil))
                (flycheck-posframe-check-position)
                (posframe-show
                 flycheck-posframe-buffer
                 :string (concat (propertize "\n" 'face '(:height 0.3))
                                 str
                                 (propertize "\n\n" 'face '(:height 0.3)))
                 :background-color (face-background 'flycheck-posframe-background-face nil t)
                 :position (point)
                 :left-fringe 8
                 :right-fringe 8
                 :max-width (round (* (frame-width) 0.62))
                 :max-height (round (* (frame-height) 0.62))
                 :internal-border-width flycheck-posframe-border-width
                 :internal-border-color (face-foreground 'flycheck-posframe-border-face nil t)
                 :poshandler poshandler
                 :hidehandler #'flycheck-posframe-hidehandler))))
          (advice-add #'flycheck-posframe-show-posframe :override #'my-flycheck-posframe-show-posframe)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
