;;; init-shell.el ---
;;
;; Filename: init-shell.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Fri Oct  8 22:48:57 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 8
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
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :commands vterm--internal
    :bind (:map vterm-mode-map
                ([f9] . (lambda ()
                          (interactive)
                          (and (fboundp 'shell-pop)
                               (shell-pop nil)))))
    :init
    (setq vterm-always-compile-module t)

    (with-no-warnings
      (defvar vterm-posframe--frame nil)

      (defun vterm-posframe-hidehandler (_)
        "Hidehandler used by `vterm-posframe-toggle'."
        (not (eq (selected-frame) posframe--frame)))

      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let ((buffer (vterm--internal #'ignore 100)))
          (if (and vterm-posframe--frame
                   (frame-live-p vterm-posframe--frame)
                   (frame-visible-p vterm-posframe--frame))
              (progn
                (posframe-hide buffer)
                ;; Focus the parent frame
                (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
            (let ((width  (max 80 (/ (frame-width) 2)))
                  (height (/ (frame-height) 2)))
              (setq vterm-posframe--frame
                    (posframe-show
                     buffer
                     :poshandler #'posframe-poshandler-frame-center
                     :hidehandler #'vterm-posframe-hidehandler
                     :left-fringe 8
                     :right-fringe 8
                     :width width
                     :height height
                     :min-width width
                     :min-height height
                     :internal-border-width 3
                     :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                     :background-color (face-background 'tooltip nil t)
                     :override-parameters '((cursor-type . t))
                     :accept-focus t))
              ;; Blink cursor
              (with-current-buffer buffer
                (save-excursion (vterm-clear t))
                (setq-local cursor-type 'box))
              ;; Focus the child frame
              (select-frame-set-input-focus vterm-posframe--frame))))))))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
