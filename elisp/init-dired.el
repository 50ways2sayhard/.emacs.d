;;; init-dired.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dired.el
;; Description: Initialize Dired and Related Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:37:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Apr 20 16:31:47 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d dired auto-save
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes dired, disk-usage
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

;; DiredPackage
(use-package dired
  :commands (dired)
  :straight nil
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (with-eval-after-load 'general
    (general-define-key :states '(normal)
                        :keymaps 'dired-mode-map
                        "l" 'dired-find-alternate-file
                        "h"  'dired-up-directory)
    )
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  )


;; Colourful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))


(use-package dired-git-info
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map ")" 'dired-git-info-mode))

;; Extra Dired functionality
(use-package dired-x
  :straight nil
  :demand
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
    :after dired))

(use-package dired-narrow
  :after dired) ;; use `s' for fliter
(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions
        (mapcar (lambda (ext)
                  (cons ext "open")) '("pdf" "doc" "docx" "ppt" "pptx"))))

(use-package dirvish
  :straight (dirvish :includes (dirvish-extras dirvish-side dirvish-peek dirvish-vc dirvish-yank dirvish-fd) :files (:defaults "extensions/dirvish-*.el"))
  :after dired
  :hook ((+self/first-input . dirvish-override-dired-mode)
         (evil-collection-setup . (lambda (&rest a)
                                    (evil-define-key '(normal) dired-mode-map
                                      (kbd "C-c f") 'dirvish-fd
                                      "i" 'wdired-change-to-wdired-mode
                                      "." 'dired-omit-mode
                                      (kbd "TAB") 'dirvish-subtree-toggle
                                      (kbd "M-s") 'dirvish-setup-menu
                                      (kbd "M-f") 'dirvish-toggle-fullscreen
                                      "*"   'dirvish-mark-menu
                                      "f"   'dirvish-file-info-menu
                                      [remap dired-sort-toggle-or-edit] 'dirvish-quicksort
                                      [remap dired-do-redisplay] 'dirvish-ls-switches-menu
                                      [remap dired-summary] 'dirvish-dispatch
                                      [remap dired-do-copy] 'dirvish-yank-menu
                                      [remap mode-line-other-buffer] 'dirvish-history-last))))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :custom
  (dirvish-attributes '(all-the-icons file-size))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-side-follow-buffer-file t)
  :config
  (when (boundp 'dirvish-side-follow-mode)
    (dirvish-side-follow-mode t))
  (set-face-attribute 'ansi-color-blue nil :foreground "#FFFFFF")
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (general-define-key :states '(normal)
                      :keymaps 'dirvish-mode-map
                      "?" 'dirvish-menu-all-cmds)

  (use-package dirvish-extras
    :straight nil
    :after dirvish))


;; SaveAllBuffers
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))
(with-eval-after-load 'general
  (general-def "C-x C-s" nil)
  (general-def "C-x C-s" 'save-all-buffers)
  )
;; -SaveAllBuffers

(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
