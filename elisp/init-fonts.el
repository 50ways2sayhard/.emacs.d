;;; init-fonts.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-fonts.el
;; Description: Initialize Fonts and Icons
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:32:54 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Aug  5 17:48:25 2021 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d fonts
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes fonts and icons
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

;; FontsList
(defvar font-list '(("Cascadia Code" . 15) ("Iosevka Comfy" . 16) ("Maple Mono SC NF" . 14) ("Fira Code" . 15) ("SF Mono" . 15))
  "List of fonts and sizes.  The first one available will be used.")
;; -FontsList

;; FontFun
(defun change-font ()
  "Interactively change a font from a list a available fonts."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

(defun my-apply-font ()
  ;; Set default font
  ;; (cl-loop for font in '("SF Mono" "Cascadia Code" "Fira Code")
  ;;          when (font-installed-p font)
  ;;          return (set-face-attribute 'default nil
  ;;                                     :font font
  ;;                                     :height (cond (*sys/mac* 160)
  ;;                                                   (*sys/win32* 110)
  ;;                                                   (t 130))))
  (set-face-attribute 'default nil
                      :font "Cascadia Code"
                      :height (cond (*sys/mac* 150)
                                    (t 130)))


  ;; Specify font for all unicode characters
  ;; (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
  ;;          when (font-installed-p font)
  ;;          return(set-fontset-font t 'unicode font nil 'prepend))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)

  ;; Specify font for Chinese characters
  ;; (cl-loop for font in '("Sarasa Mono SC Nerd" "Microsoft Yahei")
  ;;          when (font-installed-p font)
  ;;          return (set-fontset-font t '(#x4e00 . #x9fff) font)))
  (set-fontset-font t '(#x4e00 . #x9fff) "Maple Mono SC NF")

  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

(add-hook 'after-init-hook #'my-apply-font)


;; ATIPac
;; (use-package all-the-icons :if *sys/gui*)
(use-package all-the-icons
  :if *sys/gui*
  :config
  (declare-function memoize 'memoize)
  (declare-function memoize-restore 'memoize)
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (ignore-errors
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  ;; Support more icons
  (defvar my-extension-icon-alist
    '(("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
      ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
      ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
      ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
      ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
      ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
      ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen)))

  (defvar my-regexp-icon-alist
    '(("Cask\\'"             all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
      ("^Rakefile$"          all-the-icons-alltheicon "ruby-alt"    :face all-the-icons-red)
      ("\\.\\(bat\\|cmd\\)$" all-the-icons-alltheicon "terminal"    :face all-the-icons-lsilver)
      ("\\go.mod$"           all-the-icons-fileicon "go"            :face all-the-icons-dblue)
      ("\\go.sum$"           all-the-icons-fileicon "go"            :face all-the-icons-dpurple)
      ("\\.[bB][iI][nN]$"    all-the-icons-octicon "file-binary"    :v-adjust 0.0 :face all-the-icons-yellow)
      ("NEWS$"               all-the-icons-faicon "newspaper-o"     :height 0.9 :v-adjust -0.2)))

  (defvar my-mode-icon-alist
    '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
      (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
      (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
      (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
      (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
      (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
      (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
      (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
      (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
      (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
      (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
      (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
      (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
      (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
      (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
      (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
      (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
      (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
      (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
      (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
      (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
      (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
      (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
      (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
      (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
      (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
      (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)))

  (dolist (i my-extension-icon-alist)
    (add-to-list 'all-the-icons-extension-icon-alist i))
  (dolist (i my-regexp-icon-alist)
    (add-to-list 'all-the-icons-regexp-icon-alist i))
  (dolist (i my-mode-icon-alist)
    (add-to-list 'all-the-icons-mode-icon-alist i)))
;; -ATIPac

(provide 'init-fonts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
