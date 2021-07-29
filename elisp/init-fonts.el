;;; init-fonts.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-fonts.el
;; Description: Initialize Fonts and Icons
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:32:54 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Jul 29 15:36:02 2021 (+0800)
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
(defvar font-list '(("Cascadia Code" . 13) ("Hack" . 13) ("Inconsolata" . 13) ("Fira Code" . 13) ("SF Mono" . 13))
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
  (set-fontset-font t '(#x4e00 . #x9fff) "Sarasa Mono SC Nerd")

  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  )

(add-hook 'after-init-hook #'my-apply-font)


;; ATIPac
;; (use-package all-the-icons :if *sys/gui*)
(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or *sys/win32* (font-installed-p "all-the-icons"))
          (all-the-icons-install-fonts t))
  :config
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (dolist (f '(all-the-icons-icon-for-file
                 all-the-icons-icon-for-mode
                 all-the-icons-icon-for-url
                 all-the-icons-icon-family-for-file
                 all-the-icons-icon-family-for-mode
                 all-the-icons-icon-family))
      (ignore-errors
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue))
  )

;; -ATIPac

(provide 'init-fonts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
