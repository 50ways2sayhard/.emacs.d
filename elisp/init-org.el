;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Apr 14 17:01:44 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d org toc-org htmlize ox-gfm
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes org toc-org htmlize ox-gfm
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
(require 'org/+tables)
(require 'org/+funcs)

;; OrgPac
(defvar +org-capture-file-gtd (concat +self/org-base-dir "gtd.org"))
(defvar +org-capture-file-idea (concat +self/org-base-dir "ideas.org"))
(defvar +org-capture-file-note (concat +self/org-base-dir "notes.org"))
(defvar +org-capture-file-inbox (concat +self/org-base-dir "inbox.org"))
(defvar +org-capture-file-someday (concat +self/org-base-dir "someday.org"))
(defvar +org-capture-file-tickler (concat +self/org-base-dir "tickler.org"))
(defun archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
      (goto-char (line-beginning-position))
      (org-archive-subtree))))
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-update-cookies-h)
         (org-mode . org-num-mode))
  :bind
  (:map org-mode-map
        ([tab] . org-cycle))
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-ellipsis " ▼ ")
  (org-bullets-bullet-list '("#"))
  (org-tags-column -77)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-capture-bookmark nil) ;; TODO: no bookmark for refile
  (org-log-done 'time)
  (org-hide-emphasis-markers t)
  (org-deadline-warning-days 90)
  (org-agenda-span 7)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-start-on-weekday 1)
  (org-directory (expand-file-name +self/org-base-dir))
  (org-babel-python-command "python3")

  :config
  (setq org-clock-persist t
        org-clock-persist-file (concat +self/org-base-dir "org-clock-save.el"))
  (add-hook 'org-clock-in-hook #'org-save-all-org-buffers)
  (add-hook 'org-clock-out-hook #'org-save-all-org-buffers)
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (add-hook (quote hack-local-variables-hook)
            (lambda ()
              (let ((symbol (quote org-startup-folded)))
                (when (and (eq major-mode (quote org-mode))
                           (boundp symbol))
                  (let ((value (symbol-value symbol)))
                    (when (and value (integerp value))
                      (org-shifttab value)))))))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (+org-init-appearance-h)
  (+org-init-agenda-h)
  (+org-init-capture-defaults-h)

  (setq org-log-into-drawer "LOGBOOK")
  (setq org-agenda-files (list +org-capture-file-gtd
                               +org-capture-file-tickler))
  (setq org-refile-targets '((+org-capture-file-gtd :level . 1)
                             (+org-capture-file-someday :level . 1)
                             (+org-capture-file-tickler :level . 1)))
  (setq org-log-into-drawer t)
  (setq org-tag-alist '(("demand" . ?d) ("code" . ?a) ("life" . ?l) ("document" . ?p) ("emacs" . ?e) ("bug" . ?b) ("okr" . ?o)))
  (setq org-use-fast-todo-selection t)
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-capture-file-gtd "Next Actions")
           "* ☞TODO %i%? [0%] \n:LOGBOOK: \n:CREATED: %U \n:END:" :prepend t :kill-buffer t)
          ("w" "Waiting for" entry
           (file+headline +org-capture-file-tickler "Tickler")
           "* %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file-note "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("m" "Maybe" entry
           (file+headline +org-capture-file-someday "Some day/maybe")
           "* %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-file-idea "Ideas")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          )
        org-todo-keywords
        '((sequence "TODO(t!)" "DOING(i!)" "|" "DONE(d!)" "ABORT(a!)"))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        )
  (setq org-todo-keywords
        '((sequence
           "☞TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "⚔INPROCESS(s)"  ; A task that is in progress
           "⚑WAITING(w)"  ; Something is holding up this task; or it is paused
           "|"
           "☟NEXT(n)"
           "✰Important(i)"
           "✔DONE(d)"  ; Task successfully completed
           "✘CANCELED(c@)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "✍ NOTE(N)"
           "FIXME(f)"
           "☕ BREAK(b)"
           "❤ Love(l)"
           "REVIEW(r)"
           )) ; Task was completed
        org-todo-keyword-faces
        '(
          ("☞TODO" . (:foreground "#ff39a3" :weight bold))
          ("⚔INPROCESS"  . "orangered")
          ("✘CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("⚑WAITING" . "pink")
          ("☕BREAK" . "gray")
          ("❤LOVE" . (:foreground "VioletRed4"
                                  ;; :background "#7A586A"
                                  :weight bold))
          ("☟NEXT" . (:foreground "DeepSkyBlue"
                                  ;; :background "#7A586A"
                                  :weight bold))
          ("✰IMPORTANT" . (:foreground "greenyellow"
                                       ;; :background "#7A586A"
                                       :weight bold))
          ("✔DONE" . "#008080")
          ("FIXME" . "IndianRed"))
        )

  (add-hook 'after-change-major-mode-hook
            (lambda () (if (equal show-paren-mode 't)
    		              (when (derived-mode-p 'org-mode)
    		                (show-paren-mode -1))
                    (show-paren-mode 1))))

  ;; https://emacs-china.org/t/topic/2119/15
  (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
    (require 'cal-china)
    (if year
        (let* ((d-date (diary-make-date lunar-month lunar-day year))
               (a-date (calendar-absolute-from-gregorian d-date))
               (c-date (calendar-chinese-from-absolute a-date))
               (cycle (car c-date))
               (yy (cadr c-date))
               (y (+ (* 100 cycle) yy)))
          (diary-chinese-anniversary lunar-month lunar-day y mark))
      (diary-chinese-anniversary lunar-month lunar-day year mark)))

  ;; (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  (defun my:org-agenda-time-grid-spacing ()
    "Set different line spacing w.r.t. time duration."
    (save-excursion
      (let* ((background (alist-get 'background-mode (frame-parameters)))
             (background-dark-p (string= background "dark"))
             (colors (if background-dark-p
                         (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                       (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7")))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors)
                                                  :foreground
                                                  ,(if background-dark-p "black" "white")))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))

  (add-hook 'org-agenda-finalize-hook #'my:org-agenda-time-grid-spacing)

  ;; binding
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (general-define-key :states '(normal insert)
                      :keymaps 'org-mode-map
                      "C-<return>" #'+org/insert-item-below
                      "C-S-<return>" #'org-insert-subheading
                      )
  (general-define-key :states '(normal)
                      :keymaps 'org-mode-map
                      "<return>" #'+org/dwim-at-point)

  (local-leader-def
    :keymaps 'org-mode-map
    "'" 'org-edit-special
    "*" 'org-ctrl-c-star
    "+" 'org-ctrl-c-minus
    "," 'org-switchb
    ;; "." 'org-goto

    "." 'consult-org-heading

    "A" 'org-archive-subtree
    "e" 'org-export-dispatch
    "f" 'org-footnote-new
    "h" 'org-toggle-heading
    "i" 'org-toggle-item
    "I" 'org-toggle-inline-images
    "n" 'org-store-link
    "o" 'org-set-property
    "q" 'org-set-tags-command
    "t" 'org-todo
    "T" 'org-todo-list
    "x" 'org-toggle-checkbox

    "a" '(:wk "attackments")
    "aa" 'org-attach
    "ad" 'org-attach-delete-one
    "aD" 'org-attach-delete-all
    "af" '+org/find-file-in-attachments
    "al" '+org/attach-file-and-insert-link
    "an" 'org-attach-new
    "ao" 'org-attach-open
    "aO" 'org-attach-open-in-emacs
    "ar" 'org-attach-reveal
    "aR" 'org-attach-reveal-in-emacs
    "au" 'org-attach-url
    "as" 'org-attach-set-directory
    "aS" 'org-attach-sync

    "b"  '(:wk "tables")
    "b-" 'org-table-insert-hline
    "ba" 'org-table-align
    "bb" 'org-table-blank-field
    "bc" 'org-table-create-or-convert-from-region
    "be" 'org-table-edit-field
    "bf" 'org-table-edit-formulas
    "bh" 'org-table-field-info
    "bs" 'org-table-sort-lines
    "br" 'org-table-recalculate
    "bR" 'org-table-recalculate-buffer-tables
    "bd" '(:wk "delete")
    "bdc" 'org-table-delete-column
    "bdr" 'org-table-kill-row
    "bi" '(:wk "insert")
    "bic" 'org-table-insert-column
    "bih" 'org-table-insert-hline
    "bir" 'org-table-insert-row
    "biH" 'org-table-hline-and-move
    "bt" '("toggle")
    "btf" 'org-table-toggle-formula-debugger
    "bto" 'org-table-toggle-coordinate-overlays

    "c" '(:wk "clock")
    "cc" 'org-clock-cancel
    "cd" 'org-clock-mark-default-task
    "ce" 'org-clock-modify-effort-estimate
    "cE" 'org-set-effort
    "cg" 'org-clock-goto
    "cl" '+org/toggle-last-clock
    "ci" 'org-clock-in
    "cI" 'org-clock-in-last
    "co" 'org-clock-out
    "cr" 'org-resolve-clocks
    "cR" 'org-clock-report
    "ct" 'org-evaluate-time-range
    "c=" 'org-clock-timestamps-up
    "c-" 'org-clock-timestamps-down

    "d" '(:wk "date/deadline")
    "dd" 'org-deadline
    "ds" 'org-schedule
    "dt" 'org-time-stamp
    "dT" 'org-time-stamp-inactive

    "D" 'archive-done-tasks

    "g" '(:wk "goto")
    "gg" 'consult-org-heading
    "gc" 'org-clock-goto
    "gi" 'org-id-goto
    "gr" 'org-refile-goto-last-stored
    "gv" '+org/goto-visible
    "gx" 'org-capture-goto-last-stored

    "l" '(:wk "links")
    "lc" 'org-cliplink
    "ld" '+org/remove-link
    "li" 'org-id-store-link
    "ll" 'org-insert-link
    "lL" 'org-insert-all-links
    "ls" 'org-store-link
    "lS" 'org-insert-last-stored-link
    "lt" 'org-toggle-link-display

    "P" '(:wk "publish")
    "Pa" 'org-publish-all
    "Pf" 'org-publish-current-file
    "Pp" 'org-publish
    "PP" 'org-publish-current-project
    "Ps" 'org-publish-sitemap

    "r" '(:wk "refile")
    "r." '+org/refile-to-current-file
    "rc" '+org/refile-to-running-clock
    "rl" '+org/refile-to-last-location
    "rf" '+org/refile-to-file
    "ro" '+org/refile-to-other-window
    "rO" '+org/refile-to-other-buffer
    "rv" '+org/refile-to-visible
    "rr" 'org-refile

    "s" '(:wk "tree/subtree")
    "sa" 'org-toggle-archive-tag
    "sb" 'org-tree-to-indirect-buffer
    "sd" 'org-cut-subtree
    "sh" 'org-promote-subtree
    "sj" 'org-move-subtree-down
    "sk" 'org-move-subtree-up
    "sl" 'org-demote-subtree
    "sn" 'org-narrow-to-subtree
    "sr" 'org-refile
    "ss" 'org-sparse-tree
    "sA" 'org-archive-subtree
    "sN" 'widen
    "sS" 'org-sort

    "p" '(:wk "priority")
    "pd" 'org-priority-down
    "pp" 'org-priority
    "pu" 'org-priority-up


    "x" '(:wk "Download")
    "xc" 'org-download-clipboard
    "xd" 'org-download-delete
    "xi" 'org-download-image
    "xy" 'org-download-yank
    "xe" 'org-download-edit
    "xr" 'org-download-rename-at-point
    "xR" 'org-download-rename-last-file
    "xs" 'org-download-screenshot
    )

  (local-leader-def
    :keymaps 'org-agenda-mode-map
    "d" '(:wk "date/deadline")
    "dd" 'org-agenda-deadline
    "ds" 'org-agenda-schedule

    "c" '(:wk "clock")
    "cc" 'org-agenda-clock-cancel
    "cg" 'org-agenda-clock-goto
    "ci" 'org-agenda-clock-in
    "co" 'org-agenda-clock-out
    "cr" 'org-agenda-clockreport-mode
    "cs" 'org-agenda-show-clocking-issues

    "p" '(:wk "priority")
    "pd" 'org-agenda-priority-down
    "pp" 'org-agenda-priority
    "pu" 'org-agenda-priority-up

    "q" 'org-agenda-set-tags
    "r" 'org-agenda-refile
    "t" 'org-agenda-todo)
  )
;; -OrgPac

;; OrgDownload
(use-package org-download
  :defer t
  :commands (org-download-clipboard org-download-delete org-download-image org-download-yank org-download-edit org-download-rename-at-point org-download-rename-last-file org-download-screenshot)
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil)
  :config
  (cond (*sys/mac*
         (setq org-download-screenshot-method "screencapture -i %s"))))
;; -OrgDownload

(use-package plantuml-mode
  :defer t
  :config
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
  (setq plantuml-default-exec-mode 'jar)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
                                        ; Make babel results blocks lowercase
  (setq org-babel-results-keyword "results")

  (defun bh/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))
  )

(use-package org-contrib
  :after org)

(use-package valign
  :after org
  :defer t
  :bind (([remap org-table-align] . valign-table))
  :hook (org-mode . valign-mode)
  )


;; (use-package org-bars
;;   :straight (:host github :repo "tonyaldon/org-bars")
;;   :after org
;;   :hook (org-mode . org-bars-mode)
;;   :config
;;   (setq org-bars-color-options '(:only-one-color t
;;                                                  :bar-color "#4C4A4D")
;;         org-bars-stars '(:empty "◉"
;;                                 :invisible "▶"
;;                                 :visible "▼")))

(use-package calfw
  :commands (cfw:open-org-calendar)
  :straight (:host github :repo "zemaye/emacs-calfw")
  :bind (:map cfw:calendar-mode-map
              ("s" . cfw:show-details-command))
  :custom
  (cfw:display-calendar-holidays nil)
  :config
  (with-eval-after-load 'calfw
	  (use-package calfw-ical
	    :straight (:host github :repo "zemaye/emacs-calfw"))
	  (use-package calfw-org
	    :straight (:host github :repo "zemaye/emacs-calfw"))
	  (use-package calfw-cal
	    :straight (:host github :repo "zemaye/emacs-calfw"))))

;; (use-package org-fancy-priorities
;;   :after org
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴")))

(use-package electric-spacing
  :straight (:host github :repo "zk-phi/electric-spacing")
  :after org)

(use-package separate-inline
  :straight (:host github :repo "ingtshan/separate-inline.el")
  :hook ((org-mode-hook . separate-inline-mode)
         (org-mode-hook
          .
          (lambda ()
            (add-hook 'separate-inline-mode-hook
                      'separate-inline-use-default-rules-for-org-local
                      nil 'make-it-local)))))


(use-package org-modern
  :defer t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Looks better for tags
                              (setq line-spacing 0.1)
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1))))
  :config
  (setq
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  )

(defun +my/open-org-agenda ()
  "open org agenda in left window"
  (interactive)
  (org-agenda nil "n"))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
