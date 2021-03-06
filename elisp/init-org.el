;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Mon Jun 28 16:10:31 2021 (+0800)
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
(defvar +org-capture-file-gtd (concat org-base-dir "gtd.org"))
(defvar +org-capture-file-idea (concat org-base-dir "ideas.org"))
(defvar +org-capture-file-note (concat org-base-dir "notes.org"))
(defvar +org-capture-file-inbox (concat org-base-dir "inbox.org"))
(defvar +org-capture-file-someday (concat org-base-dir "someday.org"))
(defvar +org-capture-file-tickler (concat org-base-dir "tickler.org"))
(defun archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
      (goto-char (line-beginning-position))
      (org-archive-subtree))))
(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-update-cookies-h))
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-directory (expand-file-name org-base-dir))
  (org-ellipsis " ??? ")
  (org-babel-python-command "python3")
  (org-bullets-bullet-list '("#"))
  (org-tags-column -77)
  (org-capture-bookmark nil)

  :config
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
           "* TODO %i%? [0%] \n:LOGBOOK: \n:CREATED: %U \n:END:" :prepend t :kill-buffer t)
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
        ;; org-agenda-window-setup 'other-window
        org-todo-keyword-faces '(("DOING" . warning)
                                 ("ABORT" . error))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        )

  (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  ;; TocOrgPac
  (use-package toc-org
    :after org
    :hook (org-mode . toc-org-enable)
    :config (setq toc-org-hrefify-default "gh")
    )
  ;; -TocOrgPac


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
  )
;; -OrgPac

;; OrgDownload
(use-package org-download
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil))
;; -OrgDownload

(use-package plantuml-mode
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

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state))

(use-package valign
  :after org
  :defer t
  :bind (([remap org-table-align] . valign-table))
  :hook (org-mode . valign-mode)
  )

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
