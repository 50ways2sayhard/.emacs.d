;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Apr 16 13:12:10 2022 (+0800)
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
(defvar +org-capture-file-note (concat +self/org-base-dir "notes.org"))
(defvar +org-capture-file-someday (concat +self/org-base-dir "someday.org"))
(defvar +org-capture-file-tickler (concat +self/org-base-dir "tickler.org"))
(defvar +org-capture-file-done (concat +self/org-base-dir "done.org"))
(defvar +org-capture-file-routine (concat +self/org-base-dir "routine.org"))

(defvar +org-files (mapcar (lambda (p) (expand-file-name p)) (list +org-capture-file-gtd
                                                              +org-capture-file-done
                                                              +org-capture-file-someday
                                                              +org-capture-file-note
                                                              +org-capture-file-routine)))

(use-package org
  :straight nil
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-update-cookies-h)
         (org-mode . (lambda ()
                       (unless (cl-member (buffer-file-name) +org-files :test 'equal)
                         (org-num-mode)))))
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
  (org-clock-persistence-insinuate)
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
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success)))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "INPROCESS(s)"  ; A task that is in progress
           "WAITING(w)"  ; Something is holding up this task; or it is paused
           "|"
           "DONE(d)"  ; Task successfully completed
           "CANCELED(c@)") ; Task was cancelled, aborted or is no longer applicable
          )
        )

  ;; HACK: fix folded org headings
  ;; https://github.com/minad/consult/issues/563#issuecomment-1186612641
  (defun org-show-entry-consult-a (fn &rest args)
    (when-let ((pos (apply fn args)))
      (when (derived-mode-p 'org-mode)
        (org-fold-show-entry))))
  (advice-add 'consult-outline :around #'org-show-entry-consult-a)

  (add-hook 'org-mode-hook (lambda ()
                             (show-paren-local-mode -1))
            (defface org-checkbox-done-text
              '((t (:strike-through t)))
              "Face for the text part of a checked org-mode checkbox.")

            (font-lock-add-keywords
             'org-mode
             `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                1 'org-checkbox-done-text prepend))
             'append))


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
  :straight (:host github :repo "minad/org-modern")
  :defer t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq
   org-modern-table t
   org-modern-block t
   org-modern-keyword t
   org-modern-todo nil ;;  TODO: no better way to define fine faces
   org-modern-timestamp t
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")


  (setq org-tags-column 0
        org-auto-align-tags nil)

  ;; avoid unneccesary calculations, I never need it.
  (defalias 'org-align-tags #'ignore)

  ;; Inspired by Ihor Radchenko’s code at: https://orgmode.org/list/87lfh745ch.fsf@localhost/
  (add-hook 'org-modern-mode-hook #'aj/org-set-tag-align-keywords)
  (defun aj/org-set-tag-align-keywords ()
    (add-to-list 'font-lock-extra-managed-props 'org-tag-aligned)
    (font-lock-add-keywords nil '((yant/org-align-tags t)) 'append))

  (defun aj/string-pixel-width (string &optional mode)
    "Calculate pixel width of STRING.
Optional MODE specifies major mode used for display."
    (let ((fra face-remapping-alist))
      (with-temp-buffer
        (with-silent-modifications
          (setf (buffer-string) string))
        (when (fboundp mode)
          (funcall mode)
          (font-lock-ensure))
        (setq-local face-remapping-alist fra)
        (if (get-buffer-window (current-buffer))
	          (car (window-text-pixel-size nil (line-beginning-position) (point)))
          (set-window-buffer nil (current-buffer))
          (car (window-text-pixel-size nil (line-beginning-position) (point)))))))


  (defun yant/org-align-tags (limit &optional force)
    "Align all the tags in org buffer."
    (save-match-data
      (when (eq major-mode 'org-mode)
        (let ((ellw (aj/string-pixel-width org-ellipsis)))
	        (while (re-search-forward "^\\*+ \\(.+?\\)\\([ \t]+\\)\\(:\\(?:[^ \n]+:\\)+\\)$" limit t)
	          (when (and (match-string 2)
		                   (or force
			                     (not (get-text-property (match-beginning 2) 'org-tag-aligned))))
	            (with-silent-modifications
                (put-text-property (match-beginning 2) (match-end 2) 'org-tag-aligned t)
	              (put-text-property
                 (if (>= 2 (- (match-end 2) (match-beginning 2)))
				             (match-beginning 2)
			             ;; multiple whitespaces may mean that we are in process of typing
			             (1+ (match-beginning 2)))
			           (match-end 2)
			           'display
			           `(space . (:align-to
                            (- right
						                   (,(+ 3 ;; no idea, but otherwise it is sometimes not enough
							                      ellw
                                    (if (match-beginning 3)
                                        (car (window-text-pixel-size nil (match-beginning 3) (match-end 3)))
                                      0))))))))))))))


  )


(defun +my/open-org-agenda ()
  "open org agenda in left window"
  (interactive)
  (org-agenda nil "n"))

;; -Notification only for mac os
(when *sys/mac*
  (add-hook '+my/first-input-hook
            (lambda ()
              (run-with-timer 5 nil
                              (lambda ()
                                (with-eval-after-load 'org
                                  (require 'appt)

                                  (setq appt-time-msg-list nil) ;; clear existing appt list
                                  (setq appt-display-interval '5) ;; warn every 5 minutes from t - appt-message-warning-time
                                  (setq
                                   appt-message-warning-time '15 ;; send first warning 15 minutes before appointment
                                   appt-display-mode-line nil ;; don't show in the modeline
                                   appt-display-format 'window) ;; pass warnings to the designated window function
                                  (setq appt-disp-window-function (function ct/appt-display-native))

                                  (appt-activate 1) ;; activate appointment notification
                                        ; (display-time) ;; Clock in modeline

                                  ;; brew install terminal-notifier
                                  (defun ct/send-notification (title msg)
                                    (let ((notifier-path (executable-find "terminal-notifier")))
                                      (start-process
                                       "Appointment Alert"
                                       nil
                                       notifier-path
                                       "-message" msg
                                       "-title" title
                                       "-sender" "org.gnu.Emacs"
                                       "-activate" "org.gnu.Emacs")))
                                  (defun ct/appt-display-native (min-to-app new-time msg)
                                    (ct/send-notification
                                     (format "Appointment in %s minutes" min-to-app) ; Title
                                     (format "%s" msg))) ; Message/detail text
                                  ;; Agenda-to-appointent hooks
                                  (org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
                                  (run-at-time nil 900 'org-agenda-to-appt) ;; update appt list hourly
                                  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
                                  ))))))
;; -Notification

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
