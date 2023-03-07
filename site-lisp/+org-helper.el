;;; org-helper.el --- flutter-utils               -*- lexical-binding: t -*-
;;; package -- Summary
;;; Commentary:

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-fold)
(require 'org-cycle)

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Return the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                (match-string 1 x)
                              x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

(defun +org--insert-item (direction)
  (let* ((context
          (save-excursion
            (when (bolp)
              (back-to-indentation)
              (forward-char))
            (org-element-lineage
             (org-element-context)
             '(table table-row headline inlinetask item plain-list)
             t)))
         (type (org-element-type context)))
    (cond ((memq type '(item plain-list))
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (org-beginning-of-item)
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (save-match-data
               (pcase direction
                 (`below
                  (org-end-of-item)
                  (backward-char)
                  (end-of-line)
                  (if (and marker (string-match "\\([0-9]+\\)\\([).] *\\)" marker))
                      (let ((l (line-number-at-pos)))
                        (org-insert-item)
                        (when (= l (line-number-at-pos))
                          (org-next-item)
                          (org-end-of-line)))
                    (insert "\n" (make-string pad 32) (or marker ""))))
                 (`above
                  (org-beginning-of-item)
                  (if (and marker (string-match-p "[0-9]+[).]" marker))
                      (org-insert-item)
                    (insert (make-string pad 32) (or marker ""))
                    (save-excursion (insert "\n")))))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
             ('below (save-excursion (org-table-insert-row t))
                     (org-table-next-row))
             ('above (save-excursion (org-shiftmetadown))
                     (+org/table-previous-row))))

          ((let ((level (or (org-current-level) 1)))
             (pcase direction
               (`below
                (let (org-insert-heading-respect-content)
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert "\n" (make-string level ?*) " ")))
               (`above
                (org-back-to-heading)
                (insert (make-string level ?*) " ")
                (save-excursion (insert "\n"))))
             (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                         (todo-type (org-element-property :todo-type context)))
               (org-todo (cond ((eq todo-type 'done)
                                (car (+org-get-todo-keywords-for todo-keyword)))
                               (todo-keyword)
                               ('todo)))))))

    (when (org-invisible-p)
      (org-fold-show-hidden-entry))))

;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

;;;###autoload
(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))


;;;###autoload
(defun +org--refresh-inline-images-in-subtree ()
  "Refresh image previews in the current heading/tree."
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-cycle-force-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             (t
              (+org--refresh-inline-images-in-subtree)
              (org-clear-latex-preview)
              (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate))

      (`src-block
       (org-ctrl-c-ctrl-c))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--refresh-inline-images-in-subtree)
           (org-open-at-point))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_ (+org--refresh-inline-images-in-subtree)))))

;;;###autoload
(defun +org/archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
      (goto-char (line-beginning-position))
      (org-archive-subtree))))

;;;###autoload
(defun +my-org/mark-done ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-back-to-heading)
    (when-let* ((close-time (org-entry-get (point) "CLOSED"))
                (close-time (org-time-string-to-time close-time))
                (close-time (decode-time close-time))
                (close-time (list (decoded-time-month close-time) (decoded-time-day close-time) (decoded-time-year close-time))))
      (org-cut-subtree)
      (with-current-buffer (find-file-noselect +org-capture-file-diary)
        (+my/org-datetree-find-week-create close-time)
        (org-paste-subtree)
        (org-move-subtree-down)
        (org-shiftmetaright)
        (save-buffer)
        (kill-buffer)))))

(defun +my/org-datetree-find-week-create (d &optional keep-restriction)
  (setq-local org-datetree-base-level 1)
  (save-restriction
    (if (eq keep-restriction 'subtree-at-point)
	      (progn
	        (unless (org-at-heading-p) (error "Not at heading"))
	        (widen)
	        (org-narrow-to-subtree)
	        (setq-local org-datetree-base-level
		                  (org-get-valid-level (org-current-level) 1)))
      (unless keep-restriction (widen))
      ;; Support the old way of tree placement, using a property
      (let ((prop (org-find-property "WEEK_TREE")))
	      (when prop
	        (goto-char prop)
	        (setq-local org-datetree-base-level
		                  (org-get-valid-level (org-current-level) 1))
	        (org-narrow-to-subtree))))
    (goto-char (point-min))
    (require 'cal-iso)
    (require 'org-datetree)
    ;; WORKAROUND: this part is ugly
    (let* ((year (calendar-extract-year d))
	         (month (calendar-extract-month d))
	         (day (calendar-extract-day d))
	         (time (encode-time 0 0 0 day month year))
           (weekday (string-to-number (format-time-string "%w" time)))
           (sunday (encode-time 0 0 0 (- day weekday) month year))
           (saturday (encode-time 0 0 0 (+ day (- 6 weekday)) month year))
           (week (ceiling (/ (time-to-day-in-year sunday) 7.0)))
           (sunday_dec (decode-time sunday))
           (year (nth 5 sunday_dec))
           (month (nth 4 sunday_dec))
           (sunday_day (nth 3 sunday_dec))
           )
      (org-datetree--find-create
       "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
       year nil nil
       (format-time-string "%Y" time))
      (org-datetree--find-create
       "^\\*+[ \t]+%d-\\([01][0-9]\\) \\w+$"
       year month nil)
      (org-datetree--find-create
       "^\\*+[ \t]+W[0-5][0-9] (%d-%02d-\\([0-3][0-9]\\) - [0-2][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9])$"
       year month sunday_day
       (format "W%02d (%s - %s)" week
               (format-time-string "%Y-%m-%d" sunday)
               (format-time-string "%Y-%m-%d" saturday))))))

(provide '+org-helper)
;;; org-helper.el ends here
