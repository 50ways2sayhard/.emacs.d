;;; +popterm.el --- Intelligent popterm management  -*- lexical-binding: t -*-
;;; Commentary:
;; DWIM toggle and registration system for popterm.
;;; Code:

(require 'popterm)

(defcustom +popterm-shortcuts nil
  "Alist of popterm shortcuts.
Each entry is (NAME . DEFAULT-DIRECTORY) where NAME is the
instance name and DEFAULT-DIRECTORY is the directory to cd into."
  :type '(alist :key-type string :value-type directory)
  :group 'popterm)

(defun +popterm--instance-name (buf)
  "Extract the instance name from popterm buffer BUF.
Returns the buffer-local `popterm--buffer-instance-name' or nil."
  (buffer-local-value 'popterm--buffer-instance-name buf))

(defun +popterm--build-candidates ()
  "Build completing-read candidates.
Returns alist of (DISPLAY . (:name NAME :dir DIR-OR-NIL :open BOOL)).
Combines open popterm buffers and registered shortcuts.
Open buffers that are also shortcuts show their registered directory."
  (let ((open-bufs (let ((popterm-scope nil)) (popterm--buffer-list)))
        (open-names '())
        result)
    ;; Collect open buffers
    (dolist (buf open-bufs)
      (let* ((inst (+popterm--instance-name buf))
             (key (or inst (buffer-name buf)))
             (shortcut (assoc key +popterm-shortcuts))
             (dir (cdr shortcut))
             (display (if shortcut
                         (format "%s (open) → %s" key dir)
                       (format "%s (open)" key))))
        (push key open-names)
        (push (cons display (list :name key :dir dir :open t)) result)))
    ;; Add shortcuts not yet open
    (dolist (sc +popterm-shortcuts)
      (unless (member (car sc) open-names)
        (let ((display (format "%s → %s" (car sc) (cdr sc))))
          (push (cons display (list :name (car sc) :dir (cdr sc) :open nil)) result))))
    (nreverse result)))

(defun +popterm--send-cd-to-dir (term-buf dir)
  "Send cd command to TERM-BUF to change to DIR."
  (let ((expanded (expand-file-name dir)))
    (when (file-directory-p expanded)
      (with-temp-buffer
        (setq default-directory expanded)
        (popterm--send-cd term-buf (current-buffer))))))

;;;###autoload
(defun +popterm-toggle-dwim (&optional name)
  "Intelligent popterm toggle with DWIM behavior.
When NAME is provided, behave like `popterm-toggle' with that name.
When NAME is nil, show completing-read with:
  - Already opened popterm buffers (respecting scope)
  - Registered shortcuts from `+popterm-shortcuts'
  - Option to input a new name
Respects `popterm-display-method' for all operations."
  (interactive)
  (if (and name (not (string-empty-p name)))
      (popterm-toggle name)
    (let* ((entries (+popterm--build-candidates))
           (candidates (mapcar #'car entries))
           (selection (completing-read "Popterm: " candidates nil nil))
           (entry (cdr (assoc selection entries))))
      (if entry
          ;; Known entry (open buffer or registered shortcut)
          (let* ((inst-name (plist-get entry :name))
                 (dir (plist-get entry :dir))
                 (buf (popterm--get-or-create inst-name))
                 (popterm-auto-cd nil))
            (popterm--show buf)
            (when dir
              (+popterm--send-cd-to-dir buf dir)))
        ;; New name typed by user
        (let* ((new-name (string-trim selection))
               (new-name (if (string-empty-p new-name) "default" new-name))
               (buf (popterm--get-or-create new-name))
               (popterm-posframe-width-ratio 0.95)
               (popterm-posframe-height-ratio 0.85)
               (popterm-auto-cd nil))
          (popterm--show buf)
          (+popterm--send-cd-to-dir buf "~"))))))

;;;###autoload
(defun +popterm-register-shortcut (&optional name dir)
  "Register a popterm shortcut mapping NAME to DIR.
When called interactively without arguments, prompt for both.
Persists the registration via `customize-save-variable'."
  (interactive)
  (let ((name (or name (read-string "Shortcut name: ")))
        (dir (or dir (read-directory-name "Default directory: " "~/"))))
    (when (or (null name) (string-empty-p name))
      (user-error "Name cannot be empty"))
    (setq dir (expand-file-name dir))
    (unless (file-directory-p dir)
      (user-error "Directory does not exist: %s" dir))
    ;; Update the alist: replace existing or add new
    (let ((existing (assoc name +popterm-shortcuts)))
      (if existing
          (setcdr existing dir)
        (push (cons name dir) +popterm-shortcuts)))
    (customize-save-variable '+popterm-shortcuts +popterm-shortcuts)
    (message "Registered popterm shortcut: %s → %s" name dir)))

(provide '+popterm)
;;; +popterm.el ends here
