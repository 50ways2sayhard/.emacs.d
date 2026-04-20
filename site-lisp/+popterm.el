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

(defun +popterm--pick-via-completion ()
  "Prompt user via `completing-read' and show the chosen popterm.
Candidates include open popterm buffers (all scopes) and registered
shortcuts; user can also type a new name."
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
        (setq-local switch-to-buffer-obey-display-actions t)
        (+popterm--send-cd-to-dir buf "~")))))

;;;###autoload
(defun +popterm-toggle-dwim (&optional arg)
  "Intelligent popterm toggle with DWIM behavior.

Behavior:
  - Called from within a popterm buffer: close it (`popterm-return').
  - With prefix ARG (C-u): always prompt via `completing-read'.
  - Otherwise: if any popterm buffer exists in current scope, show the
    most recently accessed one; if none exists, prompt via
    `completing-read'.

The completion list contains open popterm buffers (all scopes) plus
registered shortcuts from `+popterm-shortcuts', and accepts a new name.
Respects `popterm-display-method' for all operations."
  (interactive "P")
  (cond
   ;; Called from within a popterm buffer → close it
   ((popterm--buffer-p (current-buffer))
    (popterm-return))
   ;; C-u prefix → force completing-read
   (arg
    (+popterm--pick-via-completion))
   ;; No prefix → use most recently accessed popterm if any exists
   (t
    (let ((recent (car (let ((popterm-scope nil))
                         (popterm--buffer-list)))))
      (cond
       ;; Most recent popterm exists → show it
       (recent
        (popterm--show recent))
       ;; No open popterm and no registered shortcuts → open default
       ((null +popterm-shortcuts)
        (let ((buf (popterm--get-or-create "default"))
              (popterm-auto-cd nil))
          (popterm--show buf)
          (+popterm--send-cd-to-dir buf "~")))
       ;; Otherwise → prompt
       (t
        (+popterm--pick-via-completion)))))))

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

;; ── Auto-hide when switching to a non-popterm buffer ──────────────────────────

(defvar +popterm--auto-hide-inhibit nil
  "Re-entry guard for `+popterm--auto-hide-on-buffer-change'.")

(defun +popterm--other-buffer-displayed-p ()
  "Return non-nil when selected window shows a non-popterm buffer
on the parent frame while popterm is visible."
  (and (popterm--visible-p)
       (not (active-minibuffer-window))
       (not (frame-parameter (selected-frame) 'parent-frame))
       (not (eq (selected-frame) (bound-and-true-p popterm--frame)))
       (let ((buf (window-buffer (selected-window))))
         (and (buffer-live-p buf)
              (not (popterm--buffer-p buf))
              (not (minibufferp buf))))))

(defun +popterm--auto-hide-on-buffer-change (&rest _)
  "Hide popterm when user switches to a non-popterm buffer."
  (unless +popterm--auto-hide-inhibit
    (let ((+popterm--auto-hide-inhibit t))
      (when (+popterm--other-buffer-displayed-p)
        (popterm--hide)))))

;;;###autoload
(define-minor-mode +popterm-auto-hide-mode
  "Globally hide popterm posframe whenever a non-popterm buffer is
displayed on the parent frame.  Opposite of popterm's built-in
focus-guard which reclaims focus back to the posframe."
  :global t
  :group 'popterm
  (if +popterm-auto-hide-mode
      (progn
        (add-hook 'window-buffer-change-functions
                  #'+popterm--auto-hide-on-buffer-change)
        (add-hook 'window-selection-change-functions
                  #'+popterm--auto-hide-on-buffer-change))
    (remove-hook 'window-buffer-change-functions
                 #'+popterm--auto-hide-on-buffer-change)
    (remove-hook 'window-selection-change-functions
                 #'+popterm--auto-hide-on-buffer-change)))

(provide '+popterm)
;;; +popterm.el ends here
