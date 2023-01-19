;;; flutter-utils.el --- flutter-utils               -*- lexical-binding: t -*-
;;; package -- Summary
;;; Commentary:

;;; Code:

(require 'comint)
(require 'subr-x)
(require 'project)

(defgroup flutter-utils nil
  "Customize flutter-utils."
  :prefix "flutter-utils-"
  :group 'flutter-utils)

(defcustom flutter-utils-pub-hosted-url "https://pub.dev"
  "Pub downloads dependencies site."
  :group 'flutter-utils
  :type 'string)


(defvar flutter-utils--app-id-alist '())
(defvar flutter-utils--device-alist '())

(defun flutter-utils-find-project-root()
  "Return current project or nil if not in a project."
  (interactive)
  (let ((project (project-current)))
    (if project
        (project-root project) ;;  HACK: original repo breaks here
      nil)))

(defun flutter-utils--process-name ()
  "Return the name of the flutter process."
  (let ((project-name (flutter-utils-find-project-root)))
    (if project-name
        (concat "flutter-" project-name)
      nil)))

(defun flutter-utils--buffer-name()
  "Return the name of the flutter buffer."
  (concat "*Flutter Daemon - " (flutter-utils-find-project-root) "*"))

(defun flutter-utils--process-running-p ()
  "Return t if the flutter process is running."
  (get-process (flutter-utils--process-name)))

(defun flutter-utils--process-filter (output)
  "Capture app-id from OUTPUT."
  (let ((output-splitted (split-string (string-trim output) " ")))
    (when (string-equal (car output-splitted) "flutter")
      (let ((app-id (string-join (nthcdr 3 output-splitted) " ")))
        (message "capture app-id: %s" app-id)
        (unless (member app-id flutter-utils--app-id-alist)
          (add-to-list 'flutter-utils--app-id-alist app-id))))))

(defun flutter-utils--command (mode &optional app-id)
  "Create flutter command.
MODE is attach or run.  APP-ID is the app-id to attach to."
  (if app-id
      (list "flutter" mode "--app-id" app-id)
    (list "flutter" mode)))

(defun flutter-utils--sentinel (_ event)
  "Sentinel for flutter process.EVENT is the event that triggered the sentinel."
  (message "[Flutter] event: %s" event)
  (when (string-prefix-p "finished" event)
    (kill-buffer (flutter-utils--buffer-name))))

(defun flutter-utils-run-or-attach ()
  "Interactively run or attach to a running flutter app."
  (interactive)
  (if (flutter-utils--process-running-p)
      (message "Flutter Process of project %s is already running." (flutter-utils-find-project-root))
    (progn
      (let ((mode (completing-read "Mode: " '("attach" "run") nil t)))
        (if (and (string-equal mode "attach") (> (length flutter-utils--app-id-alist) 0))
            (flutter-utils--run-or-attach
             mode (completing-read "App-id: " flutter-utils--app-id-alist nil t))
          (flutter-utils--run-or-attach mode)
          )))))

(defun flutter-utils--run-or-attach (mode &optional app-id)
  "Run or attach to a running flutter app.
MODE is attach or run.APP-ID is the app-id to attach to."
  ;; (interactive (list (completing-read "Mode: " '("run" "attach") nil t)))
  (unless (flutter-utils--process-running-p)
    (let* ((project (flutter-utils-find-project-root))
           (process-name (flutter-utils--process-name))
           (buffer (get-buffer-create (flutter-utils--buffer-name)))
           (temp (mapcar 'concat process-environment))
           (process-environment (setenv-internal temp "PUB_HOSTED_URL" flutter-utils-pub-hosted-url t)))
      (if (file-exists-p (concat project "lib/main.dart"))
          (cd project)
        (cd (concat project "example")))
      (make-process
       :name process-name
       :buffer buffer
       :command (flutter-utils--command mode app-id)
       :coding 'utf-8
       ;; :filter 'flutter-utils--process-filter
       :sentinel 'flutter-utils--sentinel
       :noquery t)
      (with-current-buffer buffer
        (unless (derived-mode-p 'comint-mode)
          (comint-mode)
          (setq-local comint-output-filter-functions #'flutter-utils--process-filter)))
      (cd (file-name-directory buffer-file-name))
      (display-buffer buffer))))

(defun flutter-utils--send (command)
  "Send a command to a running Flutter application.COMMAND is the command to send."
  (if (flutter-utils--process-running-p)
      (process-send-string (flutter-utils--process-name) command)
    (call-interactively #'flutter-utils-run-or-attach)))

;;;###autoload
(defun flutter-utils-run-or-hot-reload ()
  "Hot reload the current Flutter application."
  (interactive)
  (flutter-utils--send "r"))

;;;###autoload
(defun flutter-utils-run-or-hot-restart ()
  "Hot restart the current Flutter application."
  (interactive)
  (flutter-utils--send "R"))

;;;###autoload
(defun flutter-utils-open-devtools ()
  "Open the Flutter DevTools."
  (interactive)
  (flutter-utils--send "v"))

;;;###autoload
(defun flutter-utils-quit ()
  "Quit the Flutter application."
  (interactive)
  (when (flutter-utils--process-running-p)
    (flutter-utils--send "q")
    (display-buffer (flutter-utils--buffer-name))))

;;;###autoload
(defun flutter-utils-pub-get ()
  "Run pub get."
  (interactive)
  ;; (start-process "flutter-pub-get" "*Flutter Pub Get*" "flutter" "pub" "get")
  (cd (flutter-utils-find-project-root))
  (let* ((temp (mapcar 'concat process-environment))
         (process-environment (setenv-internal temp "PUB_HOSTED_URL" flutter-utils-pub-hosted-url t)))
    (make-process :name "flutter-pub-get"
                  :buffer "*Flutter Pub Get*"
                  :command '("flutter" "pub" "get")
                  :coding 'utf-8
                  :noquery t
                  :sentinel (lambda (_ event)
                              (message "[Flutter] run pub get: %s" event)
                              (when (string-prefix-p "finished" event)
                                (kill-buffer "*Flutter Pub Get*")))))
  (cd (file-name-directory buffer-file-name))
  (display-buffer "*Flutter Pub Get*"))

(provide 'flutter-utils)
;;; flutter-utils.el ends here
