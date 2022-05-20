;;; +flutter.el --- summary -*- lexical-binding: t -*-

;; Author: John
;; Maintainer: John
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'init-func)
(require 'comint)
(require 'subr-x)

(defvar +my/flutter--app-id-alist '())

(defvar +my/flutter-pub-host "https://pub.dev")

(defun +my/flutter--process-name ()
  "Return the name of the flutter process."
  (let ((project-name (+my/find-project-root)))
    (if project-name
        (concat "flutter-" project-name)
      nil)))

(defun +my/flutter--buffer-name()
  "Return the name of the flutter buffer."
  (concat "*Flutter Daemon - " (+my/find-project-root) "*"))

(defun +my/flutter--process-running-p ()
  "Return t if the flutter process is running."
  (get-process (+my/flutter--process-name)))

(defun +my/flutter--process-filter (output)
  "Capture app-id from OUTPUT."
  (let ((output-splitted (split-string (string-trim output) " ")))
    (when (string-equal (car output-splitted) "flutter")
      (let ((app-id (string-join (nthcdr 3 output-splitted) " ")))
        (message "capture app-id: %s" app-id)
        (unless (member app-id +my/flutter--app-id-alist)
          (add-to-list '+my/flutter--app-id-alist app-id)))))
  )

(defun +my/flutter--command (mode &optional app-id)
  "Create flutter command.MODE is 'attach' or 'run'.APP-ID is the app-id to attach to."
  (if app-id
      (list "flutter" mode "--app-id" app-id)
    (list "flutter" mode)))

(defun +my/flutter--sentinel (_ event)
  "Sentinel for flutter process.EVENT is the event that triggered the sentinel."
  (message "[Flutter] event: %s" event)
  (kill-buffer (+my/flutter--buffer-name)))

(defun +my/flutter-run-or-attach ()
  "Interactively run or attach to a running flutter app."
  (interactive)
  (if (+my/flutter--process-running-p)
      (message "Flutter Process of project %s is already running." (+my/find-project-root))
    (progn
      (let ((mode (completing-read "Mode: " '("attach" "run") nil t)))
        (if (and (string-equal mode "attach") (> (length +my/flutter--app-id-alist) 0))
            (+my/flutter--run-or-attach
             mode (completing-read "App-id: " +my/flutter--app-id-alist nil t))
          (+my/flutter--run-or-attach mode)
          )))))

(defun +my/flutter--run-or-attach (mode &optional app-id)
  "Run or attach to a running flutter app.MODE is 'attach' or 'run'.APP-ID is the app-id to attach to."
  ;; (interactive (list (completing-read "Mode: " '("run" "attach") nil t)))
  (unless (+my/flutter--process-running-p)
    (let* ((project (+my/find-project-root))
           (process-name (+my/flutter--process-name))
           (buffer (get-buffer-create (+my/flutter--buffer-name)))
           (command (+my/flutter--command mode app-id)))
      (if (file-exists-p (concat project "lib/main.dart"))
          (cd project)
        (cd (concat project "example")))
      (make-process
       :name process-name
       :buffer buffer
       :command (+my/flutter--command mode app-id)
       :coding 'utf-8
       ;; :filter '+my/flutter--process-filter
       :sentinel '+my/flutter--sentinel
       :noquery t)
      (with-current-buffer buffer
        (unless (derived-mode-p 'comint-mode)
          (comint-mode)
          (setq-local comint-output-filter-functions #'+my/flutter--process-filter)))
      (cd (file-name-directory buffer-file-name))
      (display-buffer buffer))))

(defun +my/flutter--send (command)
  "Send a command to a running Flutter application.COMMAND is the command to send."
  (if (+my/flutter--process-running-p)
      (process-send-string (+my/flutter--process-name) command)
    (call-interactively #'+my/flutter-run-or-attach)))

(defun +my/flutter-run-or-hot-reload ()
  "Hot reload the current Flutter application."
  (interactive)
  (+my/flutter--send "r"))

(defun +my/flutter-run-or-hot-restart ()
  "Hot restart the current Flutter application."
  (interactive)
  (+my/flutter--send "R"))

(defun +my/flutter-open-devtools ()
  "Open the Flutter DevTools."
  (interactive)
  (+my/flutter--send "v"))

(defun +my/flutter-quit ()
  "Quit the Flutter application."
  (interactive)
  (when (+my/flutter--process-running-p)
    (+my/flutter--send "q")
    (display-buffer (+my/flutter--buffer-name))))

(defun +my/flutter-pub-get ()
  "Run pub get."
  (interactive)
  ;; (start-process "flutter-pub-get" "*Flutter Pub Get*" "flutter" "pub" "get")
  (cd (+my/find-project-root))
  (let* ((temp (mapcar 'concat process-environment))
         (process-environment (setenv-internal temp "PUB_HOSTED_URL" +my/flutter-pub-host t)))
    (make-process :name "flutter-pub-get"
                  :buffer "*Flutter Pub Get*"
                  :command '("flutter" "pub" "get")
                  :coding 'utf-8
                  :noquery t
                  :sentinel (lambda (process event)
                              (message "[Flutter] run pub get: %s" event)
                              (kill-buffer "*Flutter Pub Get*"))
                  )
    )
  (cd (file-name-directory buffer-file-name))
  (display-buffer "*Flutter Pub Get*")
  )


(provide 'prog/+flutter)

;;; +flutter.el ends here
