;;; init-dart.el ---
;;
;; Filename: init-dart.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Tue Jun  1 16:36:05 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 126
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

(eval-and-compile
  (require 'init-const)
  (require 'init-func))

(use-package dart-mode
  :mode ("\\.dart\\'")
  :hook ((dart-mode . (lambda ()
                        (setq-local lsp-enable-imenu t)
                        (setq-local lsp-diagnostics-provider :flymake)
                        (add-hook 'lsp-mode-hook
                                  (lambda ()
                                    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                                    (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
                        ))
         (eglot-managed-mode . (lambda ()
                                 (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
                                 (add-hook 'before-save-hook '+eglot-organize-imports nil t)
                                 ))
         )
  :config
  ;; (defun project-try-dart (dir)
  ;;   (let ((project (or (locate-dominating-file dir "pubspec.yaml")
  ;;                      (locate-dominating-file dir "BUILD"))))
  ;;     (if project ;;         (cons 'dart project)
  ;;       (cons 'transient dir))))
  ;; (add-hook 'project-find-functions #'project-try-dart)
  ;; (cl-defmethod project-roots ((project (head dart)))
  ;;   (list (cdr project)))

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
    (comint-check-proc (+my/flutter--process-name)))

  (defun +my/flutter-run-or-attach (mode)
    "Attach to a running Flutter application."
    (interactive (list (completing-read "Mode: " '("run" "attach") nil t)))

    (unless (+my/flutter--process-running-p)
      (let* ((project (+my/find-project-root))
             (process-name (+my/flutter--process-name))
             (buffer (get-buffer-create (+my/flutter--buffer-name))))
        (if (file-exists-p (concat project "lib/main.dart"))
            (cd project)
          (cd (concat project "example")))
        (start-process process-name (+my/flutter--buffer-name) "flutter" mode)
        (display-buffer (+my/flutter--buffer-name))
        )
      )
    )

  (defun +my/send-flutter-command (command)
    "Send a command to a running Flutter application."
    (let ((process (+my/flutter--process-name)))
      (if (eq (process-status process) 'run)
          (process-send-string process command)
        (call-interactively #'+my/flutter-run-or-attach))
      ))

  (defun +my/flutter-run-or-hot-reload ()
    "Hot reload the current Flutter application."
    (interactive)
    (+my/send-flutter-command "r"))

  (defun +my/flutter-run-or-hot-restart ()
    "Hot restart the current Flutter application."
    (interactive)
    (+my/send-flutter-command "R"))

  (defun +my/flutter-open-devtools ()
    "Open the Flutter DevTools."
    (interactive)
    (+my/send-flutter-command "v"))

  (defun +my/flutter-quit ()
    "Quit the Flutter application."
    (interactive)
    (when (+my/flutter--process-running-p)
      (+my/send-flutter-command "q")
      (display-buffer (+my/flutter--buffer-name))))

  (local-leader-def
    :keymaps 'dart-mode-map
    "r" '(+my/flutter-run-or-hot-reload :wk "Run or hot reload")
    "R" '(+my/flutter-run-or-hot-restart :wk "Run or hot restart")

    "v" '(+my/flutter-open-devtools :wk "Open devtools")
    "Q" '(+my/flutter-quit :wk "Quit application")

    "s" '(+my/flutter-run-or-attach :wk "Run or Attach")
    )
  )

(use-package lsp-dart
  :after lsp-mode
  :custom
  (lsp-dart-dap-flutter-hot-reload-on-save t)
  (lsp-dart-flutter-widget-guides nil)
  (lsp-dart-flutter-fringe-colors nil)
  (lsp-dart-outline nil)
  (lsp-dart-flutter-outline nil)
  (lsp-dart-closing-labels nil)
  (lsp-dart-main-code-lens nil)
  (lsp-dart-test-code-lens nil)
  )

(provide 'init-dart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
