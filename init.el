;;; init.el --- user-init-file               -*- lexical-binding: t -*-
;;; Early birds

;;; Code:
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory))))

;; (eval-and-compile ; `borg'
;;   (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
;;   (require 'borg)
;;   (borg-initialize))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (setq borg-compile-function #'borg-byte+native-compile-async)
  (add-to-list 'load-path (expand-file-name "lib/names" user-emacs-directory))
  (require 'names)

  (defun lld-collect-autoloads (file)
    "insert all enabled drone's autoloads file to a single file."
    (make-directory (file-name-directory file) 'parents)

    ;; cleanup obsolete autoloads file
    (dolist (f (directory-files single-autoload-path t "autoload-[0-9]+-[0-9]+\\.elc?\\'"))
      (unless (string= file f)
        (delete-file f)))

    (message "Generating single big autoload file.")
    (condition-case-unless-debug e
        (with-temp-file file
          (setq-local coding-system-for-write 'utf-8)
          (let ((standard-output (current-buffer))
                (print-quoted t)
                (print-level nil)
                (print-length nil)
                (home (expand-file-name "~"))
                path-list
                theme-path-list
                drones-path
                auto)
            (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                    ";; This file is generated from enabled drones.\n")

            ;; replace absolute path to ~
            (dolist (p load-path)
              ;; collect all drone's load-path
              (when (string-prefix-p (expand-file-name user-emacs-directory) (expand-file-name p))
                (push p drones-path))

              (if (string-prefix-p home p)
                  (push (concat "~" (string-remove-prefix home p)) path-list)
                (push p path-list)))

            (dolist (p custom-theme-load-path)
              (if (and (stringp p)
                       (string-prefix-p home p))
                  (push (concat "~" (string-remove-prefix home p)) theme-path-list)
                (push p theme-path-list)))

            (prin1 `(set `load-path ',(nreverse path-list)))
            (insert "\n")
            (print `(set `custom-theme-load-path ',(nreverse theme-path-list)))
            (insert "\n")

            ;; insert all drone's autoloads.el to this file
            (dolist (p drones-path)
              (when (file-exists-p p)
                (setq auto (car (directory-files p t ".*-autoloads.el\\'")))
                (when (and auto
                           (file-exists-p auto))
                  (insert-file-contents auto))))
            ;; remove all #$ load code
            (goto-char (point-min))
            (while (re-search-forward "\(add-to-list 'load-path.*#$.*\n" nil t)
              (replace-match ""))

            ;; write local variables region
            (goto-char (point-max))
            (insert  "\n"
                     "\n;; Local Variables:"
                     "\n;; version-control: never"
                     "\n;; no-update-autoloads: t"
                     "\n;; End:"
                     ))
          t)
      (error (delete-file file)
             (signal 'collect-autoload-error (list file e)))))

  (defvar single-autoload-path (concat user-emacs-directory "etc/borg/autoload/") "single autoload file.")
  (let ((file (concat single-autoload-path
                      "autoload-"
                      (format-time-string
                       "%+4Y%m%d-%H%M%S"
                       (file-attribute-modification-time
                        (file-attributes (concat user-emacs-directory ".gitmodules"))))
                      ".el")))
    (if (file-exists-p file)
        (load file nil t)
      (require 'borg)
      (borg-initialize)
      (lld-collect-autoloads file))))

;; :after-call for use-package
(defvar +use-package--deferred-pkgs '(t))
(defun use-package-handler/:after-call (name _keyword hooks rest state)
  "Add keyword `:after-call' to `use-package'.
The purpose of this keyword is to expand the lazy-loading
capabilities of `use-package'.  Consult `use-package-concat' and
`use-package-process-keywords' for documentations of NAME, HOOKS,
REST and STATE."
  (if (plist-get state :demand)
      (use-package-process-keywords name rest state)
    (let ((fn (make-symbol (format "grandview--after-call-%s-h" name))))
      (use-package-concat
       `((fset ',fn
               (lambda (&rest _)
                 (condition-case e
                     (let ((default-directory user-emacs-directory))
                       (require ',name))
                   ((debug error)
                    (message "Failed to load deferred package %s: %s" ',name e)))
                 (when-let (deferral-list (assq ',name +use-package--deferred-pkgs))
                   (dolist (hook (cdr deferral-list))
                     (advice-remove hook #',fn)
                     (remove-hook hook #',fn))
                   (setq +use-package--deferred-pkgs
                         (delq deferral-list +use-package--deferred-pkgs))
                   (unintern ',fn nil)))))
       (cl-loop for hook in hooks
                collect (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                            `(add-hook ',hook #',fn)
                          `(advice-add #',hook :before #',fn)))
       `((unless (assq ',name +use-package--deferred-pkgs)
           (push '(,name) +use-package--deferred-pkgs))
         (nconc (assq ',name +use-package--deferred-pkgs)
                '(,@hooks)))
       (use-package-process-keywords name rest state)))))
(require 'use-package-core)
(push :after-call use-package-deferring-keywords)
(setq use-package-keywords (use-package-list-insert :after-call use-package-keywords :after))
(defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)

(use-package borg
  :commands (borg-assimilate borg-insert-update-message))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash
  :defer t
  :config (global-dash-fontify-mode))
(use-package eieio)

(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Long tail

(use-package diff-hl
  :defer t
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((find-file . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (dolist (hook '(conf-mode-hook prog-mode-hook))
    (add-hook hook #'diff-hl-update))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (setq diff-hl-draw-borders nil))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (with-eval-after-load 'general
    (general-define-key :states '(normal)
                        :keymaps 'dired-mode-map
                        "l" 'dired-find-alternate-file
                        "h"  'dired-up-directory)
    )
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  (setq dired-open-extensions
        (mapcar (lambda (ext)
                  (cons ext "open")) '("pdf" "doc" "docx" "ppt" "pptx")))
  )

(use-package eldoc
  :defer t
  :commands (eldoc)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :commands (magit-open-repo magit-add-section-hook)
  :config
  (setq magit-display-buffer-function #'+magit-display-buffer-fn)
  (magit-auto-revert-mode -1)
  (setq magit-diff-refine-hunk (quote all))

  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (if (string-match "^http" url)
                        url
                      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                                "https://\\2/\\3"
                                                url)))
        (message "opening repo %s" url))))

  (defun aborn/simple-git-commit-push ()
    "Simple commit current git project and push to its upstream."
    (interactive)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (magit-stage-modified)
    (magit-diff-staged)
    (setq msg (read-string "Commit Message: "))
    (when (length= msg 0)
      (setq msg (format-time-string "commit by magit in emacs@%Y-%m-%d %H:%M:%S"
                                    (current-time))))
    (magit-call-git "commit" "-m" msg)
    (when (magit-get "remote" "origin" "url")
      (magit-push-current-to-upstream nil)
      (message "now do async push to %s" (magit-get "remote" "origin" "url")))
    (magit-mode-bury-buffer))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen 'overlay
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t))

(use-package prog-mode
  :defer t
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook 'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config
  (recentf-mode)
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:")
  (setq recentf-auto-cleanup "05:00am")
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook 'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

;;; Tequila worms

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()

              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time)))) t)
  (add-hook 'window-setup-hook
            (lambda ()
              (+my/open-org-agenda)) t)
  )

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

(defvar +my/first-input-hook nil)
(defun +my/first-input-hook-fun ()
  "Hook for first input."
  (when +my/first-input-hook
    (run-hooks '+my/first-input-hook)
    (setq +my/first-input-hook nil))
  (remove-hook 'pre-command-hook '+my/first-input-hook-fun))
(add-hook 'pre-command-hook '+my/first-input-hook-fun)

(let ((my-env-file (concat user-emacs-directory "env")))
  (when (and (or (display-graphic-p)
                 (daemonp))
             (file-exists-p my-env-file))
    (if (not (file-readable-p my-env-file))
        (unless noerror
          (signal 'file-error (list "Couldn't read envvar file" my-env-file)))
      (let (envvars environment)
        (with-temp-buffer
          (save-excursion
            (insert "\n")
            (insert-file-contents my-env-file))
          (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
            (push (match-string 1) envvars)
            (push (buffer-substring
                   (match-beginning 1)
                   (1- (or (save-excursion
                             (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                               (line-beginning-position)))
                           (point-max))))
                  environment)))
        (when environment
          (setq process-environment
                (append (nreverse environment) process-environment)
                exec-path
                (if (member "PATH" envvars)
                    (append (split-string (getenv "PATH") path-separator t)
                            (list exec-directory))
                  exec-path)
                shell-file-name
                (if (member "SHELL" envvars)
                    (or (getenv "SHELL") shell-file-name)
                  shell-file-name))
          envvars)))))

(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")


;; indentation
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 2)
(setq-default js-switch-indent-offset 2)
(add-hook 'after-change-major-mode-hook
          (lambda () (if (equal electric-indent-mode 't)
                    (when (derived-mode-p 'text-mode)
                      (electric-indent-mode -1))
                  (electric-indent-mode 1))))


;; When buffer is closed, saves the cursor location
(save-place-mode 1)

(setq-default create-lockfiles nil
              make-backup-files nil)
(setq create-lockfiles nil
      make-backup-files nil)

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t)

(use-package gcmh
  :defer t
  :hook (emacs-startup . gcmh-mode)
  :diminish
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

(windmove-default-keybindings 'meta)

(fset 'yes-or-no-p 'y-or-n-p)

(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(setq inhibit-startup-screen t
      initial-major-mode 'text-mode)

(setq display-line-numbers-type 'relative)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)
(custom-set-variables '(x-select-enable-clipboard t))
(setq blink-cursor-mode nil)
(setq word-wrap t
      word-wrap-by-category t
      require-final-newline t)
(setq split-width-threshold 0
      split-height-threshold nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq redisplay-skip-fontification-on-input t)

;; Vertical Scroll
(setq scroll-step 1
      ;; scroll-margin 0
      ;; scroll-conservatively 100000
      auto-window-vscroll t
      scroll-preserve-screen-position 'always)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-progressive-speed t)
(global-set-key (kbd "<C-wheel-down>") nil)
(global-set-key (kbd "<C-wheel-up>") nil)

(pixel-scroll-precision-mode)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;;;###autoload
(defun +open-configuration-folder ()
  "Open configuration folder."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

;;;###autoload
(defun +my-rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory buffer-file-name))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      ;; (set-visited-file-name file-to)
      ;; (rename-buffer file-to)
      ;; (save-buffer)
      (kill-buffer)
      (find-file file-to))))

;;;###autoload
(defun +my-delete-file ()
  "Put current buffer file to top."
  (interactive)
  (delete-file
   (read-file-name "Delete: " default-directory buffer-file-name))
  (unless (file-exists-p (buffer-file-name))
    (kill-current-buffer)))

;;;###autoload
(defun +my-imenu (args)
  "Call 'consult-outline' in 'org-mode' else 'consult-imenu'."
  (interactive "P")
  (if (derived-mode-p 'org-mode)
      (consult-outline)
    (if (boundp 'lsp-mode)
        (if (and lsp-mode lsp-enable-imenu (not lsp-use-plists))
            (consult-lsp-file-symbols args)
          (consult-imenu))
      (consult-imenu)
      )
    )
  )

;;;###autoload
(defun +default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun +default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

(defun +complete--get-meta (setting)
  "Get metadata SETTING from completion table."
  (completion-metadata-get
   (condition-case-unless-debug err
       (completion-metadata (minibuffer-contents)
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
     (error (message (error-message-string err)) nil))
   setting))


;;;###autoload
(defun open-in-external-app ()
  "TODO: only for macos now."
  (interactive)
  (let ((candidate (vertico--candidate)))
    (when (eq (+complete--get-meta 'category) 'file)
      (shell-command (concat "open " candidate))
      (abort-recursive-edit))))


;;;###autoload
(defun +my/find-project-root()
  (interactive)
  (let ((project (project-current)))
    (if project
        (project-root project) ;;  HACK: original repo breaks here
      nil)))


;;;###autoload
(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

;;;###autoload
(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
               (regexp-opt (x-defined-colors) 'words))
      (0 (let ((colour (match-string-no-properties 0)))
           (put-text-property
            (match-beginning 0) (match-end 0)
            'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                      "white" "black"))
                    (:background ,colour)))))))))


;;;###autoload
(defun +my/google-it (&optional word)
  "Google it."
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  (thing-at-point 'symbol))))
  (browse-url (concat "https://www.google.com/search?q=" word)))

(defun complete-path-at-point+ ()
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn
                   (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table))))

(use-package evil
  :hook (after-init . evil-mode)
  :demand t
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-want-keybinding nil
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo
        evil-want-C-w-delete nil
        evil-want-fine-undo t
        )

  :config
  (setcdr evil-insert-state-map nil)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (put 'evil-define-key* 'lisp-indent-function 'defun)
  (dolist (mode '(color-rg-mode smerge-mode vterm-mode git-timemachine-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Allows you to click buttons without initiating a selection
  (define-key evil-motion-state-map [down-mouse-1] nil)

  (with-eval-after-load 'general
    (general-define-key :keymaps 'evil-window-map
                        "C-h" 'evil-window-left
                        "C-j" 'evil-window-down
                        "C-k" 'evil-window-up
                        "C-l" 'evil-window-right)
    )

  (add-hook 'after-change-major-mode-hook #'(lambda ()
                                              (when (or (derived-mode-p 'fundamental-mode)
                                                        (derived-mode-p 'text-mode)
                                                        (derived-mode-p 'snippet-mode))
                                                (setq-local evil-auto-indent nil))))

  )

(use-package evil-collection
  :defer nil
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (require 'evil-collection)
  (delete 'corfu evil-collection-mode-list)
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :commands  (evil-escape-pre-command-hook evil-escape)
  :hook (+my/first-input-hook . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp))

(use-package evil-anzu
  :after evil
  :after-call evil-ex-search-next
  :config
  (global-anzu-mode)
  (add-hook 'evil-insert-state-entry-hook #'evil-ex-nohighlight))

(use-package evil-indent-plus
  :after evil
  :hook (+my/first-input . evil-indent-plus-default-bindings))

(use-package evil-embrace
  :after evil
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((c++-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil-embrace-angle-bracket-modes-hook-h)
  :config
;;;###autoload
  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

;;;###autoload
  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

;;;###autoload
  (defun +evil--embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

;;;###autoload
  (defun +evil--embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

;;;###autoload
  (defun +evil--embrace-angle-brackets ()
    "Type/generic angle brackets."
    (cons (format "%s<" (or (read-string "") ""))
          ">"))


  (setq evil-embrace-show-help-p nil)

  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))

  (defun +evil-embrace-latex-mode-hook-h ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{\" \"}" #'+evil--embrace-latex))

  (defun +evil-embrace-lisp-mode-hook-h ()
    ;; Avoid `embrace-add-pair-regexp' because it would overwrite the default
    ;; `f' rule, which we want for other modes
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ \"
                                   :right-regexp \")"))
          embrace--pairs-list))



  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair ?> "<" ">"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))

(use-package evil-nerd-commenter
  :after evil
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

(use-package evil-snipe
  :after evil
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-traces
  :after evil-ex
  :config
  (evil-traces-mode))

(use-package evil-visualstar
  :after evil
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

(use-package general
  :commands (leader-def local-leader-def)
  :config
  (general-create-definer leader-def
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix "SPC"
    )
  (general-create-definer local-leader-def
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix ",")

  (general-create-definer tab-def
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix "C-s")

  (tab-def
    "" nil
    "c" '(tab-new :wk "New")
    "r" '(tab-bar-rename-tab :wk "Rename")
    "d" '(tab-bar-close-tab :wk "Close")
    "s" '(tab-bar-select-tab-by-name :wk "Select"))

  ;; evil mode
  (evil-define-key 'normal 'global
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    ;; "K" 'lsp-ui-doc-glance
    ;; Comment
    "gcc" 'evilnc-comment-or-uncomment-lines
    "gcC" 'evilnc-comment-or-uncomment-to-the-line
    "gcr" 'comment-or-uncomment-region

    ;; goto
    "gd" 'xref-find-definitions
    "gD" 'xref-find-definitions-other-window
    "gI" 'xref-find-implementations

    "/" 'consult-line-symbol-at-point
    "'" 'noct-consult-ripgrep-or-line)

  (evil-ex-define-cmd "W" 'evil-write)
  (general-def "<escape>" 'keyboard-quit)
  (general-def "C-;" 'embrace-commander)
  (general-def [C-return] '+default/newline-below)
  (general-def "C-RET" '+default/newline-below)
  (general-def [C-S-return] '+default/newline-above)
  (general-def "C-S-RET" '+default/newline-above)
  (general-def "M-?" '+consult-ripgrep-at-point)
  (general-def "M->" '+lookup-xref-references-backend-fn)
  (general-def "M-y" 'consult-yank-from-kill-ring)
  (general-def "C-0" 'vterm-posframe-toggle)

  ;; Navigation
  (general-define-key
   :states 'insert
   :keymaps 'evil-insert-state-map
   "C-n" 'next-line
   "C-p" 'previous-line
   "C-a" 'beginning-of-line
   "C-d" 'delete-char
   "C-e" 'end-of-line
   "C-k" 'kill-line
   )

  ;; Leader def
  (leader-def
    :keymaps 'override
    "<SPC>" '(consult-project-extra-find :wk "Project Find File")
    ";" '((lambda() (interactive "") (org-agenda nil "n")) :wk "Agenda")
    ":" '(execute-extended-command :wk "M-x")
    "/" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Search here")
    "?" '(+consult-ripgrep-at-point :wk "Search symbol here")
    "\\" '(evilnc-comment-or-uncomment-to-the-line :wk "Comment to line")
    "." '(noct-consult-ripgrep-or-line :wk "Swiper")
    "`" '(vertico-repeat-last :wk "Repeat last search")
    "-" '(vertico-repeat-select :wk "Repeat historical search")
    "[" '(previous-buffer :wk "Previous buffer")
    "]" '(next-buffer :wk "Next buffer")

    "b" '(:wk "Buffer")
    "b[" '(previous-buffer :wk "Previous buffer")
    "b]" '(next-buffer :wk "Next buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bB" '(switch-to-buffer-other-window :wk "Switch buffer other window")
    "bd" '(kill-current-buffer :wk "Kill buffer")

    "c" '(:wk "Code")
    "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "cf" '(format-all-buffer :wk "Format buffer")
    "cF" '(eglot-find-implementation :wk "Find implementation")
    "cD" '(eglot-find-typeDefinition :wk "Find References")
    "cd" '(eglot-find-declaration :wk "Jump to definition")
    "cI" '(+eglot-organize-imports :wk "Organize import")
    "ci" '(consult-eglot-symbols :wk "Symbols in current file")
    "cr" '(eglot-rename :wk "LSP rename")
    "co" '(imenu :wk "Outline")
    "ca" '(eglot-code-actions :wk "Code Actions")
    "cc" '(separedit-dwim :wk "Write comment")
    "ch" '(+eglot-help-at-point :wk "Toggle lsp-ui-doc")

    "d" '(:wk "Debug")
    "dt" '(dap-breakpoint-toggle :wk "Add breakpoint")

    "e" '(:wk "Error")
    "eb" '(flymake-start :wk "Check current buffer")
    "el" '(consult-flymake :wk "List errors")
    "eP" '(flymake-show-project-diagnostics :wk "Show project errors")
    "en" '(flymake-goto-next-error :wk "Next error")
    "ep" '(flymake-goto-prev-error :wk "Previous error")

    "f" '(:wk "Files")
    "ff" '(find-file :wk "Find file")
    "fF" '(find-file-other-window :wk "Find file in new Frame")
    "fr" '(recentf-open-files :wk "Recent file")
    "fR" '(embark-rename-buffer :wk "Rename file")
    "fp" '(+open-configuration-folder :wk ".emacs.d")
    "fD" '(+my-delete-file :wk "Delete file")
    "f<SPC>" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "fo" '((lambda() (interactive)(find-file +org-capture-file-gtd)) :which-key "Org files")
    "fj" '(consult-dir :wk "Consult directory")
    "fh" '((lambda() (interactive)(consult-fd default-directory)) :wk "Find file here")

    "g" '(:wk "Git")
    "gs" '(magit-status :wk "Git status")
    "gb" '(magit-branch-checkout :wk "Git checkout")
    "gB" '(magit-blame :wk "Git blame")
    "gm" '(gitmoji-picker :wk "Gitmoji")
    "gM" '((lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit))) :wk "Git stage and commit")
    "gf" '(magit-fetch :wk "Git fetch")
    "gF" '(magit-pull :wk "Git pull")
    "go" '(magit-open-repo :wk "Open repo")
    "gu" '(aborn/simple-git-commit-push :wk "Commit and push")
    "gy" '(magit-add-current-buffer-to-kill-ring :wk "Copy current branch name")
    "gt" '(git-timemachine :wk "Git timemachine")

    "j" '(:wk "Jump")
    "jj" '(evil-avy-goto-char :wk "Jump to character")
    "jl" '(evil-avy-goto-line :wk "Jump to line")
    "jJ" '(evil-avy-goto-char-2 :wk "Jump to character 2")

    "m" '(:wk "Local leader")

    "o" '(:wk "Open")
    "ot" '(org-todo-list :wk "Org todos")
    "ox" '(org-agenda :wk "Org agenda")
    "oc" '(cfw:open-org-calendar :wk "Open calendar")
    "oi" '(consult-clock-in :wk "Clock in")
    "oo" '((lambda () (interactive)(org-clock-out)(org-save-all-org-buffers)) :wk "Clock out")
    "od" '(consult-mark-done :wk "Mark done")

    "p" '(:wk "Project")
    "pk" '(project-kill-buffers :wk "Kill project buffers" )
    "pp" '(project-switch-project :wk "Switch project")
    "pf" '(project-find-file :wk "Find file in project")
    "pt" '(magit-todos-list :wk "List project tasks")
    "pS" '(save-some-buffers :wk "Save project buffers")
    "pd" '(project-find-dir :wk "Find dir in project")

    "q" '(:wk "Quit")
    "qq" '(kill-emacs :wk "Quit")
    "qr" '(restart-emacs :wk "Restart")

    "s" '(:wk "Search")
    "sa" '(consult-org-agenda :wk "Search agenda")
    ;; "sd" '(+devdocs-lookup-at-point :wk "Devdocs lookup")
    "sd" '(devdocs-dwim :wk "Devdocs lookup")
    "sD" '(+devdocs-search-at-point :wk "Devdocs search")
    "sf" '(locate :wk "Locate file")
    "sh" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Search here")
    "si" '(+my-imenu :wk "Jump to symbol")
    "sI" '(consult-imenu-multi :wk "Jump to symbol all buffer")
    "sp" '(consult-ripgrep :wk "Search project")
    "sP" '(color-rg-search-project :wk "Color-rg Search project")
    "sy" '(color-rg-search-symbol-in-project :wk "Color-rg Search symbol")
    "sT" '(load-theme :wk "Load theme")
    "sc" '(:wk "In current file")
    "scs" '(color-rg-search-input-in-current-file :wk "Search input")
    "sci" '(color-rg-search-symbol-in-current-file :wk "Search symbol at point")
    "sg" '(+my/google-it :wk "Google")

    "t" '(:wk "Toggle")
    "te" '(vterm-posframe-toggle :wk "Shell")
    "tt" '(dirvish-side :wk "Tree view")
    "tl" '(toggle-truncate-lines :wk "Toggle line wrap")
    "td" '(toggle-debug-on-error :wk "Toggle debug on error")
    "ti" '(imenu-list-smart-toggle :wk "imenu-list")
    "ty" '(my-youdao-search-at-point :wk "youdao")

    "w" '(:wk "Window")
    "wv" '(split-window-vertically :wk "Split window vertically")
    "wH" '(split-window-horizontally :wk "Split window horizontally")
    "wj" '(evil-window-down :wk "Focus window down")
    "wk" '(evil-window-up :wk "Focus window up")
    "wh" '(evil-window-left :wk "Focus window left")
    "wl" '(evil-window-right :wk "Focus window right")
    "wu" '(winner-undo :wk "Undo window")
    "wo" '(winner-redo :wk "Redo window")

    "x" '(org-capture :wk "Org capture")


    "=" '(er/expand-region :wk "Expand Region")
    )

  ;; Python
  (leader-def
    :states 'normal
    :keymaps 'python-mode-map
    "md" '(sphinx-doc :wk "Docstring")

    "mv" '(:wk "Virtualenv")
    "mvw" '(pyvenv-workon :wk "Pyvenv workon")
    "mva" '(pyvenv-activate :wk "Pyvenv activate")
    "mvd" '(pyvenv-deactivate :wk "Pyvenv deactivate")

    "mi" '(:wk "Imports")
    "mis" '(+python/python-sort-imports :wk "Sort imports")
    "mii" '(importmagic-fix-imports :wk "Fix imports")

    "mt" '(:wk "Tests")
    "mtp" '(python-pytest-popup :wk "Pytest popup")
    "mtf" '(python-pytest-file :wk "Pytest file")
    "mtF" '(python-pytest-file-dwim :wk "Pytest file dwim")
    "mtt" '(python-pytest-function :wk "Pytest function")
    "mtT" '(python-pytest-function-dwim :wk "Pytest function dwin")
    "mtr" '(python-pytest-repeat :wk "Pytest repeat")
    "mtl" '(python-pytest-last-failed :wk "Pytest last failed")

    "mp" '(:wk "Poetry")
    "mpv" '(poetry-venv-workon :wk "Poetry workon")
    "mpV" '(poetry-venv-deactivate :wk "Poetry deactivate workon")
    "mpp" '(poetry :wk "Poetry popup")
    "mpa" '(poetry-add :wk "Poetry add dep")
    "mpr" '(poetry-run :wk "Run poetry command")
    "mpR" '(poetry-remove :wk "Poetry remove dep")
    )

  ;; JS
  (leader-def
    :states 'normal
    :keymaps '(js-mode-map js2-mode-map rjsx-mode-map)
    "mi" '(:wk "Imports")
    "mif" '(import-js-fix :wk "Fix imports")
    "mir" '(run-import-js :wk "Run import js")
    "mii" '(import-js-import :wk "Import module")

    "md" '(:wk "Docs")
    "mdf" '(js-doc-insert-function-doc :wk "Insert function doc")
    "mdF" '(js-doc-insert-file-doc :wk "Insert file doc")
    "mdt" '(js-doc-insert-tag :wk "Insert tag")
    ))

(use-package embark
  :after-call +my/first-input-hook-fun
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)
   ("C-/" . embark-export)      ;; alternative for `describe-bindings'
   :map embark-file-map
   ("F" . find-file-other-window)
   ("r" . +my-rename-file)
   ("d" . +my-delete-file))
  :custom
  (embark-cycle-key ".")
  (embark-help-key "?")
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;;  HACK: bind will be override by evil
  (with-eval-after-load 'general
    (general-define-key :states '(normal insert visual emacs)
                        "C-." 'embark-act
                        "M-." 'embark-dwim
                        "C-h B" 'embark-bindings))
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; which key indicator
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  (add-hook 'embark-collect-post-revert-hook
            (defun resize-embark-collect-window (&rest _)
              (when (memq embark-collect--kind '(:live :completions))
                (fit-window-to-buffer (get-buffer-window)
                                      (floor (frame-height) 2) 1)))))

(use-package embark-consult
  :after consult)

(use-package vertico
  :hook (+my/first-input . vertico-mode)
  :bind
  (:map vertico-map
        ("C-<return>" . open-in-external-app))
  :custom
  (vertico-cycle nil)
  (vertico-preselect 'first)
  :config
  (defun +vertico/jump-list (jump)
    "Go to an entry in evil's (or better-jumper's) jumplist."
    (interactive
     (let (buffers)
       (unwind-protect
           (list
            (consult--read
             ;; REVIEW Refactor me
             (nreverse
              (delete-dups
               (delq
                nil (mapcar
                     (lambda (mark)
                       (when mark
                         (cl-destructuring-bind (path pt _id) mark
                           (let* ((visiting (find-buffer-visiting path))
                                  (buf (or visiting (find-file-noselect path t)))
                                  (dir default-directory))
                             (unless visiting
                               (push buf buffers))
                             (with-current-buffer buf
                               (goto-char pt)
                               (font-lock-fontify-region
                                (line-beginning-position) (line-end-position))
                               (format "%s:%d: %s"
                                       (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                           (file-relative-name (buffer-file-name buf) dir))
                                                     #'< :key #'length))
                                       (line-number-at-pos)
                                       (string-trim-right (or (thing-at-point 'line) ""))))))))
                     (cddr (better-jumper-jump-list-struct-ring
                            (better-jumper-get-jumps (better-jumper--get-current-context))))))))
             :prompt "jumplist: "
             :sort nil
             :require-match t
             :category 'jump-list))
         (mapc #'kill-buffer buffers))))
    (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
        (user-error "No match")
      (let ((file (match-string-no-properties 1 jump))
            (line (match-string-no-properties 2 jump)))
        (find-file file)
        (goto-char (point-min))
        (forward-line (string-to-number line)))))

  ;; Configure directory extension.
  (use-package vertico-quick
    :after vertico
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))

  (use-package vertico-repeat
    :after vertico
    :config
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (with-eval-after-load 'general
      (general-def "C-c r" 'vertico-repeat)
      ))
  (use-package vertico-directory
    :after vertico
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)
                ("C-w" . vertico-directory-up))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    ))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'completion)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(use-package consult
  :after orderless
  :commands (+consult-ripgrep-at-point noct-consult-ripgrep-or-line consult-line-symbol-at-point consult-clock-in)
  :bind (([remap recentf-open-files] . consult-recent-file)
         ([remap imenu] . consult-imenu)
         ([remap switch-to-buffer] . consult-buffer)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-s bindings (search-map)
         ("M-s m" . consult-multi-occur)
         ;; Isearch integration
         ("M-s e" . consult-isearch))
  :config
  (setq consult-preview-key "M-p")
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-args "fd --color=never --full-path ARG OPTS")

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)


  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :state    ,#'consult--buffer-state
                :hidden   t
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

  (defun my-consult-set-evil-search-pattern (&optional condition)
    (let ((re
           (cond
            ((eq condition 'rg) (substring (car consult--grep-history) 1)) ;; HACK: assume the history begins with `#'
            ((or t (eq condition 'line)) (car consult--line-history))
            )))
      (add-to-history 'evil-ex-search-history re)
      (setq evil-ex-search-pattern (list re t t))
      (setq evil-ex-search-direction 'forward)
      (anzu-mode t)))

  ;; simulate swiper
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol))
    (my-consult-set-evil-search-pattern))

  (defcustom noct-consult-ripgrep-or-line-limit 300000
    "Buffer size threshold for `noct-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
    :type 'integer)

  ;; simulate counsel-grep-or-swiper
  (defun noct-consult-ripgrep-or-line ()
    "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
    (interactive)
    (evil-without-repeat
      (if (or (not buffer-file-name)
              (buffer-narrowed-p)
              (ignore-errors
                (file-remote-p buffer-file-name))
              (jka-compr-get-compression-info buffer-file-name)
              (<= (buffer-size)
                  (/ noct-consult-ripgrep-or-line-limit
                     (if (eq major-mode 'org-mode) 4 1))))
          (progn (consult-line)
                 (my-consult-set-evil-search-pattern))

        (when (file-writable-p buffer-file-name)
          (save-buffer))
        (let ((consult-ripgrep-args
               (concat "rg "
                       "--null "
                       "--line-buffered "
                       "--color=ansi "
                       "--max-columns=250 "
                       "--no-heading "
                       "--line-number "
                       ;; adding these to default
                       "--smart-case "
                       "--hidden "
                       "--max-columns-preview "
                       ;; add back filename to get parsing to work
                       "--with-filename "
                       ;; defaults
                       "-e ARG OPTS "
                       (shell-quote-argument buffer-file-name))))
          (consult-ripgrep)
          (my-consult-set-evil-search-pattern 'rg)))))

  ;; Configure initial narrowing per command
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t))))

  (defun +consult-ripgrep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (consult-ripgrep dir initial))

  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)
  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))

  (defun +my/retrieval-todo-items ()
    (require 'consult-org)
    (consult--read
     (consult--with-increased-gc
      (-filter (lambda (item)
                 (not (member
                       (car (cdr (get-text-property 0 'consult-org--heading item)))
                       '("DONE" "CANCELED"))))
               (consult-org--headings nil nil 'agenda)))
     :prompt "Go to heading: "
     :category 'consult-org-heading
     :sort nil
     :require-match t
     :history '(:input consult-org--history)
     :narrow (consult-org--narrow)
     :state (consult--jump-state)
     :group
     (lambda (cand transform)
       (let ((name (buffer-name
                    (marker-buffer
                     (get-text-property 0 'consult--candidate cand)))))
         (if transform cand name)))
     :lookup #'consult--lookup-candidate))

  (defun consult-clock-in (&optional match scope resolve)
    "Clock into an Org heading."
    (interactive (list nil nil current-prefix-arg))
    (require 'org-clock)
    (org-clock-load)
    (save-window-excursion
      (consult-org-heading
       match
       (or scope
           (thread-last org-clock-history
                        (mapcar 'marker-buffer)
                        (mapcar 'buffer-file-name)
                        (delete-dups)
                        (delq nil))
           (user-error "No recent clocked tasks")))
      (org-clock-in nil (when resolve
                          (org-resolve-clocks)
                          (org-read-date t t)))))

  (consult-customize consult-clock-in
                     :prompt "Clock in: "
                     :preview-key (kbd "M-.")
                     :group
                     (lambda (cand transform)
                       (let* ((marker (get-text-property 0 'consult--candidate cand))
                              (name (if (member marker org-clock-history)
                                        "*Recent*"
                                      (buffer-name (marker-buffer marker)))))
                         (if transform (substring cand (1+ (length name))) name))))

  (defun consult-mark-done ()
    "Clock into an Org agenda heading."
    (interactive)
    (save-window-excursion
      (+my/retrieval-todo-items)
      (org-todo 'done)
      (save-buffer)))
  (consult-customize consult-mark-done :prompt "Mark done: ")

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

  ;; Shorten candidates in consult-buffer:
  ;; See: https://emacs-china.org/t/21-emacs-vertico-orderless-marginalia-embark-consult/19683/50
  (defun vmacs-consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file recentf-list (nreverse items))
        ;; Emacs 29 abbreviates file paths by default, see
        ;; `recentf-filename-handlers'.
        (unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
        (unless (gethash file ht)
          (push (propertize
                 (vmacs-short-filename file)
                 'multi-category `(file . ,file))
                items)))))

  (defun vmacs-short-filename(file)
    "return filename with one parent directory.
/a/b/c/d-> c/d"
    (let* ((file (directory-file-name file))
           (filename (file-name-nondirectory file))
           ;; (dir (file-name-directory file))
           short-name)
      (setq short-name filename
            ;; (if dir
            ;;     (format "%s/%s" (file-name-nondirectory
            ;;                      (directory-file-name dir))
            ;;             filename)
            ;;   filename)
            )
      (propertize short-name 'multi-category `(file . ,file))))

  (plist-put consult--source-recent-file
             :items #'vmacs-consult--source-recentf-items)
  (advice-add 'marginalia--annotate-local-file :override
              (defun marginalia--annotate-local-file-advice (cand)
                (marginalia--fields
                 ((marginalia--full-candidate cand)
                  :face 'marginalia-size ))))
  )

(use-package consult-dir
  :commands (consult-dir consult-dir-jump-file)
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (defun consult-dir--zlua-dirs ()
    "Return list of fasd dirs."
    (reverse
     (mapcar
      (lambda (str) (format "%s/" (car (last (split-string str " ")))))
      (split-string (shell-command-to-string "z -l | tail -n 50") "\n" t))))
  (defvar consult-dir--source-zlua
    `(:name     "z.lua dirs"
                :narrow   ?z
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () t)  ;;  FIXME: check whether z.lua is installed
                :items    ,#'consult-dir--zlua-dirs)
    "Fasd directory source for `consult-dir'.")
  (setq consult-dir-sources '(consult-dir--source-recentf consult-dir--source-zlua consult-dir--source-project)))

(use-package consult-project-extra
  :commands (consult-project-extra-find)
  :after consult)

(use-package orderless
  :after-call +my/first-input-hook-fun
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ;; ((and
     ;;   ;; Completing filename or eshell
     ;;   (or minibuffer-completing-file-name
     ;;       (derived-mode-p 'eshell-mode))
     ;;   ;; File extension
     ;;   (string-match-p "\\`\\.." pattern))
     ;;  `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (flex styles basic partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch))

  )

(use-package marginalia
  :hook (+my/first-input . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))

(use-package vertico-posframe
  :when (display-graphic-p)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-parameters
        '((max-width . 80)
          (min-width . 60)
          (left-fringe . 8)
          (right-fringe . 8)))
  (evil-set-initial-state 'minibuffer-mode 'emacs))

(use-package all-the-icons
  :when (display-graphic-p)
  :demand t
  :config
  (declare-function memoize 'memoize)
  (declare-function memoize-restore 'memoize)
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (ignore-errors
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  ;; Support more icons
  (defvar my-extension-icon-alist
    '(("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
      ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
      ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
      ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
      ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
      ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
      ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen)))

  (defvar my-regexp-icon-alist
    '(("Cask\\'"             all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
      ("^Rakefile$"          all-the-icons-alltheicon "ruby-alt"    :face all-the-icons-red)
      ("\\.\\(bat\\|cmd\\)$" all-the-icons-alltheicon "terminal"    :face all-the-icons-lsilver)
      ("\\go.mod$"           all-the-icons-fileicon "go"            :face all-the-icons-dblue)
      ("\\go.sum$"           all-the-icons-fileicon "go"            :face all-the-icons-dpurple)
      ("\\.[bB][iI][nN]$"    all-the-icons-octicon "file-binary"    :v-adjust 0.0 :face all-the-icons-yellow)
      ("NEWS$"               all-the-icons-faicon "newspaper-o"     :height 0.9 :v-adjust -0.2)))

  (defvar my-mode-icon-alist
    '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
      (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
      (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
      (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
      (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
      (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
      (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
      (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
      (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
      (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
      (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
      (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
      (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
      (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
      (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
      (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
      (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
      (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
      (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
      (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
      (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
      (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
      (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
      (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
      (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
      (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
      (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)))

  (dolist (i my-extension-icon-alist)
    (add-to-list 'all-the-icons-extension-icon-alist i))
  (dolist (i my-regexp-icon-alist)
    (add-to-list 'all-the-icons-regexp-icon-alist i))
  (dolist (i my-mode-icon-alist)
    (add-to-list 'all-the-icons-mode-icon-alist i)))

(use-package all-the-icons-completion
  :commands (all-the-icons-completion-marginalia-setup)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.01)
  (corfu-max-width 120)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)    ;; Disable candidate preselection

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-SPC" . corfu-insert-separator)
        ("S-TAB" . corfu-previous)
        ("C-j" . corfu-insert)
        ("C-d" . corfu-info-documentation)
        ("M-." . corfu-info-location)
        ("C-i" . nil)
        ("M-j" . nil)
        ("M-k" . nil)
        ([?\r] . nil)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'insert 'corfu-map
      (kbd "C-j") 'corfu-insert))
  :config
  (use-package corfu-quick
    :after corfu
    :commands (corfu-quick-insert corfu-quick-complete)
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)
          ("M-q" . corfu-quick-complete)))

  (use-package corfu-history
    :after corfu
    :hook (corfu-mode . corfu-history-mode))

  (use-package corfu-popupinfo
    :after corfu
    :hook (corfu-mode . corfu-popupinfo-mode)
    :config
    (set-face-attribute 'corfu-popupinfo nil :height 140)
    (setq corfu-popupinfo-delay '(0.5 . 0.3)))

  (add-to-list 'corfu-auto-commands 'awesome-pair-open-round)
  (add-to-list 'corfu-auto-commands 'awesome-pair-open-bracket)
  (add-to-list 'corfu-auto-commands 'awesome-pair-open-curly)

  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)

  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (with-eval-after-load 'all-the-icons
    (defvar kind-all-the-icons--cache nil
      "The cache of styled and padded label (text or icon).
An alist.")

    (defun kind-all-the-icons-reset-cache ()
      "Remove all cached icons from `kind-all-the-icons-mapping'."
      (interactive)
      (setq kind-all-the-icons--cache nil))

    (defun kind-all-the-icons--set-default-clear-cache (&rest args)
      (kind-all-the-icons-reset-cache)
      (apply #'set-default args))

    (defvar kind-all-the-icons--icons
      `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
        (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
        (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
        (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
        (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
        (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
        (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
        (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


    (defsubst kind-all-the-icons--metadata-get (metadata type-name)
      (or
       (plist-get completion-extra-properties (intern (format ":%s" type-name)))
       (cdr (assq (intern type-name) metadata))))

    (defun kind-all-the-icons-formatted (kind)
      "Format icon kind with all-the-icons"
      (or (alist-get kind kind-all-the-icons--cache)
          (let ((map (assq kind kind-all-the-icons--icons)))
            (let*  ((icon (if map
                              (cdr map)
                            (cdr (assq t kind-all-the-icons--icons))))
                    (half (/ (default-font-width) 2))
                    (pad (propertize " " 'display `(space :width (,half))))
                    (disp (concat pad icon pad)))
              (setf (alist-get kind kind-all-the-icons--cache) disp)
              disp))))

    (defun kind-all-the-icons-margin-formatter (metadata)
      "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
      (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
          (lambda (cand)
	          (if-let ((kind (funcall kind-func cand)))
	              (kind-all-the-icons-formatted kind)
	            (kind-all-the-icons-formatted t))))) ;; as a backup
    (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
    )

  ;; allow evil-repeat
  ;; https://github.com/minad/corfu/pull/225
  (defun corfu--unread-this-command-keys ()
    (when (> (length (this-command-keys)) 0)
      (setq unread-command-events (nconc
                                   (listify-key-sequence (this-command-keys))
                                   unread-command-events))
      (clear-this-command-keys t)))

  (defun corfu--pre-command ()
    "Insert selected candidate unless command is marked to continue completion."
    (when corfu--preview-ov
      (delete-overlay corfu--preview-ov)
      (setq corfu--preview-ov nil))
    ;; (corfu--echo-cancel corfu--echo-message)
    ;; Ensure that state is initialized before next Corfu command
    (when (and (symbolp this-command) (string-prefix-p "corfu-" (symbol-name this-command)))
      (corfu--update))
    (when (and (eq corfu-preview-current 'insert)
               (/= corfu--index corfu--preselect)
               ;; See the comment about `overriding-local-map' in `corfu--post-command'.
               (not (or overriding-terminal-local-map
                        (corfu--match-symbol-p corfu-continue-commands this-command))))
      (corfu--unread-this-command-keys)
      (setq this-command 'corfu-insert-exact)))

  (defun corfu-insert-exact ()
    "Insert current candidate with the `exact' status.
  Quit if no candidate is selected."
    (interactive)
    (if (>= corfu--index 0)
        (corfu--insert 'exact)
      (corfu-quit)))

  (mapc #'evil-declare-ignore-repeat
        '(corfu-next
          corfu-previous
          corfu-first
          corfu-last))

  (mapc #'evil-declare-change-repeat
        '(corfu-insert
          corfu-insert-exact
          corfu-complete)))

(use-package cape
  :after corfu
  ;; Bind dedicated completion commands
  :bind (("C-x C-f" . cape-file)
         ("C-x C-k" . cape-keyword)
         ("C-x C-s" . cape-symbol)
         ("C-x C-l" . cape-line)
         ("C-x C-w" . cape-dict))
  :hook ((prog-mode . my/set-basic-capf)
         (emacs-lisp-mode . (lambda ()
                              (my/convert-super-capf #'elisp-completion-at-point)))
         (org-mode . my/set-basic-capf)
         )
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (setq cape-dict-file "/usr/share/dict/words")

  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     (cape-super-capf
      arg-capf
      #'tabnine-completion-at-point
      #'tempel-complete)
     ;; #'cape-dabbrev
     ))

  (defun my/set-basic-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf (car completion-at-point-functions))))

  (defun my/set-eglot-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf #'eglot-completion-at-point))
    )
  (defun my/set-lsp-bridge-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf #'lsp-bridge-capf)))

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Add `completion-at-point-functions', used by `completion-at-point'.

  (use-package tempel
    :after-call +my/first-input-hook-fun
    :after cape
    :config
    (defun my/tempel-expand-or-next ()
      "Try tempel expand, if failed, try copilot expand."
      (interactive)
      (if tempel--active
          (tempel-next 1)
        (call-interactively #'tempel-expand)))

    (with-eval-after-load 'general
      (general-define-key
       :keymaps '(evil-insert-state-map)
       "C-o" 'my/tempel-expand-or-next)))

  (use-package tabnine-capf
    :after cape
    :commands (tabnine-completion-at-point tabnine-capf-start-process)
    :hook ((kill-emacs . tabnine-capf-kill-process))))

(use-package pinyinlib
  :after-call +my/first-input-hook-fun
  :after orderless
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package multi-translate
  :commands (multi-translate multi-translate-at-point multi-translate-yank-at-point)
  :custom
  (multi-translate-sentence-backends '(google))
  (multi-translate-word-backends '(bing youdao))
  :config
  (defun multi-translate-yank-at-point (arg)
    "Used temporarily for thesis"
    (interactive "P")
    (let* ((bounds (if (region-active-p)
                       (cons (region-beginning) (region-end))
                     (bounds-of-thing-at-point 'word)))
           (text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))
      (kill-new (multi-translate--google-translation "en" "zh-CN" text))
      (evil-normal-state)
      (message "Translate Done"))))

(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :hook (window-setup . which-key-mode))

(use-package vundo
  :commands vundo
  :defer t
  :config
  (setf (alist-get 'selected-node vundo-glyph-alist) ?X
        (alist-get 'node vundo-glyph-alist) ?O))


;; Colourful dired
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dired-git-info
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map ")" 'dired-git-info-mode))

;; Extra Dired functionality
(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(use-package dirvish
  :after dired
  :hook ((+my/first-input . dirvish-override-dired-mode)
         (evil-collection-setup . (lambda (&rest a)
                                    (evil-define-key '(normal) dired-mode-map
                                      (kbd "C-c f") 'dirvish-fd
                                      "i" 'wdired-change-to-wdired-mode
                                      "." 'dired-omit-mode
                                      (kbd "TAB") 'dirvish-subtree-toggle
                                      (kbd "M-s") 'dirvish-setup-menu
                                      (kbd "M-f") 'dirvish-toggle-fullscreen
                                      "*"   'dirvish-mark-menu
                                      "f"   'dirvish-file-info-menu
                                      [remap dired-sort-toggle-or-edit] 'dirvish-quicksort
                                      [remap dired-do-redisplay] 'dirvish-ls-switches-menu
                                      [remap dired-summary] 'dirvish-dispatch
                                      [remap dired-do-copy] 'dirvish-yank-menu
                                      [remap mode-line-other-buffer] 'dirvish-history-last))))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :custom
  (dirvish-attributes '(all-the-icons file-size))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-side-follow-buffer-file t)
  :config
  (when (boundp 'dirvish-side-follow-mode)
    (dirvish-side-follow-mode t))
  (set-face-attribute 'ansi-color-blue nil :foreground "#FFFFFF")
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (general-define-key :states '(normal)
                      :keymaps 'dirvish-mode-map
                      "?" 'dirvish-menu-all-cmds)

  (use-package dirvish-extras
    :after dirvish)
  )

;; SaveAllBuffers
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))
(with-eval-after-load 'general
  (general-def "C-x C-s" nil)
  (general-def "C-x C-s" 'save-all-buffers)
  )
;; -SaveAllBuffers

(use-package project
  :defer t
  :commands (project-find-file project-switch-project)
  :config
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project))))))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (window-setup . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package popper
  :defer t
  :defines popper-echo-dispatch-actions
  :bind (:map popper-mode-map
              ("C-h z" . popper-toggle-latest)
              ("C-<tab>"   . popper-cycle)
              ("C-M-<tab>" . popper-toggle-type))
  :hook (window-setup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Agenda Commands\\*"
          "\\*eldoc\\*"
          "\\*eldoc .*\\*"
          ;; "\\*Calendar\\*"              ; FIXME: https://github.com/karthink/popper/issues/29
          "\\*lspce-hover\\*"
          "\\*Embark Actions\\*"
          "\\*Embark Export: .*\\*"

          bookmark-bmenu-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode
          "^\\*vterm.*\\*$"  vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flymake diagnostics for .*\\*"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"
          "^\\*Flutter.*\\*$"

          "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))

  (with-eval-after-load 'project
    (setq popper-group-function 'popper-group-by-project))
  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)
  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))


(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-height 15)
  (doom-modeline-buffer-modification-icon t))

(use-package ef-themes
  :init
  (ef-themes-select 'ef-trio-light)
  :config
  (defun +my-custom-org-todo-faces()
    (ef-themes-with-colors
      (setq org-todo-keyword-faces
            `(("TODO" . (:foreground ,red-cooler :weight bold))
              ("INPROCESS"  . ,yellow-cooler)
              ("PROJ"  . ,cyan-cooler)
              ("WAITING" . ,green-faint)
              ("DONE" . (:foreground ,fg-alt :strike-through t))
              ("CANCELED" . (:foreground ,fg-dim :weight bold :strike-through t)))
            )))
  (with-eval-after-load 'org
    (+my-custom-org-todo-faces)))

;; FontsList
(defvar font-list '(("Iosevka SS08" . 16) ("Cascadia Code" . 15) ("Maple Mono SC NF" . 14) ("Fira Code" . 15) ("SF Mono" . 15))
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
  "Set default font."
  (cl-loop for font in font-list
           when (font-installed-p (car font))
           return (set-face-attribute 'default nil :font (car font) :height (* 10 (cdr font))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Sarasa Mono SC Nerd" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font))

  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

;; (add-hook 'after-init-hook #'my-apply-font)
(add-hook 'window-setup-hook #'my-apply-font)

(use-package hl-line
  :defer t
  :custom-face (hl-line ((t (:extend t))))
  :hook ((after-init . global-hl-line-mode)
         ((term-mode vterm-mode) . hl-line-unload-function)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :defer t
  :diminish
  :bind (:map help-mode-map
              ("w" . rainbow-mode))
  ;; :hook ((web-mode) . rainbow-mode)
  :config
  ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  (defun my-rainbow-colorize-match (color &optional match)
    (let* ((match (or match 0))
           (ov (make-overlay (match-beginning match) (match-end match))))
      (overlay-put ov 'ovrainbow t)
      (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                "white" "black"))
                              (:background ,color)))))
  (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

  (defun my-rainbow-clear-overlays ()
    "Clear all rainbow overlays."
    (remove-overlays (point-min) (point-max) 'ovrainbow t))
  (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE" "DONT" "GOTCHA" "DEBUG"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK" "FIXME"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))

  (dolist (keyword '("MARK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'success)) hl-todo-keyword-faces))
  )

(use-package volatile-highlights
  :diminish
  :hook (+my/first-input . volatile-highlights-mode)
  :config
  (when (fboundp 'pulse-momentary-highlight-region)
    (defun my-vhl-pulse (beg end &optional _buf face)
      "Pulse the changes."
      (pulse-momentary-highlight-region beg end face))
    (advice-add #'vhl/.make-hl :override #'my-vhl-pulse)))

;; Pulse current line
(use-package pulse
  :disabled
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun my-pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 ace-window aw--select-window
                 pager-page-down pager-page-up
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse)))


(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
  For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...
- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))

             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.
This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       +magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let (window (window-in-direction direction))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
          (unless magit-display-buffer-noselect
            (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))

(defun magit-add-current-buffer-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))


(defvar gitmoji--all-emoji
  '(("Improving structure / format of the code." . ":art:")
    ("Improving performance." . ":zap:")
    ("Removing code or files." . ":fire:")
    ("Fixing a bug." . ":bug:")
    ("Critical hotfix." . ":ambulance:")
    ("Introducing new features." . ":sparkles:")
    ("Writing docs." . ":memo:")
    ("Deploying stuff." . ":rocket:")
    ("Updating the UI and style files." . ":lipstick:")
    ("Initial commit." . ":tada:")
    ("Updating tests." . ":white_check_mark:")
    ("Fixing security issues." . ":lock:")
    ("Fixing something on macOS." . ":apple:")
    ("Fixing something on Linux." . ":penguin:")
    ("Fixing something on Windows." . ":checkered_flag:")
    ("Fixing something on Android." . ":robot:")
    ("Fixing something on iOS." . ":green_apple:")
    ("Releasing / Version tags." . ":bookmark:")
    ("Removing linter warnings." . ":rotating_light:")
    ("Work in progress." . ":construction:")
    ("Fixing CI Build." . ":green_heart:")
    ("Downgrading dependencies." . ":arrow_down:")
    ("Upgrading dependencies." . ":arrow_up:")
    ("Pinning dependencies to specific versions." . ":pushpin:")
    ("Adding CI build system." . ":construction_worker:")
    ("Adding analytics or tracking code." . ":chart_with_upwards_trend:")
    ("Refactoring code." . ":recycle:")
    ("Work about Docker." . ":whale:")
    ("Adding a dependency." . ":heavy_plus_sign:")
    ("Removing a dependency." . ":heavy_minus_sign:")
    ("Changing configuration files." . ":wrench:")
    ("Internationalization and localization." . ":globe_with_meridians:")
    ("Fixing typos." . ":pencil2:")
    ("Writing bad code that needs to be improved." . ":hankey:")
    ("Reverting changes." . ":rewind:")
    ("Merging branches." . ":twisted_rightwards_arrows:")
    ("Updating compiled files or packages." . ":package:")
    ("Updating code due to external API changes." . ":alien:")
    ("Moving or renaming files." . ":truck:")
    ("Adding or updating license." . ":page_facing_up:")
    ("Introducing breaking changes." . ":boom:")
    ("Adding or updating assets." . ":bento:")
    ("Updating code due to code review changes." . ":ok_hand:")
    ("Improving accessibility." . ":wheelchair:")
    ("Documenting source code." . ":bulb:")
    ("Writing code drunkenly." . ":beers:")
    ("Updating text and literals." . ":speech_balloon:")
    ("Performing database related changes." . ":card_file_box:")
    ("Adding logs." . ":loud_sound:")
    ("Removing logs." . ":mute:")
    ("Adding contributor(s)." . ":busts_in_silhouette:")
    ("Improving user experience / usability." . ":children_crossing:")
    ("Making architectural changes." . ":building_construction:")
    ("Working on responsive design." . ":iphone:")
    ("Mocking things." . ":clown_face:")
    ("Adding an easter egg." . ":egg:")
    ("Adding or updating a .gitignore file" . ":see_no_evil:")
    ("Adding or updating snapshots" . ":camera_flash:")
    ("Experimenting new things" . ":alembic:")
    ("Improving SEO" . ":mag:")
    ("Work about Kubernetes" . ":wheel_of_dharma:")
    ("Catching errors" . ":goal_net:")
    ("Adding or updating types (Flow, TypeScript)" . ":label:")
    ("增加新特性" . "feat")
    ("bug 修复" . "fix")
    ("文档改动" . "docs")
    ("功能、交互优化" . "improve")
    ("格式改动（不影响代码运行的变动，例如加空格、换行、分号等）" . "style")
    ("重构代码" . "refactor")
    ("性能相关优化" . "perf")
    ("测试代码" . "test")
    ("构建过程或辅助工具变动" . "chore")
    ("回滚" . "revert")
    ("合并" . "merge")
    ("上传资源文件" . "resource")))

(defun gitmoji-picker ()
  "Choose a gitmoji."
  (interactive)
  (let* ((choices gitmoji--all-emoji)
         (candidates (mapcar (lambda (cell)
                               (cons (format "%s — %s" (cdr cell) (car cell)) (concat (cdr cell) " ")))
                             choices)))
    (insert (cdr (assoc (completing-read "Choose a gitmoji " candidates) candidates)))
    (evil-insert-state)))

(use-package magit-todos
  :after magit)

(use-package pretty-hydra
  :diminish)

(use-package smerge-mode
  :after magit
  :diminish
  :bind (:map smerge-mode-map
              ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))
         )
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t))
  (require 'transient)
  (transient-define-prefix smerge-dispatch ()
    "Invoke an SMerge command from a list of available commands."
    [["Keep"
      ("b" "Base" smerge-keep-base)
      ("u" "Upper" smerge-keep-upper)
      ("l" "Lower" smerge-keep-lower)
      ("a" "All" smerge-keep-all) ("RET" "Current" smerge-keep-current)]
     ["Diff"
      ("<" "Base/upper" smerge-diff-base-upper)
      ("=" "Upper/lower" smerge-diff-upper-lower)
      (">" "Base/lower" smerge-diff-base-lower)
      ("R" "Refine" smerge-refine :transient t)]
     ["Other"
      ("C" "Combine" smerge-combine-with-next)
      ("r" "Resolve" smerge-resolve) ("x" "Kill current" smerge-kill-current)]])
  (define-key (plist-get smerge-text-properties 'keymap)
              (kbd "RET") '(menu-item "" smerge-dispatch :enable (evil-normal-state-p)))
  (evil-define-motion evil-forward-conflict (count)
    "Move the cursor to the beginning of the COUNT-th next conflict."
    :jump t
    (require 'smerge-mode)
    (smerge-next count)
    (unless smerge-mode (smerge-mode)))
  (evil-define-motion evil-backward-conflict (count)
    "Move the cursor to the beginning of the COUNT-th previous conflict."
    :jump t :type inclusive
    (require 'smerge-mode)
    (smerge-prev count)
    (unless smerge-mode (smerge-mode)))
  )

(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

;; YASnippetPac
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand-snippet)
  :hook (prog-mode . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
        ("C-c C-n" . yas-expand-from-trigger-key))
  (:map yas-keymap
        (("M-}" . smarter-yas-expand-next-field)
         ("TAB" . nil)
         ([tab]. nil)
         ))
  :config
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field))))))

;; -YASnippetPac


(use-package flymake
  :defer t
  :after-call +my/first-input-hook-fun
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  (defun sanityinc/eldoc-flymake-first ()
    "Gives flymake's eldoc function priority in the minibuffer."
    (when flymake-mode
      (setq-local eldoc-documentation-functions
                  (cons 'flymake-eldoc-function
                        (delq 'flymake-eldoc-function eldoc-documentation-functions)))))
  (add-hook 'flymake-mode-hook 'sanityinc/eldoc-flymake-first)

  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path
                load-path))

  (setq eldoc-documentation-function 'eldoc-documentation-compose)
  (setq flymake-no-changes-timeout nil))

(use-package elec-pair
  :hook (+my/first-input . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            '(lambda ()
               (setq-local electric-pair-inhibit-predicate
                           `(lambda (c)
                              (if (char-equal c ?<) t
                                (,electric-pair-inhibit-predicate c)))))))

(use-package puni
  :defer t
  :hook ((prog-mode markdown-mode org-mode) . puni-mode)
  :init
  (general-def
    :keymaps 'puni-mode-map
    "DEL" 'puni-backward-delete-char
    "C-d" 'puni-forward-delete-char
    "C-k" 'puni-kill-line
    "M-h" 'puni-force-delete
    "C-u" 'puni-backward-kill-line
    "C-M-f" 'puni-forward-sexp
    "C-M-b" 'puni-backward-sexp
    "C-M-a" 'puni-beginning-of-sexp
    "C-M-e" 'puni-end-of-sexp
    "s-<backspace>" 'puni-force-delete)
  :config
  (setq puni-confirm-when-delete-unbalanced-active-region nil))

(use-package format-all
  :hook (((emacs-lisp-mode) . format-all-mode)
         ((prog-mode) . format-all-ensure-formatter))
  :config
  ;; (setq format-all-formatters '(("Vue" (prettier "--parser vue"))))
  (setq format-all-show-errors 'never))

;; DeleteBlockPac
(use-package delete-block
  :defer
  :commands (delete-block-for delete-bl-backward)
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))
;; -DeleteBlockPac

(use-package avy
  :diminish
  :demand t
  :commands (avy-goto-char avy-goto-line))

(use-package rime
  :after-call +my/first-input-hook-fun
  :defer t
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-after-ascii-char-p
     rime-predicate-space-after-cc-p))
  (rime-posframe-properties
   (list :internal-border-width 5))
  :config
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)
  (cond (*sys/mac* (setq rime-user-data-dir "~/.config/rime"
                         rime-librime-root "~/.local/share/librime/dist/"))
        (*sys/linux* (setq rime-user-data-dir "~/.rime")))
  (defun activate-default-input-method ()
    (interactive)
    (activate-input-method default-input-method))
  (add-hook 'text-mode-hook 'activate-default-input-method))

(use-package super-save
  :diminish
  :defer 0.5
  :init
  (setq auto-save-default nil)
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (add-to-list 'super-save-triggers 'switch-to-buffer)
  (add-to-list 'super-save-triggers 'eglot-rename)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 5)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)
  (super-save-mode 1)

  (defun +super-save-without-hook ()
    (let ((before-save-hook (remove 'format-all--buffer-from-hook before-save-hook)))
      (when (super-save-p)
        (save-all-buffers))))
  (advice-add 'super-save-command :override '+super-save-without-hook))

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode +lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not +lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max
            ;; DEPRECATED Remove check when 26 support is dropped
            (if (boundp 'read-process-output-max)
                (default-value 'read-process-output-max))
            +lsp--default-gcmh-high-cons-threshold
            (default-value 'gcmh-high-cons-threshold))
      ;; `read-process-output-max' is only available on recent development
      ;; builds of Emacs 27 and above.
      (setq-default read-process-output-max (* 1024 1024))
      ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
      ;;        native JSON library, so we up the GC threshold to stave off
      ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
      ;;        GC strategy, so we modify its variables rather than
      ;;        `gc-cons-threshold' directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (gcmh-set-high-threshold)
      (setq +lsp--optimization-init-p t))))

(use-package eglot
  :commands (+eglot-organize-imports +eglot-help-at-point)
  :hook (
         (eglot-managed-mode . (lambda ()
                                 (+lsp-optimization-mode)
                                 (setq eldoc-documentation-functions
                                       (cons #'flymake-eldoc-function
                                             (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                                 ;; Show all eldoc feedback.
                                 (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

                                 (if (or (boundp 'lsp-bridge-mode) (boundp 'lspce-mode))
                                     (setq completion-at-point-functions (remove #'eglot-completion-at-point completion-at-point-functions))
                                   (my/set-eglot-capf))
                                 (when (boundp 'lspce-mode)
                                   eglot-stay-out-of '(eldoc))
                                 ))
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'makefile-mode)
                          (eglot-ensure)))))
  :config
  (use-package consult-eglot
    :commands (consult-eglot-symbols))
  (setq
   ;; eglot-send-changes-idle-time 0.2
   eglot-send-changes-idle-time 0
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-confirm-server-initiated-edits nil
   eglot-sync-connect nil
   eglot-events-buffer-size 0
   ;; eglot-max-candidates 100
   )
  (setq eldoc-echo-area-use-multiline-p 5)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
  (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))

  (setq-default eglot-workspace-configuration '((:dart . (:completeFunctionCalls t :enableSnippets t))))

  ;; HACK Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
  ;;      more problematic approach of deferred to eldoc. Here, I've restored it.
  ;;      Doom's lookup handlers try to open documentation in a separate window
  ;;      (so they can be copied or kept open), but doing so with an eldoc buffer
  ;;      is difficult because a) its contents are generated asynchronously,
  ;;      making them tough to scrape, and b) their contents change frequently
  ;;      (every time you move your cursor).
  (defvar +eglot--help-buffer nil)
  (defun +eglot-lookup-documentation (_identifier)
    "Request documentation for the thing at point."
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (let ((blurb (and (not (seq-empty-p contents))
                        (eglot--hover-info contents range)))
            (hint (thing-at-point 'symbol)))
        (if blurb
            (with-current-buffer
                (or (and (buffer-live-p +eglot--help-buffer)
                         +eglot--help-buffer)
                    (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
              (with-help-window (current-buffer)
                (rename-buffer (format "*eglot-help for %s*" hint))
                (with-current-buffer standard-output (insert blurb))
                (setq-local nobreak-char-display nil)))
          (display-local-help))))
    'deferred)

  (defun +eglot-help-at-point()
    (interactive)
    (+eglot-lookup-documentation nil))

  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))


(use-package treesit
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :defer t
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          ))
  :config
  (setq major-mode-remap-alist
        '((javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (python-mode . python-ts-mode)
          (js-json-mode . json-ts-mode)
          (sh-mode . bash-ts-mode)))
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package devdocs
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :init
  (defvar devdocs-major-mode-docs-alist
    '((web-mode . ("Javascript" "Less" "HTML" "Vue.js~2" "CSS"))))
  (mapc
   (lambda (e)
     (add-hook (intern (format "%s-hook" (car e)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr e)))))
   devdocs-major-mode-docs-alist)
  :config
;;;###autoload
  (defun +devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))

;;;###autoload
  (defun +devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol)))

;;;###autoload
  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.
Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (str)
       (let* ((docs (split-string str " "))
              (doc (if (length= docs 1)
                       (downcase (car docs))
                     (concat (downcase (car docs)) "~" (downcase (cdr docs))))))
         (unless (and (file-directory-p devdocs-data-dir)
                      (directory-files devdocs-data-dir nil "^[^.]"))
           (message "Installing %s..." str)
           (devdocs-install doc))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t)))
  )

(use-package imenu
  :commands (imenu)
  :hook (imenu-after-jump . recenter))

(use-package xref
  :init
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :hook ((xref-after-return xref-after-jump) . recenter))

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(use-package vterm
  :commands (vterm--internal vterm-posframe-toggle)
  :init
  (setq vterm-always-compile-module t)
  (setq vterm-shell "/usr/local/bin/fish")
  (setq vterm-timer-delay 0.001
        process-adaptive-read-buffering nil)

  (with-no-warnings
    (defvar vterm-posframe--frame nil)

    (defun vterm-posframe-hidehandler (_)
      "Hidehandler used by `vterm-posframe-toggle'."
      (not (eq (selected-frame) posframe--frame)))

    (defun vterm-posframe-toggle ()
      "Toggle `vterm' child frame."
      (interactive)
      (let ((buffer (vterm--internal #'ignore 100)))
        (if (and vterm-posframe--frame
                 (frame-live-p vterm-posframe--frame)
                 (frame-visible-p vterm-posframe--frame))
            (progn
              (posframe-hide buffer)
              ;; Focus the parent frame
              (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
          (let ((width  (max 80 (/ (frame-width) 2)))
                (height (/ (frame-height) 2)))
            (setq vterm-posframe--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :hidehandler #'vterm-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :override-parameters '((cursor-type . t))
                   :accept-focus t))
            ;; Blink cursor
            (with-current-buffer buffer
              (save-excursion (vterm-clear t))
              (setq-local cursor-type 'box))
            ;; Focus the child frame
            (select-frame-set-input-focus vterm-posframe--frame)))))))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . (lambda ()
                         (process-query-on-exit-flag
                          (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (setq py-tab-indent nil)
  (add-hook 'python-mode-hook (lambda ()
                                (setq-local tab-width 4)))

  (use-package pyvenv
    :after python
    :defer t)

  (use-package py-isort
    :after python
    :defer t
    :hook (python-mode . (lambda ()
                           (add-hook 'before-save-hook #'py-isort-before-save)))))

(use-package web-mode
  :defer t
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.vue\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.wxml\\'")
  :custom
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-block-padding 0)
  (web-mode-part-padding 0)
  :init
  (defun my/web-vue-setup()
    (with-eval-after-load 'general
      (local-leader-def
        :keymaps 'web-mode-map
        "f" 'lsp-eslint-fix-all))
    (setq-local lsp-enable-imenu t)
    (make-local-variable 'before-save-hook)
    (with-eval-after-load 'lsp-eslint
      (add-hook 'before-save-hook 'lsp-eslint-fix-all)))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-comment-annotation t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-comment-formats '(("java" . "//") ("javascript" . "//") ("php" . "//")))
  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  ;; "-" as word so company completes kabeb-case
  (modify-syntax-entry ?_ "w" web-mode-syntax-table)
  (modify-syntax-entry ?- "w" web-mode-syntax-table)
  (modify-syntax-entry ?# "_" web-mode-syntax-table)
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'")))
  (add-hook 'web-mode-hook
            (lambda ()
              (cond ((equal web-mode-content-type "vue")
                     (my/web-vue-setup)))))

  (local-leader-def
    :keymaps 'web-mode-map
    "d" '(web-mode-element-vanish :wk "Remove current element")
    "D" '(web-mode-element-kill :wk "Remove region")
    "r" '(instant-rename-tag :wk "Rename current tag")
    )
  )

(use-package css-mode
  :mode ("\\.css\\'" "\\.wxss\\'")
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

;; EmmetPac
(use-package emmet-mode
  :defer t
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  ;; :bind (:map web-mode-map
  ;;             ("C-j" . emmet-expand-yas))
  :config
  (add-hook 'emmet-mode-hook (lambda()
                               (setq emmet-indent-after-insert t))))
;; -EmmetPac

(require 'comint)
(require 'subr-x)

(defvar +my/flutter--app-id-alist '())
(defvar +my/flutter--device-alist '())

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
  (when (string-prefix-p "finished" event)
    (kill-buffer (+my/flutter--buffer-name))))

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
           (command (+my/flutter--command mode app-id))
           (temp (mapcar 'concat process-environment))
           (process-environment (setenv-internal temp "PUB_HOSTED_URL" +my/flutter-pub-host t)))
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
                              (when (string-prefix-p "finished" event)
                                (kill-buffer "*Flutter Pub Get*")))
                  )
    )
  (cd (file-name-directory buffer-file-name))
  (display-buffer "*Flutter Pub Get*")
  )

(use-package dart-mode
  :mode ("\\.dart\\'")
  :hook ((dart-mode . (lambda ()
                        (format-all-mode t)))
         (eglot-managed-mode . (lambda ()
                                 (add-hook 'before-save-hook #'+eglot-organize-imports nil t)))
         )
  :config

  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(dart-mode :types
                                                   ((?c "Class"    font-lock-type-face)
                                                    (?e "Enum" font-locl-type-face)
                                                    (?V "Constructor" font-lock-type-face)
                                                    (?C "Constant"    font-lock-constant-face)
                                                    (?f "Function"  font-lock-function-name-face)
                                                    (?m "Method"  font-lock-function-name-face)
                                                    (?p "Property" font-lock-variable-name-face)
                                                    (?F "Field"  font-lock-variable-name-face)))))

  (local-leader-def
    :keymaps 'dart-mode-map
    "r" '(+my/flutter-run-or-hot-reload :wk "Run or hot reload")
    "R" '(+my/flutter-run-or-hot-restart :wk "Run or hot restart")

    "v" '(+my/flutter-open-devtools :wk "Open devtools")
    "Q" '(+my/flutter-quit :wk "Quit application")

    "s" '(+my/flutter-run-or-attach :wk "Run or Attach")
    "p" '(+my/flutter-pub-get :wk "Pub get")
    ))

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

(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-image-actual-width nil
        org-list-description-max-indent 4
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{}
        ))

(defun +org-init-agenda-h ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-inhibit-startup nil
   org-agenda-skip-unavailable-files t
   ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
   ;; better context instead of just the current week which is a bit confusing
   ;; on, for example, a sunday
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"))

(defun +org-init-capture-defaults-h()
  ;; enter insert state for org-capture
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  )

;;;###autoload
(defun +org-update-cookies-h ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))



(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands (+my/open-org-agenda)
  :defer t
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
  (defun +my/open-org-agenda ()
    "open org agenda in left window"
    (interactive)
    (org-agenda nil "n"))
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
  :after org
  :defer t
  :commands (org-download-clipboard org-download-delete org-download-image org-download-yank org-download-edit org-download-rename-at-point org-download-rename-last-file org-download-screenshot)
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil)
  :config
  (cond (*sys/mac*
         (setq org-download-screenshot-method "screencapture -i %s"))))
;; -OrgDownload

(use-package org-contrib
  :defer t
  :after org)

(use-package valign
  :after org
  :defer t
  :bind (([remap org-table-align] . valign-table))
  :hook (org-mode . valign-mode)
  )

(use-package electric-spacing
  :defer t
  :after org)

(use-package separate-inline
  :defer t
  :after org
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
  (defalias 'org-align-tags #'ignore))


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

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
