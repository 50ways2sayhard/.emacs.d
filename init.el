;;; package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;; init.el --- user-init-file               -*- lexical-binding: t -*-

;;; Code:

;;; Early init
(progn
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  )

;;; native-comp 的 libgccjit 在真正启动编译后端前读取 LIBRARY_PATH。
;;; 不把它放进 Emacs 主进程环境：同步编译通过 comp--final 的局部环境传入，
;;; 异步 worker 通过 comp--run-async-workers 继承，并在 worker 内再次设置。
(defvar my/native-comp-library-path nil
  "供 native-comp 和 GCC 子进程使用的完整 LIBRARY_PATH。")

(require 'cl-lib)
(setq my/native-comp-library-path
      (let* ((library-path (getenv "LIBRARY_PATH"))
             (arch-dir (if (string-match-p
                            "\\`\\(aarch64\\|arm64\\)"
                            system-configuration)
                           "lib-arm64-11"
                         "lib-x86_64-11"))
             (bundled-lib-dir
              (expand-file-name (concat arch-dir "/libgccjit")
                                invocation-directory))
             (bundled-emutls
              (expand-file-name "libemutls_w.a"
                                (expand-file-name "apple-darwin"
                                                  bundled-lib-dir)))
             (bundled-library-path
              (mapconcat #'identity
                         (cl-remove-if-not
                          #'file-directory-p
                          (mapcar (lambda (suffix)
                                    (expand-file-name suffix bundled-lib-dir))
                                  '("" "apple-darwin" "sdk-libs")))
                         path-separator))
             (gcc (executable-find "gcc-16"))
             (homebrew-emutls (and gcc
                                   (string-trim
                                    (shell-command-to-string
                                     (format "%s -print-file-name=libemutls_w.a"
                                             (shell-quote-argument gcc)))))))
        (cond
         ((and (file-exists-p bundled-emutls)
               (not (string= bundled-library-path "")))
          bundled-library-path)
         ((and homebrew-emutls (file-exists-p homebrew-emutls))
          (if library-path
              (concat library-path path-separator
                      (file-name-directory homebrew-emutls))
            (file-name-directory homebrew-emutls)))
         (t library-path))))

;; The Emacs launcher may have installed exactly this path for libgccjit.
;; Keep it out of unrelated subprocesses; preserve any user-provided path.
(let ((library-path (getenv "LIBRARY_PATH")))
  (when (and library-path
             my/native-comp-library-path
             (equal
              (mapcar #'file-name-as-directory
                      (split-string library-path path-separator t))
              (mapcar #'file-name-as-directory
                      (split-string my/native-comp-library-path
                                    path-separator t))))
    (setenv "LIBRARY_PATH" nil)))

;; CC/CXX 只对 make/autoconf 这类构建工具有意义：native-comp 走 libgccjit，
;; 不读这两个变量（comp.el 只认 NATIVE_DISABLED），留在子进程环境里只会让
;; pip 编译扩展、Xcode 工具链误用 Homebrew GCC。
;; env 文件是 `make env`（printenv 转储）生成的，会在下面 "Load env" 处重新
;; 注入，所以加载完之后还要再清一次。
(defun my/clear-compiler-env ()
  "Drop the ambient CC/CXX from the Emacs subprocess environment."
  (dolist (var '("CC" "CXX"))
    (setenv var nil)))

(my/clear-compiler-env)

(defun my/native-comp--with-library-path (orig-fun &rest args)
  "Run the native-comp backend with its private GCC runtime path."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LIBRARY_PATH" my/native-comp-library-path)
    (apply orig-fun args)))

(with-eval-after-load 'comp
  (unless (advice-member-p #'my/native-comp--with-library-path 'comp--final)
    (advice-add 'comp--final :around #'my/native-comp--with-library-path)))

(with-eval-after-load 'comp-run
  (unless (advice-member-p #'my/native-comp--with-library-path
                           'comp--run-async-workers)
    (advice-add 'comp--run-async-workers
                :around #'my/native-comp--with-library-path)))

(setq native-comp-async-env-modifier-form
      `(setenv "LIBRARY_PATH" ,my/native-comp-library-path))

;;; Package Manager
(progn
  (defvar elpaca-installer-version 0.12)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                :ref nil :depth 1 :inherit ignore
                                :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                :build (:not elpaca-activate)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
         (build (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (<= emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                    ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                    ,@(when-let* ((depth (plist-get order :depth)))
                                                        (list (format "--depth=%d" depth) "--no-single-branch"))
                                                    ,(plist-get order :repo) ,repo))))
                    ((zerop (call-process "git" nil buffer t "checkout"
                                          (or (plist-get order :ref) "--"))))
                    (emacs (concat invocation-directory invocation-name))
                    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                    ((require 'elpaca))
                    ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order)))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t))


(mapcar
 (lambda (p) (add-to-list 'elpaca-ignored-dependencies p))
 '(xref tramp tramp-sh tramp-adb flymake simple diff-mode smerge-mode python css-mode custom
        server help elec-pair paren recentf winner tab-bar hl-line pulse prog-mode
        lisp-mode treesit imenu eldoc hippie-exp eglot autorevert))

;; Block until current queue processed.
(elpaca-wait)

(use-package benchmark-init
  :demand t
  :config
  (add-hook '+my/first-input-hook 'benchmark-init/deactivate))

(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

;;;; :after-call for use-package
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
                 (when-let* ((deferral-list (assq ',name +use-package--deferred-pkgs)))
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

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash
  :config (global-dash-fontify-mode))

(use-package autorevert
  :ensure nil
  :diminish
  :custom
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (auto-revert-use-notify nil)
  (auto-revert-interval 1) ; Auto revert every 3 sec
  :hook (elpaca-after-init . global-auto-revert-mode))

;;; Custom settings
(use-package custom
  :ensure nil
  :no-require t
  :config
  (setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;;; Server mode
(use-package server
  :ensure nil
  :defer 5
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

;;; Print startup time
(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Diff
(use-package diff-hl
  :commands (diff-hl-revert-hunk diff-hl-show-hunk diff-hl-update)
  :hook ((window-setup . global-diff-hl-mode))
  :bind
  (:map diff-hl-mode-map
        ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-update-async t)
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  :config
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun +diff-hl-update-after-focus-change ()
    (cond ((bound-and-true-p diff-hl-mode)
           (diff-hl-update))
          ((bound-and-true-p diff-hl-dir-mode)
           (diff-hl-dir-update))
          ((bound-and-true-p diff-hl-dired-mode)
           (diff-hl-dired-update))
          (t t)))
  (add-function :after after-focus-change-function
                #'+diff-hl-update-after-focus-change)

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  )

(use-package diff-mode
  :ensure nil
  :config
  (setq diff-font-lock-prettify t)
  (setq diff-font-lock-syntax 'hunk-also))

;;; Dired and Dirvish file browser
(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("'" . +my/quick-look)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("l" . #'dired-find-alternate-file)
        ("h" . #'dired-up-directory))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (setq dired-open-extensions
        (mapcar (lambda (ext)
                  (cons ext "open")) '("pdf" "doc" "docx" "ppt" "pptx"))))

(use-package savehist
  :ensure nil
  :hook ((elpaca-after-init . savehist-mode)
         (elpaca-after-init . save-place-mode)))

(use-package casual
  :commands (casual-dired-tmenu casual-dired-sort-by-tmenu casual-dired-search-replace-tmenu)
  :config
  (require 'casual-dired)
  (require 'casual-agenda)
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)
              ("s" . #'casual-dired-sort-by-tmenu)
              ("/" . #'casual-dired-search-replace-tmenu))
  (:map org-agenda-mode-map
        ("C-o" . #'casual-agenda-tmenu)))

(use-package dirvish
  :after dired
  :hook (+my/first-input . dirvish-override-dired-mode)
  :bind
  (:map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
        ("?"   . dirvish-dispatch)
        ("a"   . dirvish-quick-access)
        ("f"   . dirvish-file-info-menu)
        ("c"   . dirvish-yank-menu)
        ("N"   . dirvish-narrow)
        ("^"   . dirvish-history-last)
        ("H"   . dirvish-history-jump) ; remapped `describe-mode'
        ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
        ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
        ("q"   . dirvish-quit)
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
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-side-follow-buffer-file t)
  (dirvish-attributes '(nerd-icons file-size vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  :config
  (when (boundp 'dirvish-side-follow-mode)
    (dirvish-side-follow-mode t))
  (use-package dirvish-extras
    :ensure nil
    :after dirvish)

  (use-package dirvish-vc
    :ensure nil
    :after dirvish)

  (use-package dirvish-history
    :ensure nil
    :after dirvish))

;;; Documentation in echo area
(use-package eldoc
  :ensure nil
  :commands (eldoc)
  :config
  (global-eldoc-mode))

(use-package help
  :ensure nil
  :config (temp-buffer-resize-mode))

;;; Version control
;;;; Magit
(use-package magit
  :ensure (:files (:defaults "lisp/*.el"))
  :commands (magit-status magit-open-repo magit-add-section-hook aborn/simple-git-commit-push magit-add-current-buffer-to-kill-ring)
  :custom
  (magit-diff-refine-hunk t)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-process-connection-type nil)
  :bind
  (:map magit-mode-map
        ("Q" . magit-mode-bury-buffer)
        ("x" . magit-discard)
        ("N" . magit-section-forward)
        ("P" . magit-section-backward)
        ("p" . magit-push)
        ("'" . magit-process-buffer))
  :config
  (use-package magit-prime
    :diminish
    :hook elpaca-after-init)
  ;; (setq magit-display-buffer-function #'+magit-display-buffer-fn)
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

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (defun magit-add-current-buffer-to-kill-ring ()
    "Show the current branch in the echo-area and add it to the `kill-ring'."
    (interactive)
    (let ((branch (magit-get-current-branch)))
      (if branch
          (progn (kill-new branch)
                 (message "%s" branch))
        (user-error "There is not current branch")))))

;;;; Multi-repo Magit
(use-package multi-magit
  :ensure (:host github :repo "luismbo/multi-magit")
  :after magit)

;;;; Forge
(use-package forge
  :config
  (push '("gitlab.futunn.com"               ; GITHOST
          "gitlab.futunn.com/api/v4"        ; APIHOST
          "gitlab.futunn.com"               ; WEBHOST and INSTANCE-ID
          forge-gitlab-repository)    ; CLASS
        forge-alist)
  )

;;;; Blame
(use-package blame-reveal
  :ensure (:host github :repo "LuciusChen/blame-reveal")
  :config
  (setq blame-reveal-style 'right-fringe)
  ;; Smart time-based selection
  (setq blame-reveal-recent-days-limit 'auto)
  (setq blame-reveal-gradient-quality 'auto)

  ;; Display
  (setq blame-reveal-header-style 'block)
  (setq blame-reveal-show-uncommitted-fringe nil)  ; Use with diff-hl

  ;; Performance
  (setq blame-reveal-async-blame 'auto)
  (setq blame-reveal-lazy-load-threshold 3000)

  ;; Enable recursive blame
  (require 'blame-reveal-recursive))

(use-package transient
  :ensure t)

(setq split-width-threshold 240)
(setq split-window-preferred-direction 'horizontal)
(setq warning-display-at-bottom nil)

;;;; Browse at remote
(use-package browse-at-remote
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

;;;; Webjump
(use-package webjump
  :ensure nil
  :config (setq webjump-sites
                '(;; Emacs
                  ("Emacs Home Page" .
                   "www.gnu.org/software/emacs/emacs.html")
                  ("Mastering Emacs" .
                   "https://www.masteringemacs.org/")

                  ;; Search engines.
                  ("Google" .
                   [simple-query "www.google.com"
                                 "www.google.com/search?q=" ""])

                  ("Wikipedia" .
                   [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])

                  ;; Documentations
                  ("Flutter" .
                   [simple-query "api.flutter.dev/flutter" "api.flutter.dev/flutter/search.html?q=" ""])

                  ;; Programming languages
                  ("Pub" .
                   [simple-query "pub.dev" "pub.dev/packages?q=" ""])
                  )))

;;;; Merge conflicts
(use-package smerge-mode
  :ensure nil
  :after magit
  :diminish
  :bind (:map smerge-mode-map
              ("C-c m" . smerge-menu))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-menu))))
         )
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t))
  )

(use-package smerge-menu
  :ensure (:repo "KarimAziev/smerge-menu" :host github)
  :after smerge-mode)

;;;; Git timemachine
(use-package git-timemachine
  :ensure (:host github :repo "emacsmirror/git-timemachine")
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


;;; Parenthesis
(use-package elec-pair
  :ensure nil
  :hook (+my/first-input . electric-pair-mode)
  :config
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            #'(lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t
                                 (,electric-pair-inhibit-predicate c)))))))

(use-package puni
  :hook ((emacs-lisp-mode markdown-mode org-mode prog-mode) . puni-mode)
  :bind
  (:map puni-mode-map
        ("DEL" . puni-backward-delete-char)
        ("C-d" . puni-forward-delete-char)
        ("C-k" . puni-kill-line)
        ("M-h" . puni-force-delete)
        ("C-M-f" . puni-forward-sexp)
        ("C-M-b" . puni-backward-sexp)
        ("C-M-a" . puni-beginning-of-sexp)
        ("C-M-e" . puni-end-of-sexp))
  :config
  (setq puni-confirm-when-delete-unbalanced-active-region nil))

(use-package paren
  :ensure nil
  :hook (elpaca-after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis
        blink-matching-paren-highlight-offscreen t
        show-paren-context-when-offscreen 'child-frame
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t))

;;; After initialization
(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'elpaca-after-init-hook
            #'(lambda ()

                (message
                 "Loading %s...done (%.3fs) [after-init]" user-init-file
                 (float-time (time-subtract (current-time)
                                            before-user-init-time)))) t)

  (add-hook 'window-setup-hook
            #'(lambda ()
                (message
                 "Initialization done in (%.3fs)"
                 (float-time (time-subtract (current-time) before-user-init-time)))) t))

(defvar +my/first-input-hook nil)
(defun +my/first-input-hook-fun ()
  "Hook for first input."
  (when +my/first-input-hook
    (run-hooks '+my/first-input-hook)
    (setq +my/first-input-hook nil))
  (remove-hook 'pre-command-hook '+my/first-input-hook-fun))
(add-hook 'pre-command-hook '+my/first-input-hook-fun)

;;;; Load env
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
          (my/clear-compiler-env)
        envvars)))))

;;; Global configuration
;;;; Encoding and platform
(progn
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

  (defconst *rg*
    (executable-find "rg")
    "Is ripgrep installed?")

  (setq-default indent-tabs-mode nil)
  (setq-default indent-line-function 'insert-tab)
  (setq-default tab-width 2)

  (windmove-default-keybindings 'meta)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq mac-command-modifier 'meta) ; make cmd key do Meta
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq mac-control-modifier 'control) ; make Control key do Control
  (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

  (setq display-line-numbers-type 'relative)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (setq blink-cursor-mode nil)
  (setq word-wrap t
        word-wrap-by-category t
        require-final-newline t)
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq redisplay-skip-fontification-on-input t)

  (setq save-interprogram-paste-before-kill t)
  (setq set-mark-command-repeat-pop t)

  ;; Vertical Scroll
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000
        auto-window-vscroll nil
        scroll-preserve-screen-position t
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        )
  (when (display-graphic-p)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
          mouse-wheel-scroll-amount-horizontal 1
          mouse-wheel-progressive-speed nil))
  ;; (setq scroll-up-aggressively 0.01)
  ;; (setq scroll-down-aggressively 0.01)
  (setq mouse-wheel-progressive-speed t)
  (mouse-wheel-mode -1)
  (pixel-scroll-precision-mode)
  (defun filter-mwheel-always-coalesce (orig &rest args)
    "A filter function suitable for :around advices that ensures only
   coalesced scroll events reach the advised function."
    (if mwheel-coalesce-scroll-events
        (apply orig args)
      (setq mwheel-coalesce-scroll-events t)))

  (defun filter-mwheel-never-coalesce (orig &rest args)
    "A filter function suitable for :around advices that ensures only
   non-coalesced scroll events reach the advised function."
    (if mwheel-coalesce-scroll-events
        (setq mwheel-coalesce-scroll-events nil)
      (apply orig args)))

                                        ; Don't coalesce for high precision scrolling
  (advice-add 'pixel-scroll-precision :around #'filter-mwheel-never-coalesce)

                                        ; Coalesce for default scrolling (which is still used for horizontal scrolling)
                                        ; and text scaling (bound to ctrl + mouse wheel by default).
  (advice-add 'mwheel-scroll          :around #'filter-mwheel-always-coalesce)
  (advice-add 'mouse-wheel-text-scale :around #'filter-mwheel-always-coalesce)

  (setq pixel-scroll-precision-interpolate-page t)
  (defun +pixel-scroll-interpolate-down (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
      (pixel-scroll-interpolate-down)))

  (defun +pixel-scroll-interpolate-up (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
    (pixel-scroll-interpolate-up))

  (defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command '+pixel-scroll-interpolate-up)
  ;; Horizontal Scroll
  (setq hscroll-step 1)
  (setq hscroll-margin 1)
  ;; -SmoothScroll
  )
;;; Helper functions
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun +open-configuration-folder ()
  "Open configuration folder."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun +my-rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory (file-name-nondirectory buffer-file-name)))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      ;; (set-visited-file-name file-to)
      ;; (rename-buffer file-to)
      ;; (save-buffer)
      (kill-buffer)
      (find-file file-to))))

(defun jump-out-of-pair ()
	(interactive)
	(let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
		(when found
			(cond ((or (looking-back "\\*\\*" 2)
		             (looking-back "``" 2)
		             (looking-back "''" 2)
		             (looking-back "==" 2))
			       (forward-char))
			      (t (forward-char 0))))))

(defun +my-delete-file ()
  "Put current buffer file to top."
  (interactive)
  (delete-file
   (read-file-name "Delete: " default-directory (file-name-nondirectory buffer-file-name)))
  (unless (file-exists-p (buffer-file-name))
    (kill-current-buffer)))

(defun +my-imenu ()
  "In 'org-mode', call 'consult-outline'.Otherwise call 'consult-imenu'."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (consult-org-heading)
    (consult-imenu)))

(defun +default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))
(global-set-key (kbd "C-<return>") '+default/newline-below)
(global-set-key (kbd "C-S-<return>") '+default/newline-above)
(global-set-key (kbd "M-<return>") 'indent-new-comment-line)

(defun +default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

(defun +my/google-it (&optional word)
  "Google WORD."
  (interactive)
  (let ((target (read-string "Search for: ")))
    (browse-url (concat "http://www.google.com/search?q="
			                  (url-hexify-string target)))))

(defun +my/open-in-osx-finder ()
  "Open file in finder."
  (interactive)
  (let* ((file (read-file-name "Open:" default-directory buffer-file-name))
         (file (expand-file-name file))
         (script (concat
	                "set thePath to POSIX file \"" file "\"\n"
	                "tell application \"Finder\"\n"
	                " set frontmost to true\n"
	                " reveal thePath \n"
	                "end tell\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun +my/quick-look (&optional file)
  "Open FILE with quick look."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (car (dired-get-marked-files))
      (read-file-name "File:" default-directory buffer-file-name))))
  (call-process-shell-command (concat "qlmanage -p " (expand-file-name file))))

;;; Evil
(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))

(use-package meow
  :hook (window-setup . (lambda ()
                          (require 'meow-config)
                          (meow-setup)
                          (meow-global-mode)))
  :config
  ;; (add-hook 'meow-insert-exit-hook (lambda () (setq flymake-no-changes-timeout 0)))
  ;; (add-hook 'meow-insert-enter-hook (lambda () (setq flymake-no-changes-timeout nil)))
  (setq meow-keypad-leader-dispatch "C-c")
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(comint-mode . insert))
  (add-to-list 'meow-mode-state-list '(magit-blame-mode . insert))
  (add-to-list 'meow-mode-state-list '(ediff-mode . insert))
  (add-to-list 'meow-mode-state-list '(diff-mode . insert))
  (add-to-list 'meow-mode-state-list '(occur-mode . motion))
  (add-to-list 'meow-mode-state-list '(ghostel-mode . insert))
  (add-hook 'org-capture-mode-hook #'meow-insert)
  (add-hook 'git-timemachine-mode-hook #'meow-insert))

(use-package meow-tree-sitter
  :after meow
  :config
  (meow-tree-sitter-register-defaults)
  (meow-tree-sitter-register-thing ?C "class"))

(use-package bind
  :after-call +my/first-input-hook-fun
  :ensure (:host github :repo "repelliuss/bind")
  :functions (bind))

(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :hook (window-setup . which-key-mode))

;;; Minibuffer completion
;;;; Embark
(use-package embark
  :ensure (embark :files (:defaults "*.el"))
  :commands (my/embark-magit-status)
  :after-call +my/first-input-hook-fun
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)
   :map minibuffer-mode-map
   ("C-/" . embark-export)      ;; alternative for `describe-bindings'
   :map embark-file-map
   ("r" . rename-visited-file)
   ("d" . +my-delete-file)
   ("X" . +my/open-in-osx-finder)
   ("g" . my/embark-magit-status)
   ("SPC" . +my/quick-look)
   :map embark-become-file+buffer-map
   ("F" . consult-fd)
   :map embark-region-map
   ("V" . diff-hl-show-hunk)
   ("/" . evilnc-comment-or-uncomment-lines)
   ("=" . er/expand-region)
   (";" . embrace-commander)
   ("Y". #'evilnc-copy-and-comment-lines)
   ("R" . visual-replace)
   :map embark-identifier-map
   (";" . embrace-commander)
   ("R" . visual-replace)
   ("D" . xref-find-definitions-other-window))
  :custom
  (embark-cycle-key ".")
  (embark-help-key "?")
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (defun my/embark-magit-status (path)
    (interactive "fPath: ")
    (require 'magit)
    (let* ((dir (if (file-directory-p path)
                    (file-name-as-directory path)
                  (file-name-directory path)))
           (top (magit-toplevel dir)))
      (if top
          (magit-status top)
        (message "Not inside a Git repository: %s" dir))))
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

  (add-to-list 'embark-target-injection-hooks
               '(visual-replace embark--allow-edit))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; smerge integration
  (defun embark-target-smerge-at-point ()
    (when-let* (((bound-and-true-p smerge-mode))
                (ov (cl-find-if (lambda (ov) (eq (overlay-get ov 'smerge) 'conflict))
                                (overlays-at (point)))))
      `(smerge-diff "conflict" ,(overlay-start ov) . ,(overlay-end ov))))

  (add-to-list 'embark-keymap-alist '(smerge-diff . smerge-basic-map))
  (add-to-list 'embark-target-finders 'embark-target-smerge-at-point)
  (add-to-list 'embark-repeat-actions 'smerge-next)
  (add-to-list 'embark-repeat-actions 'smerge-prev)

  ;; ffap-menu candidates use category `ffap-menu', not `file'.
  (defun +embark-ffap-menu-target (_type target)
    (if (and (fboundp 'ffap-url-p) (ffap-url-p target))
        (cons 'url target)
      (cons 'file (abbreviate-file-name (expand-file-name target)))))
  (add-to-list 'embark-transformer-alist '(ffap-menu . +embark-ffap-menu-target)))

(use-package embark-consult
  :after consult
  :config
  (define-key minibuffer-local-map (kbd "M-.") #'my-embark-preview)
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim))))))

;;;; Vertico
(use-package vertico
  :ensure (vertico :files (:defaults "extensions/vertico-*.el"))
  :hook (+my/first-input . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :config
  ;; Configure directory extension.
  (use-package vertico-quick
    :ensure nil
    :after vertico
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))

  (use-package vertico-repeat
    :ensure nil
    :after vertico
    :bind ("C-c r" . vertico-repeat)
    :hook (minibuffer-setup . vertico-repeat-save))

  (use-package vertico-directory
    :ensure nil
    :after vertico
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)
                ("C-w" . vertico-directory-up))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;;;; Minibuffer defaults
(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (completion-cycle-threshold 3)
  (tab-always-indent 'completion)
  (elisp-fontify-semantically t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (when *sys/mac*
    (setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;;;; Consult
(use-package consult
  :after orderless
  :commands (consult-clock-in +consult-ripgrep-current-directory)
  :bind (([remap recentf-open-files] . consult-recent-file)
         ([remap imenu] . consult-imenu)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap swich-to-buffer-other-window] . consult-buffer-other-window)
         ([remap goto-line] . consult-goto-line)
         ([remap yank-pop] . consult-yank-from-kill-ring)
         ([remap bookmark-jump] . consult-bookmark)
         ("M-?" . consult-ripgrep)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-s bindings (search-map)
         ;; ("M-s m" . consult-multi-occur)
         )
  :config
  (require 'embark-consult)
  (setq consult-preview-key '(:debounce 1.0 any))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-args "fd --color=never --full-path ARG OPTS")
  (setq consult-ripgrep-args
        "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")
  (consult-customize consult-source-recent-file :narrow ?r :name "Recent File")

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :state    ,#'consult--buffer-state
                :hidden   t
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

  (defun +consult-ripgrep-current-directory (&optional initial)
    (interactive)
    (consult-ripgrep default-directory initial))

  (defun maple/consult-git ()
    "Find file in the current Git repository."
    (interactive)
    (let* ((default-directory (project-root (project-current)))
           (cmd "git ls-files -z --full-name --")
           (cands (split-string (shell-command-to-string cmd) "\0" t))
           (file (completing-read "Find file: " (project--file-completion-table cands) nil t)))
      (find-file file)))

  ;;  HACK: for `breadcrumb' and `eglot'
  (defun my-consult-imenu--compute ()
    "Compute imenu candidates."
    (consult--forbid-minibuffer)
    (let* ((imenu-use-markers t)
           ;; Generate imenu, see `imenu--make-index-alist'.
           (items (imenu--truncate-items
                   (save-excursion
                     (without-restriction
                       (funcall imenu-create-index-function)))))
           (config (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-config))))
      ;; Fix toplevel items, e.g., emacs-lisp-mode toplevel items are functions
      (when-let* ((toplevel (plist-get config :toplevel))
                  (tops (seq-remove (lambda (x) (listp (cdr x))) items))
                  (rest (seq-filter (lambda (x) (listp (cdr x))) items)))
        (setq items (nconc rest (and tops (list (cons toplevel tops))))))
      ;; Apply our flattening in order to ease searching the imenu.
      (let ((fn (if (and (boundp 'eglot--managed-mode) eglot--managed-mode) #'consult-imenu--flatten-eglot #'consult-imenu--flatten)))
        (funcall fn
                 nil nil items
                 (mapcar (pcase-lambda (`(,x ,y ,z)) (list y x z))
                         (plist-get config :types)))
        )
      ))

  (advice-add 'consult-imenu--compute :override #'my-consult-imenu--compute)

  (defun consult-imenu--create-key-name-eglot (prefix item types)
    "Create a key-name with optional prefix and type annotations."
    (let* ((name (copy-sequence (car item)))
           (name-type (get-text-property 0 'breadcrumb-kind name))
           (type (assoc name-type types))
           (pos (consult-imenu--normalize (or (car (get-text-property 0 'breadcrumb-region name)) (cdr item))))
           (key-name (concat (when prefix (concat prefix " ")) name)))

      (when type
        (add-face-text-property (if prefix (1+ (length prefix)) 0) (length key-name)
                                (nth 2 type) 'append key-name)

        (setq key-name (concat (car type) " " key-name))
        (put-text-property 0 (length (car type)) 'consult--type (nth 1 type) key-name))

      (list (cons key-name pos))))


  (defun consult-imenu--flatten-eglot (prefix face list types)
    "Flatten imenu LIST.
PREFIX is prepended in front of all items.
FACE is the item face.
TYPES is the mode-specific types configuration."
    (mapcan
     (lambda (item)
       (if (and (consp item) (stringp (car item)) (integer-or-marker-p (cdr item)))
           (consult-imenu--create-key-name-eglot prefix item types)

         (progn
           (append
            (consult-imenu--create-key-name-eglot prefix item types)

            (let* ((name (concat (car item)))
                   (next-prefix (if prefix (concat prefix "/" name) name)))
              (add-face-text-property 0 (length name)
                                      'consult-imenu-prefix 'append name)

              (consult-imenu--flatten-eglot next-prefix face (cdr item) types))))))
     list))

  (defvar consult-source-project-file
    `( :name     "Project Files"
       :narrow   ?f
       :category file
       :face     consult-file
       :history  file-name-history
       :state    ,#'consult--file-state
       :new
       ,(lambda (file)
          (consult--file-action
           (expand-file-name file (consult--project-root))))
       :enabled
       ,(lambda ()
          consult-project-function)
       :items ,#'consult-project--project-files)
    "Project file source for `consult-buffer'.")

  (defvar consult-source-recent-file-hidden
    `(:hidden t :narrow ((?R . "Recent Files"))
              ,@consult-source-recent-file))
  (setq consult-project-buffer-sources '(consult-source-project-buffer-hidden
                                         consult-source-project-file
                                         consult-source-recent-file-hidden
                                         consult-source-project-root-hidden))

  (defun consult-project--project-files ()
    "Compute the project files given the ROOT."
    (let* ((root (consult--project-root))
           (project (project--find-in-directory root))
           (files (project-files project)))
      (mapcar (lambda (f)
                (let* ((filename (file-relative-name f root))
                       (abs-filename (expand-file-name f root))
                       (result filename))
                  (propertize result 'multi-category `(file . ,(abbreviate-file-name abs-filename))))) files)))

  )

;;;; Directory jump
(use-package zoxide
  :commands (+zoxide-cd dired-jump-with-zoxide)
  :config
  (defun +zoxide-cd ()
    (interactive)
    (cd (completing-read "path:" (zoxide-query) nil t)))
  (defun dired-jump-with-zoxide ()
    (interactive)
    (if (equal current-prefix-arg nil)
        (zoxide-open-with nil (lambda (file) (dired file)) t)
      (zoxide-open-with nil (lambda (file) (dired-other-window file)) t)))
  :bind
  (:map dired-mode-map
        ("z" . dired-jump-with-zoxide)))

(use-package consult-dir
  :commands (consult-dir consult-dir-jump-file)
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (setq consult-dir-default-command 'dirvish-dwim)
  (defun consult-dir--zoxide-dirs ()
    "Return list of fasd dirs."
    (split-string (shell-command-to-string "zoxide query -l | head -n 50") "\n" t))
  (defvar consult-dir--source-zoxide
    `(:name     "zoxide dirs"
                :narrow   ?z
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (executable-find "zoxide"))
                :items    ,#'consult-dir--zoxide-dirs)
    "Fasd directory source for `consult-dir'.")
  (setq consult-dir-sources '(consult-dir--source-recentf consult-dir--source-zoxide consult-dir--source-project)))

;;;; Thing-at-point helpers
;; from: https://github.com/Elilif/.elemacs/blob/master/lisp/init-completion.el
(progn
  (defvar mcfly-commands
    '(consult-line
      consult-outline
      consult-git-grep
      consult-ripgrep
      noct-consult-ripgrep-or-line
      +my/google-it))

  (defvar mcfly-back-commands
    '(self-insert-command
      yank
      yank-pop
      org-yank))

  (defun mcfly-back-to-present ()
    (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
    (cond ((and (memq last-command mcfly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((memq this-command mcfly-back-commands)
           (delete-region (point) (point-max)))))

  (defun mcfly-time-travel ()
    (when (memq this-command mcfly-commands)
      (let ((pre-insert-string (with-minibuffer-selected-window
                                 (or (seq-some
                                      (lambda (thing) (thing-at-point thing t))
					                            '(region url symbol))
					                           ;; '(symbol url region sexp))
			                               ""))))
        (save-excursion
          (insert (propertize pre-insert-string 'face 'shadow))))
      (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))

  ;; setup code
  (add-hook 'minibuffer-setup-hook #'mcfly-time-travel))

;;;; Orderless
(use-package orderless
  :commands (orderless-literal-prefix)
  :after-call elpaca-after-init-hook
  :demand t
  :config
  (require 'orderless-kwd)
  (with-eval-after-load 'orderless
    (add-to-list 'orderless-style-dispatchers 'orderless-kwd-dispatch t))
  (setq completion-styles '(orderless flex)
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless))))

  ;; Option 2: Undo the Eglot modification of completion-category-defaults
  (with-eval-after-load 'eglot
    (setq completion-category-defaults nil))
  )

;;;; Marginalia
(use-package marginalia
  :hook (+my/first-input . marginalia-mode)
  :config
  :bind (:map minibuffer-local-completion-map
              ("C-i" . marginalia-cycle)))

;;;; Posframe
(use-package posframe
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    ;; Local copy of patches/posframe-avoid-posn-at-point-reentry.patch
    ;; Skip posn-at-point only during IME re-entry (crash path), not
    ;; whenever an input method is merely active.
    ;; ponytail: cache last real posn; wrap estimate ignores word-breaks.
    (defvar my/posframe--skip-posn nil)
    (defvar my/posframe--posn-cache nil)
    (defun my/posframe--cache-posn ()
      (when (and input-method-function
                 (not my/posframe--skip-posn)
                 (not inhibit-redisplay))
        (let ((w (selected-window))
              (p (posn-at-point)))
          (when (and (window-live-p w) p)
            (setq my/posframe--posn-cache
                  (list w (window-start w) (point) (posn-x-y p)))))))
    (add-hook 'post-command-hook #'my/posframe--cache-posn)
    (defun my/posframe--estimate-xy (window pos)
      (with-current-buffer (window-buffer window)
        (let ((ww (max 1 (window-body-width window)))
              (fw (window-font-width window))
              (fh (window-font-height window))
              (trunc truncate-lines)
              (hs (window-hscroll window))
              (x 0)
              (y 0))
          (save-excursion
            (goto-char (window-start window))
            (while (< (point) pos)
              (let* ((beg (point))
                     (eol (line-end-position))
                     (hit (<= pos eol))
                     (cols (string-width
                            (buffer-substring-no-properties
                             beg (if hit pos eol)))))
                (if hit
                    (progn
                      (setq x (* (max 0 (- (if trunc cols (mod cols ww)) hs)) fw)
                            y (+ y (* fh (if trunc 0 (/ cols ww)))))
                      (goto-char pos))
                  (setq y (+ y (* fh (if trunc 1 (max 1 (/ (+ cols ww -1) ww))))))
                  (let ((here (point)))
                    (forward-line 1)
                    (when (= here (point))
                      (goto-char pos)))))))
          (cons x y))))
    (defun my/posframe--approximate-posn (info)
      (let* ((window (plist-get info :parent-window))
             (pos (plist-get info :position))
             (c my/posframe--posn-cache)
             (xy (and c
                      (eq (nth 0 c) window)
                      (eq (nth 1 c) (window-start window))
                      (nth 3 c))))
        (list window nil
              (or xy (my/posframe--estimate-xy window pos))
              0 nil pos (cons 0 0) nil (cons 0 0) (cons 0 0))))
    (defun my/posframe-poshandler-point-1-safe (orig info &optional font-height upward)
      (if (and (integerp (plist-get info :position))
               (or my/posframe--skip-posn inhibit-redisplay))
          (funcall orig
                   (plist-put (copy-sequence info)
                              :position (my/posframe--approximate-posn info))
                   font-height upward)
        (funcall orig info font-height upward)))
    (advice-add #'posframe-poshandler-point-1
                :around #'my/posframe-poshandler-point-1-safe)
    (defun my/rime-input-method-skip-posn (orig &rest args)
      (let ((my/posframe--skip-posn t))
        (apply orig args)))
    (with-eval-after-load 'rime
      (advice-add #'rime-input-method :around
                  #'my/rime-input-method-skip-posn))

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))


(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-parameters
        '((max-width . 0.6)
          (min-width . 0.6)
          (left-fringe . 8)
          (right-fringe . 8))))

;;; Icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :ensure (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :commands (nerd-icons-completion-marginalia-setup)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :after-call +my/first-input-hook-fun
  :config
  (setq nerd-icons-corfu-mapping
        `((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
          (color :style "cod" :icon "symbol_color" :face success)
          (command :style "cod" :icon "terminal" :face default)
          (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
          (constructor :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
          (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
          (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
          (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
          (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
          (file :style "cod" :icon "symbol_file" :face font-lock-string-face)
          (folder :style "cod" :icon "folder" :face font-lock-doc-face)
          (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
          (keyword :style "cod" :icon "symbol_keyword" :face font-lock-keyword-face)
          (macro :style "cod" :icon "symbol_misc" :face font-lock-keyword-face)
          (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
          (method :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (module :style "cod" :icon "file_submodule" :face font-lock-preprocessor-face)
          (numeric :style "cod" :icon "symbol_numeric" :face font-lock-builtin-face)
          (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
          (param :style "cod" :icon "symbol_parameter" :face default)
          (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
          (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
          (snippet :style "cod" :icon "symbol_snippet" :face font-lock-string-face)
          (string :style "cod" :icon "symbol_string" :face font-lock-string-face)
          (struct :style "cod" :icon "symbol_structure" :face font-lock-variable-name-face)
          (text :style "cod" :icon "text_size" :face font-lock-doc-face)
          (typeparameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
          (type-parameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
          (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
          (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
          (variable :style "cod" :icon "symbol_variable" :face font-lock-variable-name-face)
          (tabnine :style "cod" :icon "hubot" :face font-lock-warning-face)
          (unknown :style "cod" :icon "code" :face font-lock-warning-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; Auto completion
;;;; Corfu
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/corfu-*.el"))
  :hook (+my/first-input . global-corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.18)
  (corfu-max-width 120)
  (corfu-preselect 'prompt)    ;; Disable candidate preselection
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
        ([backtab] . corfu-previous)
        ([remap move-beginning-of-line] . nil)
        ([remap move-end-of-line] . nil))
  :config
  (with-eval-after-load 'corfu-auto
    (add-to-list 'corfu-auto-commands 'end-of-visual-line))
  (advice-add #'keyboard-quit :before #'corfu-quit)

  (use-package corfu-quick
    :ensure nil
    :commands (corfu-quick-insert corfu-quick-complete)
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)
          ("M-q" . corfu-quick-complete)))

  (use-package corfu-history
    :ensure nil
    :hook (corfu-mode . corfu-history-mode))

  (use-package corfu-popupinfo
    :ensure nil
    :hook (corfu-mode . corfu-popupinfo-mode)
    :config
    (set-face-attribute 'corfu-popupinfo nil :height 140)
    (setq corfu-popupinfo-delay '(0.5 . 1.0)))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;;;; Snippets
(use-package tempel
  :after-call +my/first-input-hook-fun
  :after corfu
  :bind ((:map tempel-map
               ("C-i" . tempel-next)))
  :config
  (defun tempel-hippie-try-expand (old)
    "Integrate with hippie expand.
Just put this function in `hippie-expand-try-functions-list'."
    (if (not old)
        (tempel-expand t)
      (undo 1)))
  (add-to-list 'hippie-expand-try-functions-list #'tempel-hippie-try-expand t)
  (defun my/tempel-expand-or-next ()
    "Try tempel expand, if failed, try copilot expand."
    (interactive)
    (if tempel--active
        (tempel-next 1)
      (call-interactively #'tempel-expand))))

(use-package hippie-exp
  :ensure nil
  :bind
  (("M-\\" . hippie-expand))
  :custom (hippie-expand-try-functions-list
           '(try-complete-file-name-partially
             try-complete-file-name
             try-expand-dabbrev
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand-snippet)
  :hook ((yas-keymap-disable . (lambda ()
                                 (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame))))))

;;;; CAPE
(use-package cape
  :after (corfu tempel)
  :commands (my/convert-super-capf my/set-eglot-capf)
  :hook ((text-mode . (lambda ()
                        (my/convert-super-capf #'cape-dabbrev)))
         (yaml-ts-mode . (lambda ()
                           (my/convert-super-capf #'cape-dabbrev)))
         (emacs-lisp-mode . (lambda ()
                              (my/convert-super-capf #'elisp-completion-at-point)))
         (comint-mode . (lambda ()
                          (setq-local completion-at-point-functions '(cape-dabbrev comint-completion-at-point))))
         (org-mode . my/set-basic-capf))
  :config
  (setq dabbrev-upcase-means-case-search nil)
  (setq dabbrev-case-replace nil)
  (setq cape-dict-file "/usr/share/dict/words")
  (setq cape-dabbrev-buffer-function #'cape-text-buffers)

  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)

  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     (cape-capf-super
      #'tempel-complete
      arg-capf)
     ))

  (defun my/set-basic-capf ()
    (setq-local completion-at-point-functions (my/convert-super-capf (car completion-at-point-functions))))

  (defun my/set-eglot-capf ()
    (setq-local completion-at-point-functions (my/convert-super-capf #'eglot-completion-at-point)))

  (setq-default completion-at-point-functions '(cape-file cape-dabbrev)))

;;;; AI assistants
(use-package copilot
  :when +self/enable-copilot
  :after corfu
  :hook ((prog-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :ensure (:host github :repo "copilot-emacs/copilot.el"
                 :files ("dist" "*.el"))
  :bind
  ((:map copilot-completion-map
         ("C-i" . copilot-accept-completion)
         ("M-i" . copilot-accept-completion-by-word)
         ("M-n" . 'copilot-next-completion)
         ("M-p" . 'copilot-previous-completion)))
  :config
  (require 'copilot-balancer)
  (defun +my/corfu-candidates-p ()
    (or (not (eq corfu--candidates nil))
        tempel--active ;; diable copilot in tempel
        (not (looking-back "[\x00-\xff]"))))

  (add-to-list 'copilot-indentation-alist
               '(dart-ts-mode dart-ts-mode-indent-offset))

  (setq warning-minimum-level :error)
  (setq copilot-completion-model "claude-sonnet-4-6")

  (customize-set-variable 'copilot-enable-predicates '(meow-insert-mode-p))
  (customize-set-variable 'copilot-disable-predicates '(+my/corfu-candidates-p evil-ex-p minibufferp))
  (setq copilot-max-char 1000000))

(use-package agent-shell
  :ensure (:files ("*.el"))
  :hook (agent-shell-mode . (lambda ()
                              (add-to-list 'completion-at-point-functions #'cape-dabbrev)))
  :commands (agent-shell agent-shell-toggle +agent-shell-send-file-dwim project-ghostel-agent-shell)
  :init
  (defun project-ghostel-agent-shell ()
    "Open Ghostel on the left and `agent-shell' on the right."
    (interactive)
    (delete-other-windows)
    (let ((right (split-window-right))
          (popper-display-control nil))
      (switch-to-buffer (+ghostel-here))
      (select-window right)
      (agent-shell)
      (when-let ((buf (agent-shell-shell-buffer :no-error t :no-create t)))
        (set-window-buffer right buf))))
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(project-ghostel-agent-shell "Ghostel + Agent" ?a) t))
  :bind
  (:map agent-shell-mode-map
        ("C-c m" . agent-shell-help-menu))
  :custom
  (agent-shell-file-display-action '(display-buffer-pop-up-window))
  (agent-shell-chat-mode-enabled t)
  (agent-shell-file-completion-enabled t)
  (agent-shell-session-strategy 'prompt)
  (agent-shell-session-choices-function
   (lambda (choices)
     (seq-remove
      (lambda (choice)
        (let ((session (cdr choice)))
          (and (listp session)
               (string-prefix-p "subagent-"
                                (or (map-elt session 'title) "")))))
      choices)))
  (agent-shell-session-restore-verbosity 'full)
  (agent-shell-display-action '(display-buffer-reuse-window))
  (agent-shell-preferred-agent-config '(preselect . pi))
  ;; No region -> no context carried into the shell; never send the line at point.
  (agent-shell-context-sources '(files region error))
  :config
  ;; `agent-shell-elicitation--experimental-feature-enabled' is a plain
  ;; `defvar', so the `:custom' entry this used to be cannot set it.  With the
  ;; capability left unadvertised, pi-acp's select elicitation gets refused and
  ;; it silently falls back to a Tool Permission request.
  (setq agent-shell-elicitation--experimental-feature-enabled t)
  (transient-append-suffix
    'agent-shell-help-menu "d"
    '("q" "Queue prompt" agent-shell-prompt-queue :transient t))
  (transient-append-suffix
    'agent-shell-help-menu "q"
    '("c" "Compose prompt" agent-shell-prompt-compose :transient t))

  (defun +agent-shell-send-file-dwim ()
    "Send a file to the current project's agent shell when available."
    (interactive)
    (call-interactively
     (if (agent-shell-project-buffers)
         #'agent-shell-send-file
       #'agent-shell-send-file-to)))

  (defun my/agent-shell-pi-send-ui-response (state request result)
    "Send RESULT for a Pi ACP UI REQUEST."
    (acp-send-response
     :client (map-elt state :client)
     :response `((:request-id . ,(map-elt request 'id))
                 (:result . ,result))))

  (defun my/agent-shell-pi-ui-request-responder (original &rest args)
    "Handle Pi's `_pi/ui/input' request without changing agent-shell."
    (let* ((state (plist-get args :state))
           (request (plist-get args :acp-request))
           (params (and request (map-elt request 'params))))
      (if (and state
               request
               (eq (map-nested-elt state '(:agent-config :identifier)) 'pi)
               (equal (map-elt request 'method) "_pi/ui/input"))
          (let* ((method (or (map-elt params 'method) "input"))
                 (title (or (map-elt params 'message)
                            (map-elt params 'title)
                            "Pi input"))
                 (prefill (let ((value (map-elt params 'prefill)))
                            (if (stringp value) value "")))
                 (result
                  (condition-case err
                      `((value . ,(read-string
                                   (format "%s%s" title
                                           (if (equal method "editor") " (editor): " ": "))
                                   prefill)))
                    (quit '((cancelled . t)))
                    (error
                     (message "Pi UI request failed: %s" err)
                     '((cancelled . t))))))
            (my/agent-shell-pi-send-ui-response state request result))
        (apply original args))))

  (advice-add #'agent-shell--on-request :around
              #'my/agent-shell-pi-ui-request-responder)

  (defun my/consult-agent-shell--buffer (candidate)
    "Return the live agent-shell buffer represented by CANDIDATE."
    (let ((buffer (cond ((bufferp candidate) candidate)
                        ((stringp candidate) (get-buffer candidate)))))
      (and (buffer-live-p buffer) buffer)))

  (defun my/consult-agent-shell--session-name (buffer)
    "Return BUFFER's session title, falling back to its buffer name."
    (with-current-buffer buffer
      (let ((title (map-nested-elt agent-shell--state '(:session :title))))
        (truncate-string-to-width
         (if (and (stringp title) (not (string-empty-p title)))
             (replace-regexp-in-string "[ \t\n\r]+" " " title)
           (buffer-name buffer))
         36 nil nil "…"))))

  (defun my/consult-agent-shell--status (buffer)
    "Return BUFFER's display status and face."
    (with-current-buffer buffer
      (let* ((state agent-shell--state)
             (client (map-elt state :client))
             (live (and (process-live-p (get-buffer-process buffer))
                        (or (not client)
                            (process-live-p (map-elt client :process)))))
             (raw-status (if live
                             (condition-case nil
                                 (agent-shell-status :shell-buffer buffer)
                               (error 'killed))
                           'killed))
             (session-id (map-nested-elt state '(:session :id))))
        (cons
         (pcase raw-status
           ('busy "Working")
           ('blocked "Waiting")
           ('ready (cond ((not (map-elt state :initialized)) "Starting")
                         ((not session-id) "No Session")
                         (t "Ready")))
           ('killed "Killed")
           (_ "Unknown"))
         (pcase raw-status
           ((or 'blocked 'killed) 'agent-shell-error)
           ('busy 'agent-shell-warning)
           ('ready (if (and (map-elt state :initialized) session-id)
                       'agent-shell-success
                     'shadow))
           (_ 'shadow))))))

  (defun my/consult-agent-shell--context-usage (buffer)
    "Return BUFFER's context usage annotation."
    (with-current-buffer buffer
      (let* ((usage (map-elt agent-shell--state :usage))
             (used (map-elt usage :context-used))
             (size (map-elt usage :context-size)))
        (if (and (numberp used) (numberp size) (> size 0))
            (let ((percentage (* 100.0 (/ (float used) size))))
              (propertize
               (format "%s/%s (%.0f%%)"
                       (if (fboundp 'agent-shell--format-number-compact)
                           (agent-shell--format-number-compact used)
                         used)
                       (if (fboundp 'agent-shell--format-number-compact)
                           (agent-shell--format-number-compact size)
                         size)
                       percentage)
               'face (if (fboundp 'agent-shell--context-usage-face)
                         (agent-shell--context-usage-face percentage)
                       'shadow)))
          (propertize "—" 'face 'shadow)))))

  (defun my/consult-agent-shell--cwd (buffer)
    "Return BUFFER's abbreviated working directory."
    (with-current-buffer buffer
      (or (ignore-errors
            (abbreviate-file-name
             (directory-file-name (agent-shell-cwd))))
          (abbreviate-file-name
           (directory-file-name default-directory))
          "—")))

  (defun my/consult-agent-shell-buffer-annotate (candidate)
    "Annotate agent-shell CANDIDATE with session, status, context and CWD."
    (when-let* ((buffer (my/consult-agent-shell--buffer candidate)))
      (let* ((status-data (my/consult-agent-shell--status buffer))
             (status (car status-data))
             (status-face (cdr status-data))
             (session (my/consult-agent-shell--session-name buffer))
             (usage (my/consult-agent-shell--context-usage buffer))
             (cwd (my/consult-agent-shell--cwd buffer)))
        (concat
         (propertize "│ " 'face 'shadow)
         (propertize (format "%-36s" session)
                     'face 'agent-shell-session-title)
         "  "
         (propertize (format "%-9s" status) 'face status-face)
         "  "
         (propertize (format "%-18s" usage) 'face (get-text-property 0 'face usage))
         "  "
         (propertize cwd 'face 'agent-shell-session-directory)))))

  (defvar my/consult-source-agent-shell-buffer
    `(:name "Agent Shell"
            :narrow ?a
            :category agent-shell-session
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :annotate ,#'my/consult-agent-shell-buffer-annotate
            :default t
            :items
            ,(lambda ()
               (consult--buffer-query
                :sort 'visibility
                :as #'buffer-name
                :predicate (lambda (buf)
                             (with-current-buffer buf
                               (derived-mode-p 'agent-shell-mode))))))
    "Consult source for agent-shell buffers.")

  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'my/consult-source-agent-shell-buffer 'append))
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-annotators
                 '(agent-shell-session
                   my/consult-agent-shell-buffer-annotate
                   builtin
                   none)))
  )

(use-package shell-maker
  :ensure (:files (:defaults "*.el")))

(autoload 'transient-define-prefix
  "transient"
  nil nil 'macro)

(use-package codex-ide
  :ensure (:url "https://github.com/dgillis/emacs-codex-ide" :rev :newest)
  :bind ("C-c C-;" . codex-ide-menu))

;;; Utils
;;;; GC and recent files
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :diminish
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 128 1024 1024)))

(use-package recentf
  :ensure nil
  :hook (window-setup . recentf-mode)
  :demand t
  :config
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
  :ensure nil
  :config
  (column-number-mode)
  (setq kill-whole-line t)
  (setq duplicate-line-final-position 1)
  (setq duplicate-region-final-position 1))

;;;; TRAMP
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)
        tramp-copy-size-limit (* 1024 1024))

  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty))

(use-package tramp-sh
  :ensure nil
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

(use-package tramp-adb
  :ensure nil
  :config
  (setq tramp-adb-program "~/Library/Android/sdk/platform-tools/adb"))

;;;; Search helpers
(use-package pinyinlib
  :after-call +my/first-input-hook-fun
  :after orderless
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;;;; Undo and replace
(use-package vundo
  :commands vundo
  :config
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t))

(use-package visual-replace
  :commands (visual-replace my/visual-query-replace +my/replace-dwim)
  :custom-face
  (visual-replace-delete-match ((t (:inherit query-replace :strike-through t))))
  :config
  (custom-set-faces
   '(visual-replace-delete-match ((t (:inherit query-replace :strike-through t)))))
  (setq visual-replace-default-to-full-scope t
        visual-replace-display-total t)
  (defun my/visual-query-replace (args ranges)
    "Like visual-replace but defaults to query mode, like query-replace"
    (interactive (visual-replace-read (visual-replace-make-args
                                       :query t
                                       :word (and current-prefix-arg (not (eq current-prefix-arg '-))))))
    (visual-replace args ranges))

  (defun +my/replace-dwim (&optional word )
    "Make it easy to replace WORD.If WORD is not given, automatically extract it by major-mode."
    (interactive)
    (cond
     ((derived-mode-p 'occur-mode) (occur-edit-mode))
     ((derived-mode-p 'grep-mode) (grep-change-to-grep-edit-mode))
     ((and (featurep 'deadgrep) (derived-mode-p 'deadgrep-mode))
      (wgrep-change-to-wgrep-mode)))
    (let* ((buffer-name (buffer-name))
           (raw-keyword (cond ((string-match "*Embark .* - \\(?1:.*\\)\\*" buffer-name)
                               (match-string 1 buffer-name))
                              ((buffer-match-p "\\*deadgrep.*" buffer-name)
                               (car deadgrep-history))
                              (t (read-string "Keyword: " word))))
           (keyword (replace-regexp-in-string "^#" "" raw-keyword)))
      (visual-replace
       (visual-replace-read (visual-replace-make-args :from keyword :regexp t))
       (list (cons (point-min) (point-max))))
      )))

;;;; Translation
(use-package gt
  :bind
  (("C-c t w" . my-translate-word)
   ("C-c t r" . my-translate-region)
   ("C-c t b" . my-translate-buffer))
  :custom
  (gt-langs '(en zh))
  (gt-buffer-render-follow-p t)
  (gt-buffer-render-window-config
   '((display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (window-height . 0.4)))
  (gt-default-translator
   (gt-translator
    :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
    :engines (list (gt-youdao-dict-engine)
                   (gt-youdao-suggest-engine))
    :render (gt-buffer-render)))
  :custom-face
  (gt-overlay-source-face ((t nil)))
  :config
  (defvar my-gt--active nil)

  (defun my-gt-translate (&optional mode)
    (let ((gt-polyglot-p t))
      (gt-start
       (gt-translator
        :taker (gt-taker :langs '(en zh) :text mode :prompt (eq mode 'word))
        :engines (list (if (eq mode 'word)
                           (gt-youdao-dict-engine)
                         (gt-bing-engine)))
        :render (if (eq mode 'word)
                    (gt-buffer-render)
                  (gt-overlay-render))))))

  (defun my-gt-clear ()
    (dolist (ov (gt-overlay-render-get-overlays (point-min) (point-max)))
      (delete-overlay ov)))

  (defun my-gt-auto-translate (&optional mode)
    (setq my-gt--active (not my-gt--active))
    (if my-gt--active
        (my-gt-translate mode)
      (my-gt-clear)))

  (defun my-translate-word   () (interactive) (my-gt-auto-translate 'word))
  (defun my-translate-region () (interactive) (my-gt-auto-translate 'region))
  (defun my-translate-buffer () (interactive) (my-gt-auto-translate 'buffer)))

;; SaveAllBuffers
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))
;; -SaveAllBuffers

;;;; Project
(use-package project
  :ensure nil
  :commands (project-find-file project-switch-project)
  :config
  (use-package vc-git
    :ensure nil)

  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (ignores  (string-join
                      (mapcar (lambda (ignore)
                                (concat "-E " ignore))
                              project-vc-ignores)
                      " "))
           (command (format "fd -H -t f -0 . %s %s" ignores localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (setq project-vc-ignores '(".dart-tool" ".idea" ".DS_Store" ".git" "build"))

  (cl-defmethod project-files ((project (head transient)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project)))))

  (defcustom project-root-markers
    '(".project"
      "Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" "pubspec.yaml" "deps.edn" "shadow-cljs.edn")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers.
Return nil if .ignoreproject file exists in PATH."
    ;; First check if this directory should be ignored
    (if (file-exists-p (expand-file-name ".ignoreproject" path))
        (progn
          (message "Ignoring project directory: %s (.ignoreproject found)" return)
          (path-from project-root-p nil))
      ;; Then check for project markers
      (catch 'found
        (dolist (marker project-root-markers)
          (when (and
                 (file-exists-p (concat path marker)))
            (throw 'found marker))))
      ))

  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'.
Skip directories containing .ignoreproject file."
    (when-let* ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))

  (defun project-ignore-project-p (dir)
    "Check if DIR should be ignored as a project.
Returns t if .ignoreproject file exists in DIR."
    (file-exists-p (expand-file-name ".ignoreproject" dir)))

  (defun project-ignore-project-here ()
    "Create .ignoreproject file in current directory to exclude it from project.el."
    (interactive)
    (let ((ignore-file (expand-file-name ".ignoreproject" default-directory)))
      (if (file-exists-p ignore-file)
          (message "Directory already has .ignoreproject file")
        (with-temp-file ignore-file
          (insert "# This directory should be ignored by project.el\n")
          (insert "# Remove this file to restore project detection\n"))
        (message "Created .ignoreproject file in %s" default-directory))))

  (defun project-find-root--ignore-project (orig-fun path)
    "Advice function to skip directories containing .ignoreproject file."
    (let ((result (funcall orig-fun path)))
      (when result
        (let ((root-dir (cdr result)))
          (if (project-ignore-project-p root-dir)
              (progn
                (message "Skipping project root %s (contains .ignoreproject)" root-dir)
                nil)
            result)))))

  (advice-add 'project-find-root :around #'project-find-root--ignore-project)

  (require 'project)
  (add-to-list 'project-find-functions #'project-find-root t)
  (setq project-list-exclude '("~/fvm/**" "~/.pub-cache/**" "**/example/**"))
  )

;;;; Window management
(use-package window
  :ensure nil
  :preface
  (let ((windows nil) (index 0))
    (defun my/other-window-mru ()
      "Cycle windows in most-recently-used order."
      (interactive)
      (if (one-window-p t)
          (message "No other window to select")
        (if (eq last-command this-command)
            (setq index (mod (1+ index) (length windows)))
          (select-window (selected-window))
          (setq windows (sort (window-list nil 'nomini)
                              :key #'window-use-time :reverse t)
                index 1))
        (select-window (nth index windows) 'norecord))))
  :bind
  (("C-x o" . my/other-window-mru)
   ("M-o" . my/other-window-mru)
   :map other-window-repeat-map
   ("o" . my/other-window-mru))
  :config
  (put 'my/other-window-mru 'repeat-map 'other-window-repeat-map))

(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (window-setup . winner-mode)
  :config (setq winner-boring-buffers '("*Completions*"
                                        "*Compile-Log*"
                                        "*inferior-lisp*"
                                        "*Fuzzy Completions*"
                                        "*Apropos*"
                                        "*Help*"
                                        "*cvs*"
                                        "*Buffer List*"
                                        "*Ibuffer*"
                                        "*esh command on file*")))

(use-package ediff
  :ensure nil
  :hook (ediff-quit . winner-undo)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

;;;; Tabs and workspaces
(use-package tab-bar
  :ensure nil
  :hook (window-setup . tab-bar-mode)
  :config
  (defun +tab-bar-tab-name-function ()
    "Generate a name for the current tab based on the buffer name.
If the buffer name exceeds `tab-bar-tab-name-truncated-max` characters,
truncate it and append `tab-bar-tab-name-ellipsis`.  If there are multiple
windows in the tab, append the count of windows in parentheses.
Return the formatted tab name."
    (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
           (count (length (window-list-1 nil 'nomini)))
           (truncated-tab-name (if (< (length raw-tab-name)
                                      tab-bar-tab-name-truncated-max)
                                   raw-tab-name
                                 (truncate-string-to-width raw-tab-name
                                                           tab-bar-tab-name-truncated-max
                                                           nil nil tab-bar-tab-name-ellipsis))))
      (if (> count 1)
          (concat truncated-tab-name "(" (number-to-string count) ")")
        truncated-tab-name)))

  (defun +tab-bar-tab-name-format-function (tab i)
    "Format the display name for a tab in the tab bar.
TAB is the tab descriptor, and I is the tab index.  Apply custom
styling to the tab name and index using `tab-bar-tab-face-function`.

- Prefix the tab with its index and a colon, styled with a bold weight.
- Surround the tab name with spaces, adjusting vertical alignment
  for aesthetics.
- Return the formatted tab name with applied text properties."
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (concat
       ;; change tab-bar's height
       (propertize "" 'display '(raise 0.25))
       (propertize (format "%d:" i) 'face `(:inherit ,face :weight ultra-bold))
       (propertize (concat " " (alist-get 'name tab) " ") 'face face)
       (propertize "" 'display '(raise -0.25)))))

  (setopt tab-bar-close-button-show nil
          tab-bar-new-button-show nil
          tab-bar-new-tab-to 'rightmost
          tab-bar-tab-hints t
          tab-bar-show 1
          tab-bar-new-tab-choice "*scratch*"
          tab-bar-select-tab-modifiers '(super)
          tab-bar-tab-name-truncated-max 20
          tab-bar-auto-width nil
          ;; Add spaces for tab-name
          tab-bar-tab-name-function #'+tab-bar-tab-name-function
          tab-bar-tab-name-format-function #'+tab-bar-tab-name-format-function
          tab-bar-format '(tab-bar-format-tabs
                           tab-bar-format-add-tab
                           tab-bar-format-align-right))
  ;; WORKAROUND: update tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))
  )

(use-package tabspaces
  :hook (elpaca-after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  ;; (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-file (concat user-emacs-directory "tabspaces/tabsession.el"))
  (tabspaces-session-project-session-store (concat user-emacs-directory "tabspaces/"))
  (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*" "*ghostel*" "*shell*" "*eshell*" "*elpaca*"))
  (tabspaces-fully-resolve-paths t)  ; Resolve relative project paths to absolute
  (tabspaces-project-switch-opens-workspace t)
  ;; (tabspaces-session-auto-restore t)
  :config
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult-source-buffer :hidden t :default nil)

    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)
    (add-to-list 'consult-project-buffer-sources 'consult--source-workspace)))

;;;; Help
(use-package helpful
  :disabled
  :bind
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)
  ("C-h ." . helpful-at-point))

;;;; Popper
(use-package popper
  :defines popper-echo-dispatch-actions
  :bind (:map popper-mode-map
              ("C-h z" . popper-toggle)
              ("C-<tab>"   . popper-cycle)
              ("C-M-<tab>" . popper-toggle-type))
  :hook (window-setup . popper-mode)
  :config
  (setq popper-mode-line "")
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
          "\\*Calendar\\*"
          "\\*lspce-hover\\*"
          "\\*Embark Actions\\*"
          "\\*Embark Export: .*\\*"
          "\\*Embark Collect: .*\\*"
          "\\*compilation\\*"

          bookmark-bmenu-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode maple-translate-mode

          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode
          "^\\*ghostel\\*$" ghostel-mode
          "^\\*.*eat.*\\*.*$"

          "\\*dape-repl\\*$"
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
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode))

  (with-eval-after-load 'project
    (setq popper-group-function 'popper-group-by-project))
  (setq popper-echo-dispatch-actions t)
  (require 'popper-echo)
  (popper-echo-mode)
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


;;; UI
;;;; Modeline
(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-vcs-display-function #'my/doom-modeline-vcs-name-tail)
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-check-simple-format t)
  (doom-modeline-buffer-modification-icon t)
  :config
  (defvar my/doom-modeline-vcs-tail-length 20)

  (defun my/doom-modeline-vcs-name-tail ()
    (let* ((name (doom-modeline-vcs-name))
           (tail-length (min my/doom-modeline-vcs-tail-length
                             (length name))))
      (if (< tail-length (length name))
          (concat doom-modeline-ellipsis
                  (substring name (- (length name) tail-length)))
        name)))
  )

;;;; Color preview
(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  :hook ((prog-mode . colorful-mode)))

;;;; Smooth scroll
(use-package ultra-scroll
  :functions (hl-todo-mode diff-hl-flydiff-mode)
  :hook (elpaca-after-init . ultra-scroll-mode)
  :config
  (add-hook 'ultra-scroll-hide-functions #'diff-hl-flydiff-mode)
  (add-hook 'ultra-scroll-hide-functions #'hl-todo-mode)
  (add-hook 'ultra-scroll-hide-functions #'jit-lock-mode))

;;;; Themes
(use-package modus-themes
  :hook (window-setup . (lambda ()
                          (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
                          (load-theme 'modus-one-dark t)))
  :custom
  (modus-themes-include-derivatives-mode t))

(use-package ef-themes)

(when (display-graphic-p)
  (defvar +my-cn-font "Sarasa Term SC Nerd"
    "The font name of Chinese characters.")
  (defconst +my-en-font-candidates
    '(("Paper Mono" . 150)
      ("IoskeleyMono Nerd Font" . 140)
      ("Cascadia Code NF" . 130)
      ("Hack Nerd Font Mono" . 130)
      ("MonolisaCode" . 130))
    "Alist of (font-name . height) for `+my/select-en-font'.")
  (defvar +my-en-font (car (car +my-en-font-candidates))
    "The font name of English characters.")
  (defvar +my-en-font-height (cdr (car +my-en-font-candidates))
    "The height of English font.")
  (set-face-attribute 'default nil :font +my-en-font :height +my-en-font-height)

  (defun +my/select-en-font (font)
    "Select English font from `+my-en-font-candidates' and apply it."
    (interactive
     (list (completing-read "English font: "
                            (mapcar #'car +my-en-font-candidates))))
    (let ((cell (assoc font +my-en-font-candidates)))
      (unless cell
        (user-error "Unknown font: %s" font))
      (setq +my-en-font (car cell)
            +my-en-font-height (cdr cell))
      (set-face-attribute 'default nil :font +my-en-font :height +my-en-font-height)
      (message "Font set to %s (height %d)" +my-en-font +my-en-font-height)))

  (set-fontset-font t 'han (font-spec :family +my-cn-font))
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (with-eval-after-load 'org
                (set-face-attribute 'org-table nil :family +my-cn-font))
              (with-eval-after-load 'markdown-mode
                (set-face-attribute 'markdown-table-face nil :family +my-cn-font))))
  )

;;;; Ligatures
(use-package composite
  :ensure nil
  :hook (((prog-mode
           conf-mode nxml-mode markdown-mode help-mode
           shell-mode eshell-mode term-mode vterm-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  (defvar composition-ligature-table (make-char-table nil))
  ;; support ligatures, some toned down to prevent hang
  (let ((alist
         '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (36  . ".\\(?:\\(>\\)>?\\)")
           (37  . ".\\(?:\\(%\\)%?\\)")
           (38  . ".\\(?:\\(&\\)&?\\)")
           (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (43  . ".\\(?:\\([>]\\)>?\\)")
           ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
           (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
           (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (48  . ".\\(?:x[a-zA-Z]\\)")
           (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59  . ".\\(?:\\(;\\);?\\)")
           (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91  . ".\\(?:\\(|\\)[]|]?\\)")
           ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (94  . ".\\(?:\\(=\\)=?\\)")
           (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (119 . ".\\(?:\\(ww\\)w?\\)")
           (123 . ".\\(?:\\(|\\)[|}]?\\)")
           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-ligature-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  (set-char-table-parent composition-ligature-table composition-function-table))

;;; Highlight
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook ((elpaca-after-init . global-hl-line-mode)
         ((term-mode vterm-mode) . hl-line-unload-function)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :hook ((elpaca-after-init . global-hl-todo-mode))
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE" "DONT" "GOTCHA" "DEBUG"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK" "FIXME"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))
  (dolist (keyword '("MARK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'success)) hl-todo-keyword-faces))
  (defun hl-todo-rg (regexp &optional dir)
    "Use `rg' to find all TODO or similar keywords."
    (interactive
     (progn
       (unless (require 'deadgrep nil t)
         (error "`deadgrep' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)))
             (dir (read-directory-name "Base Directory: " (deadgrep--project-root))))
         (list regexp dir))))
    (deadgrep regexp dir)
    (with-current-buffer (car-safe (deadgrep--buffers))
      (setq-local deadgrep--search-type 'regexp)
      (deadgrep-restart))
    ))

(use-package indent-bars
  :ensure (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :custom-face
  (indent-bars-face ((t (:height 1.2))))
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.225))
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-wrap
   '((python
	    argument_list
	    parameters ; for python, as an example
	    list
	    list_comprehension
	    dictionary
	    dictionary_comprehension
	    parenthesized_expression
	    subscript)))
  (indent-bars-no-stipple-char ?\⎸)
  :config
  (require 'indent-bars-ts))

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
  :ensure nil
  :commands (my-recenter-and-pulse my-recenter-and-pulse-line)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((better-jumper-post-jump consult-after-jump xref-after-jump) . #'my-recenter-and-pulse)
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

(use-package symbol-overlay
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit (region bold)))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :custom
  (symbol-overlay-idle-time 0.1)
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))
;;; Syntax checker
(use-package flymake
  :ensure nil
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  (add-hook 'flymake-project-diagnostics-mode-hook #'my/flymake-sort-by-type-desc)
  (add-hook 'flymake-diagnostics-buffer-mode-hook #'my/flymake-sort-by-type-desc)
  (defun my/flymake-sort-by-type-desc ()
    (setq-local tabulated-list-sort-key (cons "Type" t)))
  ;; (defun sanityinc/eldoc-flymake-first ()
  ;;   "Gives flymake's eldoc function priority in the minibuffer."
  ;;   (when flymake-mode
  ;;     (setq-local eldoc-documentation-functions
  ;;                 (cons 'flymake-eldoc-function
  ;;                       (delq 'flymake-eldoc-function eldoc-documentation-functions)))))
  ;; (add-hook 'flymake-mode-hook 'sanityinc/eldoc-flymake-first)

  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path
                load-path))

  (setq flymake-no-changes-timeout 0.5
        flymake-show-diagnostics-at-end-of-line nil
        flymake-margin-indicator-position 'right-margin
        flymake-fringe-indicator-position 'right-fringe)
  )

(use-package flymake-popon
  :diminish
  :custom-face
  (flymake-popon-posframe-border ((t :foreground ,(face-background 'region))))
  :hook (flymake-mode . flymake-popon-mode)
  :custom
  (flymake-popon-width 80)
  (flymake-popon-posframe-border-width 1)
  (flymake-popon-method 'posframe))

(use-package jinx
  :bind (([remap ispell-word] . #'jinx-correct))
  :hook ((text-mode) . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

;;; Better edit
;;;; Formatters
(use-package apheleia
  :commands (apheleia-format-buffer)
  :bind ("C-c c f" . apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent)))

;;;; Text objects and cleanup
(use-package delete-block
  :ensure (:repo "manateelazycat/delete-block" :host github)
  :commands (delete-block-backward)
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))

(use-package ws-butler
  :diminish
  :hook (window-setup . ws-butler-global-mode))

;;;; Grep and replace
(use-package grep
  :ensure nil
  :custom
  (grep-using-headings t)
  :bind (:map grep-mode-map
              ("i" . #'grep-change-to-grep-edit-mode))
  :config
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

(use-package replace
  :ensure nil
  :bind (:map occur-mode-map
              ("i" . #'occur-edit-mode)))

;; deadgrep still needs wgrep; builtin grep-edit-mode only covers grep-mode.
(use-package wgrep
  :ensure (:files (:defaults "*.el"))
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package deadgrep
  :bind
  ("<f5>" . #'deadgrep)
  (:map deadgrep-mode-map
        ("i" . #'wgrep-change-to-wgrep-mode))
  :config
  (require 'wgrep-deadgrep))

;;;; Expand region and jump
(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))
(use-package home-row-expreg
  :ensure (:host github
                 :repo "bommbo/home-row-expreg"
                 :files ("*.el"))
  :after expreg
  :demand t
  :config
  (home-row-expreg-mode))

(use-package avy)

(use-package flash-emacs
  :ensure (:host github :repo "JiaweiChenC/flash-emacs")
  :config
  (defun flash-emacs--set-jump-before-jump (&rest _args)
    "Set a jump point before running `flash-emacs-jump`."
    (better-jumper-set-jump))

  (advice-add 'flash-emacs-jump :before #'flash-emacs--set-jump-before-jump))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;;; Surround and edit source blocks
(use-package embrace
  :commands (embrace-commander)
  :bind ("C-;" . embrace-commander))

(use-package separedit
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-default-mode 'markdown-mode)
  :config
  (add-to-list 'separedit-comment-delimiter-alist '(("///" "//") . (dart-mode dart-ts-mode))))

;;;; Input method
(use-package rime
  :after-call +my/first-input-hook-fun
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(
     ;; rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-after-ascii-char-p
     rime-predicate-space-after-cc-p
     meow-normal-mode-p))
  (rime-posframe-properties
   (list :internal-border-width 5))
  :bind
  (:map rime-mode-map
        ("C-~" . 'rime-send-keybinding))
  :config
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)
  (cond (*sys/mac* (setq rime-user-data-dir "~/.config/rime"
                         rime-librime-root "/opt/homebrew"))
        (*sys/linux* (setq rime-user-data-dir "~/.rime")))
  (when *sys/mac*
    (unless rime-emacs-module-header-root
      (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/")))
  (defun activate-default-input-method ()
    (interactive)
    (activate-input-method default-input-method))
  ;; (add-hook 'text-mode-hook 'activate-default-input-method)

  (defvar im-cursor-color "Orange"
    "The color for input method.")

  (defvar im-default-cursor-color (frame-parameter nil 'cursor-color)
    "The default cursor color.")

  (defun im--chinese-p ()
    "Check if the current input state is Chinese."
    (if (featurep 'rime)
        (and (rime--should-enable-p)
             (not (rime--ascii-mode-p))
             current-input-method)
      current-input-method))

  (defun im-change-cursor-color ()
    "Set cursor color depending on input method."
    (interactive)
    (set-cursor-color (if (im--chinese-p)
                          im-cursor-color
                        im-default-cursor-color)))

  (define-minor-mode cursor-chg-mode
    "Toggle changing cursor color.
With numeric ARG, turn cursor changing on if ARG is positive.
When this mode is on, `im-change-cursor-color' control cursor changing."
    :init-value nil :global t :group 'frames
    (if cursor-chg-mode
        (add-hook 'post-command-hook 'im-change-cursor-color)
      (remove-hook 'post-command-hook 'im-change-cursor-color)))

  (add-hook 'rime-mode-hook 'cursor-chg-mode)

  (with-eval-after-load 'modus-themes
    (modus-themes-with-colors
      (setq im-cursor-color (face-foreground 'warning))))
  )

(use-package super-save
  :hook (window-setup . super-save-mode)
  :config
  (add-to-list 'super-save-triggers 'eglot-rename)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 0.6)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)
  (add-to-list 'super-save-predicates (lambda () (not (and (featurep 'tempel) tempel--active))))
  (add-to-list 'super-save-predicates (lambda () (not (and (boundp 'corfu--frame) (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))))
  (add-to-list 'super-save-predicates (lambda () (not (and (boundp 'rime--preedit-overlay) rime--preedit-overlay))))
  (if (featurep 'copilot-mode)
      (add-to-list 'super-save-predicates (lambda () (not (and copilot-mode (copilot--overlay-visible))))))

  (defun +super-save-without-format ()
    (when (super-save-p)
      (save-all-buffers)))
  (advice-add 'super-save-command :override '+super-save-without-format))

;;; Programing
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . hs-minor-mode)
         (prog-mode . (lambda () (setq-local tab-width 2))))
  :config
  (global-prettify-symbols-mode))

(use-package vs-comment-return
  :ensure (:host github :repo "emacs-vs/vs-comment-return")
  :hook (prog-mode . vs-comment-return-mode)
  :custom
  (vs-comment-return-cancel-after t))

(use-package better-jumper
  :after-call +my/first-input-hook-fun
  :commands better-jump-set-jump-a better-jumper-mode
  :bind
  (([remap xref-pop-marker-stack] . better-jumper-jump-backward)
   ([remap xref-go-back] . better-jumper-jump-backward)
   ([remap xref-go-forward] . better-jumper-jump-forward))
  :config
  (defun better-jump-set-jump-a (fn &rest args)
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((better-jumper--jumping t))
      (apply fn args)))

  (mapcar
   (lambda (fn)
     (advice-add fn :around #'better-jump-set-jump-a))
   (list #'kill-current-buffer #'consult-imenu #'consult-line
         #'find-file #'consult-fd #'consult-ripgrep #'xref-pop-to-location)))

(use-package dumb-jump
  :after-call +my/first-input-hook-fun
  :commands (dumb-jump-result-follow dumb-jump-go dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'xref-after-jump-hook #'better-jumper-set-jump)
  (add-hook 'xref-after-return-hook #'better-jumper-set-jump))

(use-package lisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

;;;; LSP integration
(use-package eglot
  :ensure nil
  :commands (+eglot-organize-imports)
  :hook ((eglot-managed-mode . (lambda ()
                                 (setq eldoc-documentation-functions
                                       (cons #'flymake-eldoc-function
                                             (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                                 ;; ;; Show all eldoc feedback.
                                 (my/set-eglot-capf)
                                 (eglot-semantic-tokens-mode -1)
                                 (setq eldoc-documentation-strategy #'eldoc-documentation-enthusiast)))
         (prog-mode . (lambda ()
                        (unless (or (derived-mode-p 'emacs-lisp-mode 'makefile-mode))
                          (eglot-ensure)))))
  :config
  (setq read-process-output-max #x100000)  ; 1MB
  (setq
   eglot-send-changes-idle-time 0.4
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-confirm-server-initiated-edits nil
   eglot-sync-connect nil
   eglot-events-buffer-config '(:size 0 :format full)
   eglot-report-progress nil
   eglot-code-action-indications '(mode-line)
   eglot-advertise-cancellation t
   eglot-watch-files-outside-project-root nil
   )
  (setq eldoc-echo-area-use-multiline-p 5)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
  (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))

  (setq-default eglot-workspace-configuration '((:dart . (:completeFunctionCalls t :enableSnippets t))))

  ;; WORKAROUND https://github.com/joaotavora/eglot/issues/1296
  ;; (cl-defmethod eglot-handle-notification :after
  ;;   (_server (_method (eql textDocument/publishDiagnostics)) &key uri
  ;;            &allow-other-keys)
  ;;   (when-let* ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
  ;;     (with-current-buffer buffer
  ;;       (if (and (eq nil flymake-no-changes-timeout)
  ;;                (not (buffer-modified-p)))
  ;;           (run-with-timer 2 nil #'flymake-start t)))))
  )

(use-package eglot-inactive-regions
  :ensure (:url "https://github.com/fargiolas/eglot-inactive-regions")
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.3)
  :hook
  (eglot-connect . eglot-inactive-regions-mode))

(use-package eldoc-mouse
  :diminish
  :bind (:map emacs-lisp-mode-map
              ("C-h ." . eldoc-mouse-pop-doc-at-cursor)
              :map eglot-mode-map
              ("C-h ." . eldoc-mouse-pop-doc-at-cursor))
  :custom
  (eldoc-mouse-posframe-border-color (face-background 'posframe-border nil t))
  :config (add-to-list 'eldoc-mouse-posframe-override-parameters
                       `(background-color . ,(face-background 'tooltip nil t))))

(use-package eglot-booster
  :ensure (:repo "jdtsmith/eglot-booster" :host github)
  :when (executable-find "emacs-lsp-booster")
  :commands (eglot-booster-mode)
  :after eglot)

;;;; Debug
(use-package dape
  :ensure (:host github :repo "svaante/dape")
  :hook (dape-active-mode . dape-breakpoint-global-mode)
  :commands (+dape-transient)
  :bind
  (("<f6>" . +dape-transient))
  :init
  (defvar dape-flutter-devices '(("chrome" . "chrome")
                                 ("macos" . "macos")))
  :config
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (add-hook 'dape-on-start-hooks 'my/dape--create-log-buffer)
  (add-hook 'dape-stopped-hook (lambda () (kill-buffer my/dape--log-buffer)))
  (transient-define-prefix +dape-transient ()
    "Dape debugger controls."
    [["Stepping"
      ("d" "init" dape :transient t)
      ("n" "Next" dape-next :transient t)
      ("i" "Step in" dape-step-in :transient t)
      ("o" "Step out" dape-step-out :transient t)
      ("c" "Continue" dape-continue :transient t)
      ("r" "Restart" dape-restart :transient t)]
     ["Breakpoints"
      ("bb" "Toggle" dape-breakpoint-toggle :transient t)
      ("ba" "Toggle" dape-breakpoint-toggle :transient t)
      ("be" "Expression" dape-breakpoint-expression :transient t)
      ("bd" "Delete" dape-breakpoint-remove-at-point :transient t)
      ("bD" "Delete all" dape-breakpoint-remove-all :transient t)
      ("bl" "Log message" dape-breakpoint-log :transient t)]
     ["Info"
      ("si" "Info" dape-info :transient t)
      ("sm" "Memory" dape-read-memory :transient t)
      ("ss" "Select stack" dape-select-stack :transient t)
      ("R" "Repl" dape-repl :transient t)]]
    ["Quit"
     ("q" "quit" transient-quit-one)
     ("Q" "Disconnect" dape-kill)])

  (setq dape-debug t)
  (setq dape-request-timeout 20)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)

  (defun dape--flutter-cwd ()
    (let ((root (dape--default-cwd)))
      (cond
       ((string-match-p "ftf_melos_workspace" root)
        (concat root "../../app/ft_nn_app/ft_nn_module"))
       ((file-exists-p (concat root "lib/main.dart"))
        root)
       ((file-exists-p (concat root "example/lib/main.dart"))
        (concat root "example/")))))

  (defun dape--flutter-entrypoint ()
    (concat (dape--flutter-cwd) "/lib/main.dart"))

  (require 'flutter)

  (defun dape--flutter-devices ()
    (let* ((collection dape-flutter-devices)
           (choice (completing-read "Device: " collection)))
      (cdr (assoc choice collection))))

  (defun dape--flutter-cwd-fn ()
    (interactive)
    (let* ((root (dape--default-cwd)))
      (cond
       ((file-exists-p (concat root "lib/main.dart"))
        root)
       ((file-exists-p (concat root "example/lib/main.dart"))
        (concat root "example/")))))

  (add-to-list 'dape-configs
               `(flutter-run
                 modes (dart-ts-mode)
                 command "flutter"
                 command-args ("debug_adapter")
                 command-cwd dape--flutter-cwd-fn
                 :type "dart"
                 :request "launch"
                 :cwd dape--flutter-cwd-fn
                 :toolArgs ,(lambda () (vector "-d" (dape--flutter-devices)))
                 ))

  (add-to-list 'dape-configs
               `(flutter-attach
                 modes (dart-ts-mode)
                 command "flutter"
                 command-args ("debug_adapter")
                 command-cwd dape--flutter-cwd-fn
                 :type "dart"
                 :request "attach"
                 :cwd dape--flutter-cwd-fn
                 :toolArgs ,(lambda () (vector "-d" (dape--flutter-devices)))))


  (defun dape-flutter-hotRestart ()
    (interactive)
    (dape-request (dape--live-connection 'last) "hotRestart" nil))

  (defun dape-flutter-hotReload ()
    (interactive)
    (dape-request (dape--live-connection 'last) "hotReload" nil))

  (defun dape-flutter-devtools ()
    (interactive)
    (with-current-buffer "*dape-connection events*"
      (save-match-data
        (point-min)
        (let* (
               (devtools-addr (progn
                                (string-match "activeDevtoolsServerAddress.*\".*\"\\(https?://.*?\\)\"" (buffer-string))
                                (match-string 1 (buffer-string))))
               (vm-service-uri (progn
                                 (string-match "connectedVmServiceUri.*\\(https?://.*?\\)\"" (buffer-string))
                                 (match-string 1 (buffer-string)))))
          (browse-url-chrome (format "%s?uri=%s" devtools-addr vm-service-uri))
          ))))

  (with-eval-after-load 'bind
    (bind
     prog-mode-map
     (bind-prefix "C-x ,"
       "D" #'dape-breakpoint-global-mode-map)
     ))

  (defvar my/dape--log-buffer "*my-dape-log*")

  (defun my/open-dape-log-buffer ()
    (interactive)
    (if (get-buffer my/dape--log-buffer)
        (pop-to-buffer my/dape--log-buffer)
      (message "No dape log buffer")))

  (defun my/dape--create-log-buffer ()
    (my/dape--delete-log-buffer)
    (get-buffer-create my/dape--log-buffer))

  (defun my/dape--delete-log-buffer()
    (when-let* ((buffer (get-buffer my/dape--log-buffer)))
      (kill-buffer buffer)))

  (defun my/dape--log-write (msg)
    (when (buffer-live-p (get-buffer my/dape--log-buffer))
      (with-current-buffer my/dape--log-buffer
        (goto-char (point-max))
        (insert msg)
        )))
  )

(use-package dape-toolbar
  :ensure (:url "https://github.com/zsxh/dape-toolbar"
                :rev :newest)
  :after dape
  :config
  (dape-toolbar-mode))

;;;; Code navigation chrome
(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb")
  :hook (elpaca-after-init . breadcrumb-mode))

(use-package consult-eglot
  :after (consult eglot)
  :commands (consult-eglot-symbols))

(use-package consult-eglot-embark
  :after (embark consult-eglot)
  :config
  (consult-eglot-embark-mode))

;;;; Tree-sitter
(use-package treesit
  :ensure nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :custom
  (treesit-enable-modes t)
  :config
  (add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (dart . ("https://github.com/UserNobody14/tree-sitter-dart"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (protobuf . ("https://github.com/mitchellh/tree-sitter-proto"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
          (swift . ("https://codeberg.org/woolsweater/tree-sitter-swifter"))
          (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))))
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
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

(use-package hideshow
  :ensure nil
  :hook ((prog-mode . hs-minor-mode)))

(use-package kirigami
  :ensure t)

;;;; Imenu and symbols
(use-package imenu
  :ensure nil
  :commands (imenu)
  :hook (imenu-after-jump . recenter))

(use-package symbols-outline
  :commands (symbols-outline-show)
  :ensure (:files (:defaults "*.el" "icons"))
  :diminish
  :hook (symbols-outline-mode . (lambda ()
                                  (setq-local mode-line-format nil)))
  :config
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook ; Or `eglot-managed-mode-hook'
              (lambda ()
                (setq-local symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))))
  (setq symbols-outline-window-position 'left)
  (setq symbols-outline-use-nerd-icon-in-gui t)
  (setq symbols-outline-window-width 40)
  (symbols-outline-follow-mode))

;;;; Xref
(use-package xref
  :ensure nil
  :after-call +my/first-input-hook-fun
  :config
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  (setq xref-history-storage 'xref-window-local-history)
  :hook ((xref-after-return xref-after-jump) . recenter))

(use-package xref-project-history
  :commands (xref-project-history)
  :ensure (:url "https://codeberg.org/imarko/xref-project-history")
  :custom
  (xref-history-storage #'xref-project-history))

;;;; Markdown and prose
(use-package textui
  :ensure (:type git :host github :repo "yibie/textui"))

(use-package md-mode
  :ensure (:type git :host github :repo "yibie/md-mode")
  :mode ("\\.md\\'" . md-mode))


(use-package markdown-xwidget
  :after markdown-mode
  :ensure (markdown-xwidget
           :host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources"))
  :config
  (setq markdown-xwidget-github-theme "light"
        markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "default"
        markdown-xwidget-command "multimarkdown"))

(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :custom
  (olivetti-style 'fancy)
  (olivetti-margin-width 5))

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  :init-value nil :lighter " Prose" :keymap nil
  (if prose-mode
      (progn
        (when (fboundp 'olivetti-mode)
          (olivetti-mode 1))
        (setq truncate-lines nil)
        (display-line-numbers-mode -1)
        (setq word-wrap t)
        (setq word-wrap-by-category t)
        (setq cursor-type 'bar)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local electric-pair-mode nil)
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'word-wrap-by-category)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'electric-pair-mode)
    (visual-line-mode -1)
    (display-line-numbers-mode 1)
    (when (fboundp 'olivetti-mode)
      (olivetti-mode -1))))

(use-package diagram-preview
  :ensure (:host github :repo "natrys/diagram-preview"))

;;;; Language modes
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . (lambda ()
                         (process-query-on-exit-flag
                          (get-process "Python"))))
  :config
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (setq py-tab-indent nil)
  (add-hook 'python-mode-hook (lambda ()
                                (setq-local tab-width 4))))
(use-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.vue\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.wxml\\'")
  :custom
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-block-padding 0)
  (web-mode-part-padding 0)
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
                     (my/web-vue-setup))))))

(use-package css-mode
  :ensure nil
  :hook (css-mode . rainbow-mode)
  :mode ("\\.css\\'" "\\.wxss\\'"))

;; EmmetPac
(use-package emmet-mode
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  ;; :bind (:map web-mode-map
  ;;             ("C-j" . emmet-expand-yas))
  :config
  (add-hook 'emmet-mode-hook (lambda()
                               (setq emmet-indent-after-insert t))))
;; -EmmetPac

;;;; Dart / Flutter / Swift
(use-package dart-ts-mode
  :mode ("\\.dart\\'" . dart-ts-mode)
  :ensure (:repo "50ways2sayhard/dart-ts-mode" :host github)
  :init
  (defvar dart-lsp-command '("dart" "language-server" "--client-id" "emacs-eglot-dart" "--protocol" "lsp"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 (cons 'dart-ts-mode (if (executable-find "fvm")
                                         (add-to-list 'dart-lsp-command "fvm")
                                       dart-lsp-command))))
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("dart" . dart-ts)))

  (with-eval-after-load 'markdown
    (add-to-list 'markdown-code-lang-modes '("dart" . dart-ts)))

  :config
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(dart-ts-mode :types
                                                      ((?c "Class"    font-lock-type-face)
                                                       (?e "Enum" font-lock-type-face)
                                                       (?E "EnumMember" font-lock-variable-name-face)
                                                       (?C "Constructor" font-lock-type-face)
                                                       (?S "Constant"    font-lock-constant-face)
                                                       (?f "Function"  font-lock-function-name-face)
                                                       (?m "Method"  font-lock-function-name-face)
                                                       (?p "Property" font-lock-variable-name-face)
                                                       (?F "Field"  font-lock-variable-name-face))))))

(use-package swift-ts-mode
  :ensure (:url "https://codeberg.org/woolsweater/swift-ts-mode")
  :mode ("\\.swift\\'" "\\.swiftinterface\\'")
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(swift-ts-mode . ("sourcekit-lsp"))))
  :custom
  (swift-ts-basic-offset 4)
  (swift-ts:indent-trailing-call-member t))

(use-package flutter
  :commands (dart--get-dart-vm-service-port)
  :ensure (:repo "50ways2sayhard/flutter.el" :host github)
  :after dart-ts-mode
  :init
  (defvar flutter--modeline-device nil)
  (with-eval-after-load 'bind
    (bind
     dart-ts-mode-map
     (bind-prefix "C-x ,"
       "r" (lambda () (interactive)
             (if (and (featurep 'dape) (dape--live-connection 'last :nowarn))
                 (dape-flutter-hotReload)
               (flutter-run-or-hot-reload)))
       "R" (lambda () (interactive)
             (if (and (featurep 'dape) (dape--live-connection 'last :nowarn))
                 (dape-flutter-hotRestart)
               (flutter-run-or-hot-restart)))
       "v" (lambda () (interactive)
             (if (and (featurep 'dape) (dape--live-connection 'last :nowarn))
                 (dape-flutter-devtools)
               (flutter-open-devtools)))
       "Q" #'flutter-quit
       "p" #'flutter-pub-get
       "tt" #'flutter-test-at-point
       "tf" #'flutter-test-current-file
       "tF" #'flutter-test-all
       "o" #'my/flutter-doc-search)))
  :config
  (defun flutter--modeline-device-update ()
    (let* ((devices (flutter--devices))
           (choice (completing-read "Device: " devices))
           (device (assoc choice devices)))
      (when (not (equal device flutter--modeline-device))
        (setq flutter--modeline-device device))))

  (defun flutter--modeline-format ()
    (when flutter--modeline-device
      (propertize (format " [%s] " (car flutter--modeline-device)) 'face 'font-lock-constant-face)))

  (defun flutter-modeline-device-update ()
    (interactive)
    (flutter--modeline-device-update)
    (make-local-variable 'mode-line-misc-info)
    (add-to-list 'mode-line-misc-info (flutter--modeline-format) t))

  (defun my/flutter-doc-search (query)
    (interactive "ssearch: ")
    (browse-url
     (concat "https://docs.flutter.dev/search?q=" (string-replace " " "%20" query)) t))

  (defun dart--get-dart-vm-service-port ()
    "获取 Dart VM Service 信息。
使用 dns-sd 命令查找 _dartVmService._tcp 服务,
如果有多个服务则使用 completing-read 让用户选择。
返回 (port . authCode) 的 cons,如果未找到则返回 nil。"
    (let* ((timeout 1)
           (cmd (format "timeout %d dns-sd -Z _dartVmService._tcp 2>&1 || true"
                        timeout))
           (output (shell-command-to-string cmd))
           (lines (split-string output "\n" t))
           (vm-services nil)
           (current-service nil))
      ;; 解析 SRV 和 TXT 记录
      (dolist (line lines)
        (cond
         ;; 解析 SRV 记录: <name> SRV 0 0 <port> <host>
         ((string-match
           "^\\([^ ]+\\)\\._dartVmService\\._tcp\\s-+SRV\\s-+[0-9]+\\s-+[0-9]+\\s-+\\([0-9]+\\)"
           line)
          (let* ((raw-name (match-string 1 line))
                 (port (match-string 2 line))
                 ;; 去掉转义符,将 \. 替换为 .
                 (name (replace-regexp-in-string "\\\\\\." "." raw-name))
                 ;; 去掉转义的空格 \032
                 (name (replace-regexp-in-string "\\\\032" " " name)))
            ;; 过滤掉重复的 vm 服务(名称包含空格加括号,如 " (2)")
            (unless (string-match-p "\\s-+(.*)" name)
              (setq current-service (list :name name :port port :auth-code nil)))))
         ;; 解析 TXT 记录: <name> TXT "authCode=xxx"
         ((and current-service
               (string-match "TXT\\s-+\"authCode=\\([^\"]+\\)\"" line))
          (let ((auth-code (match-string 1 line)))
            (plist-put current-service :auth-code auth-code)
            (push current-service vm-services)
            (setq current-service nil)))))
      ;; 根据找到的服务数量决定如何返回
      (cond
       ((null vm-services)
        (message "找不到 VM-Service")
        nil)
       ((= (length vm-services) 1)
        (let ((service (car vm-services)))
          (cons (plist-get service :port)
                (plist-get service :auth-code))))
       (t
        ;; 多个服务时使用 completing-read 让用户选择
        (let* ((candidates (mapcar (lambda (service)
                                     (let ((name (plist-get service :name))
                                           (port (plist-get service :port)))
                                       (cons (format "[%s] %s" port name) service)))
                                   (reverse vm-services)))
               (choice (completing-read "选择 VM Service: " candidates nil t))
               (selected (cdr (assoc choice candidates))))
          (cons (plist-get selected :port)
                (plist-get selected :auth-code)))))))
  )

;;;; Terminals
(use-package ghostel
  :commands (+ghostel-here)
  :hook (eshell-load . ghostel-eshell-visual-command-mode)
  :hook (ghostel-line-mode . (lambda () (add-to-list 'ghostel-line-mode-completion-at-point-functions #'cape-dabbrev)))
  :bind
  (("C-0" . #'+ghostel-here)
   :map ghostel-mode-map
   ("C-g" . nil)
   :map ghostel-semi-char-mode-map
   ("C-s"  . consult-line)
   ("C-k"  . my/ghostel-send-C-k-and-kill)
   ;; I'm used to go up/down the shell history with M-n/p from eshell
   ;; Simulate this behavior in ghostel by sending C-p and C-n
   ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
   ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl"))))
  :custom
  (ghostel-term "xterm-256color")
  (ghostel-enable-title-tracking nil)
  (ghostel-ignore-cursor-change t)
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel" ?t) t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers" ?T) t)

  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  (when (locate-library "spinner")
    (setq ghostel-progress-function #'ghostel-spinner-progress
          ghostel-spinner-type 'progress-bar))

  (defun +ghostel-here (&optional arg)
    "Open Ghostel in project root if in a project, else current directory.
Same directory reuses the buffer; different directories do not."
    (interactive "P")
    (if (project-current)
        (ghostel-project arg)
      (let ((ghostel-buffer-name (ghostel-buffer-name-by-directory nil)))
        (ghostel arg))))

  (defvar +consult-source-ghostel
    `(:name "Ghostel"
            :narrow ?t
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :items
            ,(lambda ()
               (consult--buffer-query
                :sort 'visibility
                :as #'buffer-name
                :predicate (lambda (buf)
                             (with-current-buffer buf
                               (derived-mode-p 'ghostel-mode))))))
    "Consult source for ghostel-mode buffers.")

  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources '+consult-source-ghostel 'append)
    (add-to-list 'consult-project-buffer-sources '+consult-source-ghostel 'append)))

(use-package popterm
  :commands (+popterm-toggle-dwim +popterm-register-shortcut)
  :bind (("C-9" . +popterm-toggle-dwim)
         ("C-c t r" . +popterm-register-shortcut)
         ([f9]    . popterm-window-toggle)
         :map popterm-term-map
         ("C-g" . popterm-return))
  :custom
  (popterm-backend        'ghostel)     ; or 'ghostel, 'eat, 'shell, 'eshell
  (popterm-display-method 'posframe)  ; or 'window, 'fullscreen
  (popterm-scope          'project)   ; or 'frame, 'dedicated, nil
  (popterm-auto-cd        t)
  (popterm-posframe-width-ratio 0.95)
  (popterm-posframe-height-ratio 0.90)
  :config
  (require '+popterm)
  (popterm-global-mode 1)
  (+popterm-auto-hide-mode 1))

;;; Org Mode
;;;; Capture files
(defvar +org-capture-file-gtd (concat +self/org-base-dir "gtd.org"))
(defvar +org-capture-file-note (concat +self/org-base-dir "notes.org"))
(defvar +org-capture-file-someday (concat +self/org-base-dir "someday.org"))
(defvar +org-capture-file-tickler (concat +self/org-base-dir "tickler.org"))
(defvar +org-capture-file-done (concat +self/org-base-dir "done.org"))
(defvar +org-capture-file-routine (concat +self/org-base-dir "routine.org"))

(defvar +org-files
  (mapcar (lambda (p) (expand-file-name p)) (list +org-capture-file-gtd
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
  "Init `org-agenda'."
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
   org-agenda-span 7
   org-agenda-start-on-weekday 1
   org-agenda-start-day "-3d"))

(defun +org-update-cookies-h ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      (org-update-parent-todo-statistics))))

(defun +my/open-org-agenda ()
  "open org agenda in left window"
  (interactive)
  (org-agenda nil "n"))

;;;; Org core
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands (+my/open-org-agenda +org/archive-done-tasks)
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-update-cookies-h)
         (org-mode . (lambda ()
                       (visual-line-mode)
                       (show-paren-local-mode -1)
                       (eldoc-mode -1))))
  :bind
  (:map org-mode-map
        ([tab] . org-cycle))
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-ellipsis " ▾ ")
  (org-bullets-bullet-list '("#"))
  (org-tags-column -77)
  (org-src-preserve-indentation nil)
  (org-hide-block-startup t)
  (org-cycle-hide-block-startup t)
  (org-pretty-entities t)
  (org-edit-src-content-indentation 0)
  (org-capture-bookmark nil)
  (org-log-done 'time)
  (org-hide-emphasis-markers t)
  (org-deadline-warning-days 90)
  (org-agenda-span 7)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-start-on-weekday 1)
  (org-directory (expand-file-name +self/org-base-dir))
  (org-babel-python-command "python3")
  :custom-face
  (org-level-1 ((t (:height 1.10))))
  (org-level-2 ((t (:height 1.08))))
  (org-level-3 ((t (:height 1.06))))
  (org-level-4 ((t (:height 1.04))))

  :config
  (require '+org-helper)
  (setq org-modules '(org-habit))
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

  (setq org-log-into-drawer "LOGBOOK")
  (setq org-agenda-files (list +org-capture-file-gtd
                               +org-capture-file-tickler))
  (setq org-refile-targets '((+org-capture-file-gtd :level . 1)
                             (+org-capture-file-someday :level . 1)
                             (+org-capture-file-tickler :level . 1)))
  (setq org-log-into-drawer t)
  (setq org-tag-alist '(("company" . ?c) ("code" . ?a) ("life" . ?l) ("document" . ?p) ("emacs" . ?e) ("bug" . ?b)))
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

  (add-hook 'org-mode-hook #'(lambda ()
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
  ;; TODO: bind more keys
  (define-key org-mode-map (kbd "C-<return>") '+org/insert-item-below)
  (define-key org-mode-map (kbd "C-S-<return>") 'org-insert-subheading)

  (defvar-keymap +org-insert-map
    "l" #'org-insert-link)

  (defvar-keymap +org-schedule-map
    "d" #'org-deadline
    "s" #'org-schedule)

  (bind
   org-mode-map
   (bind-prefix "C-x ,"
     "t" #'org-todo
     "q" #'org-set-tag-commend
     "T" #'org-todo-list
     "x" #'org-toggle-checkbox
     "D" #'+my-org/mark-done
     "h" #'org-toggle-heading
     "I" #'org-toggle-inline-images
     "d" +org-schedule-map
     "i" +org-insert-map))

  (add-hook 'org-mode-hook  (lambda ()
                              (setq prettify-symbols-alist
                                    '(("lambda" . ?λ)
                                      (":PROPERTIES:" . ?)
                                      (":ID:" . ?)
                                      (":END:" . ?)
                                      ("#+TITLE:" . ?)
                                      ("#+AUTHOR:" . ?)
                                      ("#+BEGIN_QUOTE" . ?)
                                      ("#+END_QUOTE" . ?)
                                      ("#+RESULTS:" . ?)
                                      ("[ ]" . ?)
                                      ("[-]" . ?)
                                      ("[X]" . ?)
                                      ("[#A]" . ?🅐)
                                      ("[#B]" . ?🅑)
                                      ("[#C]" . ?🅒)))
                              (prettify-symbols-mode)))
  )
;; -OrgPac

;;;; Org media and UI
(use-package org-download
  :after org
  :commands (org-download-clipboard org-download-delete org-download-image org-download-yank org-download-edit org-download-rename-at-point org-download-rename-last-file org-download-screenshot)
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil)
  :config
  (cond (*sys/mac*
         (setq org-download-screenshot-method "screencapture -i %s"))))
;; -OrgDownload

(use-package valign
  :after org
  :bind (([remap org-table-align] . valign-table)))

(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq
   org-modern-checkbox nil
   org-modern-timestamp nil
   org-modern-priority nil
   org-modern-todo t
   org-modern-list nil
   org-modern-keyword nil
   org-agenda-block-separator ?─
   org-agenda-time-leading-zero 0
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")


  (setq org-tags-column 0
        org-auto-align-tags nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-insert-heading-respect-content t
        org-fold-catch-invisible-edits 'show-and-error
        org-agenda-tags-column 0)

  ;; avoid unneccesary calculations, I never need it.
  (defalias 'org-align-tags #'ignore))

(use-package org-modern-indent
  :ensure (:host github :repo "jdtsmith/org-modern-indent")
  :hook (org-mode . org-modern-indent-mode))

(use-package ob-mermaid
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)
     (scheme . t))))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
