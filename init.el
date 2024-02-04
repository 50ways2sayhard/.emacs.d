;;; package --- Summary
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
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory))))

;;; Package Manager
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))


(mapcar
 (lambda (p) (add-to-list 'elpaca-ignored-dependencies p))
 '(xref tramp tramp-sh flymake simple diff-mode smerge-mode python css-mode custom
        server help elec-pair paren recentf winner tab-bar hl-line pulse prog-mode
        lisp-mode treesit imenu eldoc transient hippie-exp eglot))

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

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package dash
  :config (global-dash-fontify-mode))

;;; Custom settings
(use-package custom
  :elpaca nil
  :no-require t
  :config
  (setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;;; Server mode
(use-package server
  :elpaca nil
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))

;;; Print startup time
(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Diff
(use-package diff-hl
  :commands (diff-hl-revert-hunk diff-hl-show-hunk)
  :hook ((find-file . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode))
  :bind
  (:map diff-hl-mode-map
        ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :config
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (add-hook 'after-change-major-mode-hook 'diff-hl-update-once)

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  (setq diff-hl-draw-borders nil))

(use-package diff-mode
  :elpaca nil
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

;;; Dired and Dirvish file browser
(use-package dired
  :elpaca nil
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

  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  (setq dired-open-extensions
        (mapcar (lambda (ext)
                  (cons ext "open")) '("pdf" "doc" "docx" "ppt" "pptx"))))

(use-package dirvish
  :after-call +my/first-input-hook-fun
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
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

  (use-package dirvish-extras
    :elpaca nil
    :after dirvish)

  (use-package dirvish-vc
    :elpaca nil
    :after dirvish))


;;; Documentation in echo area
(use-package eldoc
  :elpaca nil
  :commands (eldoc)
  :config (global-eldoc-mode))

(use-package help
  :elpaca nil
  :config (temp-buffer-resize-mode))

;;; Isearch
(progn ;    `isearch'
  (setq isearch-allow-scroll t
        isearch-lazy-count 1))

;;; Version controll
(use-package magit
  :commands (magit-status magit-open-repo magit-add-section-hook aborn/simple-git-commit-push magit-add-current-buffer-to-kill-ring)
  :bind
  (:map magit-mode-map
        ("x" . magit-discard)
        ("N" . magit-section-forward)
        ("P" . magit-section-backward)
        ("p" . magit-push)
        ("'" . magit-process-buffer))
  :config
  (setq magit-display-buffer-function #'+magit-display-buffer-fn)
  (setq magit-show-long-lines-warning nil)
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
    (let ((msg (read-string "Commit Message: ")))
      (when (length= msg 0)
        (setq msg (format-time-string "commit by magit in emacs@%Y-%m-%d %H:%M:%S"
                                      (current-time))))
      (magit-call-git "commit" "-m" msg)
      (when (magit-get "remote" "origin" "url")
        (magit-push-current-to-upstream nil)
        (message "now do async push to %s" (magit-get "remote" "origin" "url")))
      (magit-mode-bury-buffer)))

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

(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer.
For example, diffs and log buffers.  Accepts `left', `right', `up', and `down'.")
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional' displays BUFFER way, except...
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
This differs from `display-buffer-in-direction' in one way:
it will try to use a window that already exists in that direction.
It will split otherwise."
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
    (insert (cdr (assoc (completing-read "Choose a gitmoji " candidates) candidates)))))

(use-package hydra)

(use-package smerge-mode
  :elpaca nil
  :after magit
  :diminish
  :bind (:map smerge-mode-map
              ("C-c m" . hydra-smerge/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (hydra-smerge/body))))
         )
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t))
  (require 'hydra)
  (defhydra hydra-smerge (:hint nil
                                :pre (smerge-mode 1)
                                ;; Disable `smerge-mode' when quitting hydra if
                                ;; no merge conflicts remain.
                                :post (smerge-auto-leave))
    "
  ^Move^       ^Keep^               ^Diff^                 ^Other^
  ^^-----------^^-------------------^^---------------------^^-------
  _n_ext       _b_ase               _<_: upper/base        _C_ombine
  _p_rev       _u_pper              _=_: upper/lower       _r_esolve
  ^^           _l_ower              _>_: base/lower        _k_ill current
  ^^           _a_ll                _R_efine
  ^^           _RET_: current       _E_diff
  "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue)))

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


;;; Parenthesis
(use-package elec-pair
  :elpaca nil
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
  :hook ((emacs-lisp-mode markdown-mode org-mode) . puni-mode)
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
  :init
  :config
  (setq puni-confirm-when-delete-unbalanced-active-region nil))

(use-package fingertip
  :elpaca (:repo "manateelazycat/fingertip" :host github)
  :hook ((dart-ts-mode) . fingertip-mode)
  :config
  ;; (define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
  ;; (define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
  ;; (define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
  ;; (define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
  ;; (define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
  ;; (define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
  ;; (define-key fingertip-mode-map (kbd "=") 'fingertip-equal)

  ;; (define-key fingertip-mode-map (kbd "（") 'fingertip-open-chinese-round)
  ;; (define-key fingertip-mode-map (kbd "「") 'fingertip-open-chinese-bracket)
  ;; (define-key fingertip-mode-map (kbd "【") 'fingertip-open-chinese-curly)
  ;; (define-key fingertip-mode-map (kbd "）") 'fingertip-close-chinese-round)
  ;; (define-key fingertip-mode-map (kbd "」") 'fingertip-close-chinese-bracket)
  ;; (define-key fingertip-mode-map (kbd "】") 'fingertip-close-chinese-curly)

  (define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
  (define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
  (define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

  ;; (define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
  ;; (define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

  (define-key fingertip-mode-map (kbd "M-o") 'fingertip-backward-delete)
  (define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
  (define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

  (define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
  (define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
  (define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
  (define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
  (define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
  (define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

  (define-key fingertip-mode-map (kbd "M-p") 'fingertip-jump-right)
  (define-key fingertip-mode-map (kbd "M-n") 'fingertip-jump-left)
  ;; (define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-out-pair-and-newline)

  (define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-up))

(use-package paren
  :elpaca nil
  :hook (elpaca-after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis
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
                (+my/open-org-agenda)
                (message
                 "Initialization done in (%.3fs)"
                 (float-time (time-subtract (current-time) before-user-init-time)))) t)
  )

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
          envvars)))))

;;; Global configuration
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

  (setq-default indent-tabs-mode nil)
  (setq-default indent-line-function 'insert-tab)
  (setq-default tab-width 2)
  (setq-default js-switch-indent-offset 2)
  (add-hook 'after-change-major-mode-hook
            #'(lambda () (if (equal electric-indent-mode 't)
                             (when (derived-mode-p 'text-mode)
                               (electric-indent-mode -1))
                           (electric-indent-mode 1))))


  ;; When buffer is closed, saves the cursor location
  (save-place-mode 1)

  ;; for long line
  (setq-default bidi-display-reordering nil)
  (setq bidi-inhibit-bpa t
        long-line-threshold 1000
        large-hscroll-threshold 1000
        syntax-wholeline-max 1000)

  (setq-default create-lockfiles nil
                make-backup-files nil)
  (setq create-lockfiles nil
        make-backup-files nil)

  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output t)

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
  (setq split-width-threshold 0
        split-height-threshold nil)
  (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq redisplay-skip-fontification-on-input t)

  ;; Vertical Scroll
  (setq scroll-step 1
        scroll-margin 3
        ;; scroll-margin 0
        scroll-conservatively 100000
        auto-window-vscroll t
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01
        scroll-preserve-screen-position 'always)
  (when (display-graphic-p)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
          mouse-wheel-scroll-amount-horizontal 1
          mouse-wheel-progressive-speed nil))
  ;; (setq scroll-up-aggressively 0.01)
  ;; (setq scroll-down-aggressively 0.01)
  (setq mouse-wheel-progressive-speed t)
  (mouse-wheel-mode -1)
  (pixel-scroll-precision-mode)

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

  (defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; remap C-a to `smarter-move-beginning-of-line'
  (global-set-key [remap move-beginning-of-line]
                  'smarter-move-beginning-of-line)
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

(defun +my/replace (&optional word )
  "Make it easy to replace WORD.If WORD is not given, automatically extract it by major-mode."
  (interactive)
  (cond
   ((derived-mode-p 'occur-mode) (occur-edit-mode))
   ((derived-mode-p 'grep-mode) (wgrep-change-to-wgrep-mode))
   ((and (featurep 'deadgrep) (derived-mode-p 'deadgrep-mode))
    (wgrep-change-to-wgrep-mode)))
  (let* ((buffer-name (buffer-name))
         (raw-keyword (cond ((string-match "*Embark .* - \\(?1:.*\\)\\*" buffer-name)
                             (match-string 1 buffer-name))
                            ((buffer-match-p "\\*deadgrep.*" buffer-name)
                             (car deadgrep-history))
                            (t (read-string "Keyword: " word))))
         (keyword (replace-regexp-in-string "^#" "" raw-keyword))
         (replacement (read-string (format "Replace \"%s\" with: " keyword))))
    (vr/query-replace keyword replacement (point-min) (point-max))))

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

(defun hexcolour-luminance (color)
  "Calculate the luminance of a COLOR string (e.g. \"#ffaa00\", \"blue\").
This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun hexcolour-add-to-font-lock ()
  "Add hex color to font lock."
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
  :defer nil
  :config
  (require 'meow-config)
  (meow-setup)
  (meow-setup-indicator)
  (meow-global-mode)
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(comint-mode . insert))
  (add-to-list 'meow-mode-state-list '(magit-blame-mode . insert))
  (add-to-list 'meow-mode-state-list '(occur-mode . motion))
  (add-hook 'org-capture-mode-hook #'meow-insert)
  (add-to-list 'meow-mode-state-list '(git-timemachine-mode . insert)))

(use-package bind
  :elpaca (:host github :repo "repelliuss/bind")
  :functions (bind))

(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :hook (window-setup . which-key-mode))

;;; Minibuffer completion
(use-package embark
  :elpaca (embark :files (:defaults "*.el"))
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
   ("J" . dired)
   ("SPC" . +my/quick-look)
   :map embark-region-map
   ("V" . diff-hl-show-hunk)
   ("/" . evilnc-comment-or-uncomment-lines)
   ("=" . er/expand-region)
   (";" . embrace-commander)
   ("Y". #'evilnc-copy-and-comment-lines)
   :map embark-identifier-map
   (";" . embrace-commander)
   ("D" . xref-find-definitions-other-window))
  :custom
  (embark-cycle-key ".")
  (embark-help-key "?")
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
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

  ;; smerge integration
  (defun embark-target-smerge-at-point ()
    (when-let (((bound-and-true-p smerge-mode))
               (ov (cl-find-if (lambda (ov) (eq (overlay-get ov 'smerge) 'conflict))
                               (overlays-at (point)))))
      `(smerge-diff "conflict" ,(overlay-start ov) . ,(overlay-end ov))))

  (add-to-list 'embark-keymap-alist '(smerge-diff . smerge-basic-map))
  (add-to-list 'embark-target-finders 'embark-target-smerge-at-point)
  (add-to-list 'embark-repeat-actions 'smerge-next)
  (add-to-list 'embark-repeat-actions 'smerge-prev))

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

;;;; Minibuffer completion UI
(use-package vertico
  :elpaca (vertico :files (:defaults "extensions/vertico-*.el"))
  :hook (+my/first-input . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  :config
  ;; Configure directory extension.
  (use-package vertico-quick
    :elpaca nil
    :after vertico
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))

  (use-package vertico-repeat
    :elpaca nil
    :after vertico
    :bind ("C-c r" . vertico-repeat)
    :hook (minibuffer-setup . vertico-repeat-save))

  (use-package vertico-directory
    :elpaca nil
    :after vertico
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)
                ("C-w" . vertico-directory-up))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package emacs
  :elpaca nil
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
  (setq enable-recursive-minibuffers t)
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t))

(use-package consult
  ;; :after orderless
  :after fussy
  :commands (noct-consult-ripgrep-or-line consult-clock-in +consult-ripgrep-current-directory)
  :bind (([remap recentf-open-files] . consult-recent-file)
         ([remap imenu] . consult-imenu)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap swich-to-buffer-other-window] . consult-buffer-other-window)
         ([remap goto-line] . consult-goto-line)
         ([remap yank-pop] . consult-yank-from-kill-ring)
         ("M-?" . consult-ripgrep)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-s bindings (search-map)
         ("M-s m" . consult-multi-occur)
         ;; Isearch integration
         ("M-s e" . consult-isearch))
  :config
  (require 'embark-consult)
  (setq consult-preview-key '(:debounce 1.0 any))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-args "fd --color=never --full-path ARG OPTS")
  (setq consult-ripgrep-args
        "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

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

  (defcustom noct-consult-ripgrep-or-line-limit 300000
    "Buffer size threshold for `noct-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
    :type 'integer)

  ;; simulate counsel-grep-or-swiper
  (defun noct-consult-ripgrep-or-line ()
    "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
    (interactive)
    (if (or (not buffer-file-name)
            (buffer-narrowed-p)
            (ignore-errors
              (file-remote-p buffer-file-name))
            (jka-compr-get-compression-info buffer-file-name)
            (<= (buffer-size)
                (/ noct-consult-ripgrep-or-line-limit
                   (if (eq major-mode 'org-mode) 4 1))))
        (progn (consult-line)
               ;; (my-consult-set-evil-search-pattern)
               )

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
        ;; (my-consult-set-evil-search-pattern 'rg)
        )))

  ;; Configure initial narrowing per command
  ;; (dolist (src consult-buffer-sources)
  ;;   (unless (eq src 'consult--source-buffer)
  ;;     (set src (plist-put (symbol-value src) :hidden t))))

  (defun +consult-ripgrep-current-directory (&optional initial)
    (interactive)
    (consult-ripgrep default-directory initial))

  ;; (defun consult--orderless-regexp-compiler (input type &rest _config)
  ;;   (setq input (orderless-pattern-compiler input))
  ;;   (cons
  ;;    (mapcar (lambda (r) (consult--convert-regexp r type)) input)
  ;;    (lambda (str) (orderless--highlight input t str))))
  ;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

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
           (dir (file-name-directory file))
           short-name)
      (setq short-name
            (if dir
                (format "%s/%s" (file-name-nondirectory (directory-file-name dir)) filename)
              filename))
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
                :enabled  ,(lambda () (getenv "ZLUA_SCRIPT"))
                :items    ,#'consult-dir--zlua-dirs)
    "Fasd directory source for `consult-dir'.")
  (setq consult-dir-sources '(consult-dir--source-recentf consult-dir--source-zlua consult-dir--source-project)))

(use-package consult-project-extra
  :commands (consult-project-extra-find)
  :after consult
  :config
  (defun consult-project-extra--find-with-concat-root (candidate)
    "Find-file concatenating root with CANDIDATE."
    (find-file candidate))

  ;; WORKAROUND Embark action on project source, eg. find-file-other-window
  ;; FIXME: I don't know how to set full minibuffer contents for file candidate in 'consult--read'.
  (defun consult-project-extra--file (selected-root)
    "Create a view for selecting project files for the project at SELECTED-ROOT."
    ;; (let ((candidate (consult--read
    ;;                   (consult-project-extra--project-files selected-root t)
    ;;                   :prompt "Project File: "
    ;;                   :sort t
    ;;                   :require-match t
    ;;                   :category 'file
    ;;                   :state (consult--file-preview)
    ;;                   :history 'file-name-history)))
    ;;   (find-file candidate))
    (consult-fd selected-root)
    )

  (defun consult-project-extra--project-files (root &optional include-root)
    "Compute the project files given the ROOT."
    (let* ((project (consult-project-extra--project-with-root root))
           (files (project-files project)))
      (mapcar (lambda (f)
                (let* ((filename (file-relative-name f root))
                       (abs-filename (expand-file-name f root))
                       (result (if include-root (abbreviate-file-name abs-filename) filename)))
                  (propertize result 'multi-category `(file . ,(abbreviate-file-name abs-filename))))) files))))

;;;; Better xxx-thing-at-point,
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

;; (use-package orderless
;;   :after-call +my/first-input-hook-fun
;;   :demand t
;;   :config
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         orderless-component-separator #'orderless-escapable-split-on-space
;;         completion-category-overrides '((file (styles basic partial-completion))
;;                                         (eglot-capf (styles orderless-literal basic))
;;                                         (eglot (styles orderless-literal basic))
;;                                         ))
;;   )

(use-package fussy
  :after-call +my/first-input-hook-fun
  :demand t
  :custom
  (completion-styles '(fussy basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot-capf (styles fussy basic))
                                   (eglot (styles fussy basic))
                                   ))
  (fussy-filter-fn 'fussy-filter-default)
  (fussy-use-cache t)
  :config
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil))))

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
        '((max-width . 0.6)
          (min-width . 0.6)
          (left-fringe . 8)
          (right-fringe . 8))))

;;; Icons
(use-package nerd-icons
  :config
  (add-to-list 'nerd-icons-mode-icon-alist '(dart-ts-mode nerd-icons-devicon "nf-dev-dart" :face nerd-icons-blue)))
(use-package nerd-icons-completion
  :elpaca (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
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
(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/corfu-*.el"))
  :after-call +my/first-input-hook-fun
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
        ([backtab] . corfu-previous)
        ([remap move-beginning-of-line] . nil)
        ([remap move-end-of-line] . nil))
  :config
  (global-corfu-mode)
  (use-package corfu-quick
    :elpaca nil
    :commands (corfu-quick-insert corfu-quick-complete)
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)
          ("M-q" . corfu-quick-complete)))

  (use-package corfu-history
    :elpaca nil
    :hook (corfu-mode . corfu-history-mode))

  (use-package corfu-popupinfo
    :elpaca nil
    :hook (corfu-mode . corfu-popupinfo-mode)
    :config
    (set-face-attribute 'corfu-popupinfo nil :height 140)
    (setq corfu-popupinfo-delay '(0.4 . 0.2)))

  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

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
  :elpaca nil
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

(use-package cape
  :after (corfu tempel)
  :commands (my/convert-super-capf my/set-eglot-capf)
  :hook ((text-mode . (lambda ()
                        (my/convert-super-capf #'cape-dabbrev)))
         (yaml-ts-mode . (lambda ()
                           (my/convert-super-capf #'cape-dabbrev)))
         (emacs-lisp-mode . (lambda ()
                              (my/convert-super-capf #'elisp-completion-at-point)))
         (org-mode . my/set-basic-capf))
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (setq cape-dict-file "/usr/share/dict/words")
  (setq cape-dabbrev-check-other-buffers nil)

  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     (if +self/enable-tabnine
         (cape-super-capf
          arg-capf
          #'tabnine-completion-at-point
          #'tempel-complete)
       (cape-capf-super
        arg-capf))))

  (defun my/set-basic-capf ()
    (setq-local completion-at-point-functions (my/convert-super-capf (car completion-at-point-functions))))

  (defun my/set-eglot-capf ()
    (setq-local completion-at-point-functions (my/convert-super-capf #'eglot-completion-at-point)))

  (setq-default completion-at-point-functions '(cape-file cape-dabbrev)))

(use-package tabnine-capf
  :when +self/enable-tabnine
  :after cape
  :elpaca (:host github :repo "50ways2sayhard/tabnine-capf")
  :commands (tabnine-completion-at-point tabnine-capf-start-process)
  :hook ((kill-emacs . tabnine-capf-kill-process)))

(use-package copilot
  :when +self/enable-copilot
  :after corfu
  :hook ((prog-mode . copilot-mode)
         (copilot-mode . (lambda ()
                           (setq-local copilot--indent-warning-printed-p t))))
  :elpaca (:host github :repo "copilot-emacs/copilot.el"
                 :files ("dist" "*.el"))
  :bind
  ((:map copilot-completion-map
         ("C-i" . copilot-accept-completion)
         ("M-i" . copilot-accept-completion-by-word)
         ("M-n" . 'copilot-next-completion)
         ("M-p" . 'copilot-previous-completion)))
  :init
  (require 'copilot-balancer)
  :config
  (defun +my/corfu-candidates-p ()
    (or (not (eq corfu--candidates nil))
        tempel--active ;; diable copilot in tempel
        (not (looking-back "[\x00-\xff]"))))

  (add-to-list 'copilot--indentation-alist
               '(dart-ts-mode dart-ts-mode-indent-offset))

  (setq warning-minimum-level :error)

  (customize-set-variable 'copilot-enable-predicates '(meow-insert-mode-p))
  (customize-set-variable 'copilot-disable-predicates '(+my/corfu-candidates-p evil-ex-p minibufferp))
  (setq copilot-max-char 1000000))

(use-package starhugger
  :when (and (not +self/enable-copilot) (length> starhugger-api-token 0))
  :elpaca (:repo "https://gitlab.com/daanturo/starhugger.el" :files (:defaults "*.py"))
  :hook (prog-mode . starhugger-auto-mode)
  :after corfu
  :bind
  (("C-M-i" . #'starhugger-accept-suggestion)
   :map starhugger-inlining-mode-map
   ("TAB" . #'starhugger-accept-suggestion)
   ("M-[" . #'starhugger-show-prev-suggestion)
   ("M-]" . #'starhugger-show-next-suggestion)
   ("M-f" . #'starhugger-accept-suggestion-by-word)))

;;; Utils
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :diminish
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

(use-package recentf
  :elpaca nil
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
  :elpaca nil
  :config (column-number-mode))

(use-package tramp
  :elpaca nil
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
  :elpaca nil
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

(use-package pinyinlib
  :disabled
  :after-call +my/first-input-hook-fun
  :after orderless
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vundo
  :commands vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package visual-regexp)

(use-package visual-regexp-steroids)

(use-package maple-translate
  :elpaca (:host github :repo "honmaple/emacs-maple-translate")
  :commands (maple-translate maple-translate+ maple-translate-posframe)
  :bind
  ("C-c t y" . maple-translate-posframe)
  ("C-c t Y" . (lambda () (interactive) (maple-translate+ (read-from-minibuffer "Translate word: "))))
  :custom
  (maple-translate-buffer " *maple-translate* ")
  :config
  ;; (setq maple-translate-engine '(google dictcn youdao))
  (setq maple-translate-engine '(google))

  ;; with google translate
  (setq maple-translate-google-url "https://translate.googleapis.com/translate_a/single")


  (defun maple-translate-posframe-tip (result)
    "Show STRING using posframe-show."
    (unless (and (require 'posframe nil t) (posframe-workable-p))
      (error "Posframe not workable"))

    (if result
        (progn
          (with-current-buffer (get-buffer-create maple-translate-buffer)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert result)
              (maple-translate-mode)
              (goto-char (point-min))))
          (posframe-show maple-translate-buffer
                         :left-fringe 8
                         :right-fringe 8
                         :internal-border-color (face-foreground 'default)
                         :internal-border-width 1)
          (unwind-protect
              (push (read-event) unread-command-events)
            (progn
              (posframe-hide maple-translate-buffer))))
      (message "Nothing to look up")))

  (defun maple-translate-posframe(word)
    "Translate WORD and display result in posframe."
    (interactive (list (maple-translate-word)))
    (maple-translate-show word 'maple-translate-posframe-tip)))


;; SaveAllBuffers
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
  (interactive)
  (save-some-buffers t))
;; -SaveAllBuffers

(use-package project
  :elpaca nil
  :commands (project-find-file project-switch-project)
  :config
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

  (defvar project--ignore-list
    '("~/fvm"
      "~/.pub-cache"))

  (defun my-project--ignored-p (path)
    (when path
      (catch 'found
        (dolist (ignore project--ignore-list)
          (when (string-prefix-p (file-truename ignore) (file-truename path))
            (throw 'found t))))))

  (cl-defmethod project-files ((project (head transient)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project)))))

  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" "pubspec.yaml" "deps.edn" "shadow-cljs.edn")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)

  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (and (file-exists-p (concat path marker)) (not (my-project--ignored-p path)))
          (throw 'found marker)))))

  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (let ((path (expand-file-name path)))
      (catch 'found
        (while (not (equal "/" path))
          (if (not (project-root-p path))
              (setq path (file-name-directory (directory-file-name path)))
            (throw 'found (cons 'transient path)))))))
  (require 'project)
  (add-to-list 'project-find-functions #'project-find-root)
  )

(use-package project-rootfile
  :elpaca (:repo "https://github.com/buzztaiki/project-rootfile.el")
  :config
  (add-to-list 'project-rootfile-list "melos.yaml"))

(use-package winner
  :elpaca nil
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

(use-package tab-bar
  :elpaca nil
  :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t)

  ;; 使用 super-1 super-2 ... 来切换 tab
  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                          (count (length (window-list-1 nil 'nomini)))
                          (truncated-tab-name (if (< (length raw-tab-name)
                                                     tab-bar-tab-name-truncated-max)
                                                  raw-tab-name
                                                (truncate-string-to-width raw-tab-name
                                                                          tab-bar-tab-name-truncated-max
                                                                          nil nil tab-bar-tab-name-ellipsis))))
                     (if (> count 1)
                         (concat truncated-tab-name "(" (number-to-string count) ")")
                       truncated-tab-name))))

  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; 我把 meow 的 indicator 也放在 tab-bar 上
  (setq tab-bar-format '(meow-indicator  tab-bar-format-tabs))
  (tab-bar--update-tab-bar-lines)

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
  ;; (tabspaces-session-auto-restore t)
  :config
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
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
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package helpful
  :bind
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command)
  ("C-h ." . helpful-at-point))

(use-package popper
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
          "^\\*vterm.*\\*$"  vterm-mode
          "^\\*eat.*\\*$"    eat-mode

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
  :config
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
(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-buffer-modification-icon t))

(use-package catppuccin-theme
  :defer nil
  :config
  (setq catppuccin-flavor 'macchiato)
  (setq catppuccin-highlight-matches t)
  ;; (load-theme 'catppuccin :no-confirm)
  )

(use-package ef-themes
  :init
  (ef-themes-select 'ef-trio-light)
  :config
  (setq ef-themes-headings
        '((0 . (bold 1))
          (1 . (bold 1))
          (2 . (rainbow bold 1))
          (3 . (rainbow bold 1))
          (4 . (rainbow bold 1))
          (t . (rainbow bold 1))))
  (setq ef-themes-region '(intense no-extend neutral))
  ;; Disable all other themes to avoid awkward blending:
  ;; (mapc #'disable-theme custom-enabled-themes)
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
    (+my-custom-org-todo-faces))
  (with-eval-after-load 'kind-icon
    (add-hook 'ef-themes-post-load-hook #'kind-icon-reset-cache)))

(use-package spacious-padding
  :hook (window-setup . spacious-padding-mode))

(when (display-graphic-p)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-table ((t (:family "Maple Mono SC NF")))))
  (advice-add #'org-string-width :override #'org--string-width-1)

  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-fontset-font t 'han (font-spec :family "Maple Mono SC NF")))

(use-package composite
  :elpaca nil
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook (((prog-mode
           conf-mode nxml-mode markdown-mode help-mode
           shell-mode eshell-mode term-mode vterm-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
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
  :elpaca nil
  :custom-face (hl-line ((t (:extend t))))
  :hook ((elpaca-after-init . global-hl-line-mode)
         ((term-mode vterm-mode) . hl-line-unload-function)))

;; Colorize color names in buffers
(use-package rainbow-mode
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

(use-package consult-todo
  :elpaca (:host github :repo "liuyinz/consult-todo")
  :demand t)

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
  :elpaca nil
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
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
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
  :elpaca nil
  :after-call +my/first-input-hook-fun
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  (add-hook 'flymake-project-diagnostics-mode-hook #'my/flymake-sort-by-type-desc)
  (add-hook 'flymake-diagnostics-buffer-mode-hook #'my/flymake-sort-by-type-desc)
  (defun my/flymake-sort-by-type-desc ()
    (setq-local tabulated-list-sort-key (cons "Type" t)))
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
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe))

(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode))

(use-package jinx
  :elpaca (:repo "minad/jinx")
  :bind (([remap ispell-word] . #'jinx-correct))
  :hook ((text-mode) . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

;;; Better edit
(use-package apheleia
  :commands (apheleia-format-buffer)
  :bind ("C-c c f" . apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-mode-alist '(dart-ts-mode . dart-format))
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent)))

(use-package delete-block
  :elpaca (:repo "manateelazycat/delete-block" :host github)
  :commands (delete-block-backward)
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))

(use-package ws-butler
  :diminish
  :hook (window-setup . ws-butler-global-mode))

(use-package wgrep
  :elpaca (:files (:defaults "*.el"))
  :commands wgrep-change-to-wgrep-mode
  :bind
  (:map grep-mode-map
        ("i" . #'wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t)
  :init
  (define-key occur-mode-map (kbd "i") 'occur-edit-mode))

(use-package deadgrep
  :init
  (require 'wgrep-deadgrep)
  :bind
  ("<f5>" . #'deadgrep)
  (:map deadgrep-mode-map
        ("i" . #'wgrep-change-to-wgrep-mode)))

(use-package expand-region
  :commands (er/expand-region)
  :config
  (defun treesit-mark-bigger-node ()
    "Use tree-sitter to mark regions."
    (let* ((root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
        (when-let ((node (treesit-node-parent node)))
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
  (add-to-list 'er/try-expand-list 'treesit-mark-bigger-node))

(use-package avy
  :diminish
  :demand t
  :commands (avy-goto-char avy-goto-line))

(use-package embrace
  :commands (embrace-commander)
  :bind ("C-;" . embrace-commander))

(use-package separedit
  :custom
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-default-mode 'markdown-mode)
  :config
  (add-to-list 'separedit-comment-delimiter-alist '(("///" "//") . (dart-mode dart-ts-mode))))

(use-package speedrect
  :elpaca (:repo "jdtsmith/speedrect" :host github)
  :init
  (require 'speedrect))

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
                         rime-librime-root "~/.local/share/librime/dist/"))
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

  (with-eval-after-load 'ef-themes
    (ef-themes-with-colors
      (setq im-cursor-color (face-foreground 'warning)))))

(use-package super-save
  :hook (window-setup . super-save-mode)
  :init
  (setq auto-save-default nil)
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (add-to-list 'super-save-triggers 'switch-to-buffer)
  (add-to-list 'super-save-triggers 'eglot-rename)
  (add-to-list 'super-save-triggers 'consult-buffer)
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
  :elpaca nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (global-prettify-symbols-mode))

(use-package vs-comment-return
  :elpaca (:host github :repo "emacs-vs/vs-comment-return")
  :hook (prog-mode . vs-comment-return-mode)
  :custom
  (vs-comment-return-cancel-after t))

(use-package better-jumper
  :after-call +my/first-input-hook-fun
  :commands better-jump-set-jump-a better-jumper-mode
  :init
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
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
  :defer nil
  :after-call +my/first-input-hook-fun
  :commands dumb-jump-result-follow dumb-jump-go dumb-jump-xref-activate
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'xref-after-jump-hook #'better-jumper-set-jump)
  (add-hook 'xref-after-return-hook #'better-jumper-set-jump))

(use-package lisp-mode
  :elpaca nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook 'indent-spaces-mode))

;;;; Lsp integration
(use-package eglot
  :elpaca nil
  :commands (+eglot-organize-imports +eglot-help-at-point)
  :hook ((eglot-managed-mode . (lambda ()
                                 (+lsp-optimization-mode)
                                 (setq eldoc-documentation-functions
                                       (cons #'flymake-eldoc-function
                                             (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                                 ;; Show all eldoc feedback.
                                 (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
                                 (if (or (boundp 'lsp-bridge-mode) (boundp 'lspce-mode))
                                     (setq completion-at-point-functions (remove #'eglot-completion-at-point completion-at-point-functions))
                                   (my/set-eglot-capf))
                                 (when (boundp 'lspce-mode)
                                   eglot-stay-out-of '(eldoc))
                                 ))
         (prog-mode . (lambda ()
                        (unless (or (derived-mode-p 'emacs-lisp-mode 'makefile-mode)
                                     (my-project--ignored-p (buffer-file-name (current-buffer))))
                          (eglot-ensure)))))
  :init
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
  :config
  (setq
   eglot-send-changes-idle-time 0.5
   eglot-autoshutdown t
   eglot-extend-to-xref t
   eglot-confirm-server-initiated-edits nil
   eglot-sync-connect nil
   eglot-events-buffer-config '(:size 0 :format full)
   eglot-report-progress nil)
  (setq eldoc-echo-area-use-multiline-p 5)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
  (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

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
          :lint t))

  (defun old-eglot-completion-at-point ()
    "Eglot's `completion-at-point' function."
    ;; Commit logs for this function help understand what's going on.
    (when-let (completion-capability (eglot--server-capable :completionProvider))
      (let* ((server (eglot--current-server-or-lose))
             (sort-completions
              (lambda (completions)
                (cl-sort completions
                         #'string-lessp
                         :key (lambda (c)
                                (plist-get
                                 (get-text-property 0 'eglot--lsp-item c)
                                 :sortText)))))
             (metadata `(metadata (category . eglot)
                                  (display-sort-function . ,sort-completions)))
             resp items (cached-proxies :none)
             (proxies
              (lambda ()
                (if (listp cached-proxies) cached-proxies
                  (setq resp
                        (jsonrpc-request server
                                         :textDocument/completion
                                         (eglot--CompletionParams)
                                         :deferred :textDocument/completion
                                         :cancel-on-input t))
                  (setq items (append
                               (if (vectorp resp) resp (plist-get resp :items))
                               nil))
                  (setq cached-proxies
                        (mapcar
                         (jsonrpc-lambda
                             (&rest item &key label insertText insertTextFormat
                                    textEdit &allow-other-keys)
                           (let ((proxy
                                  ;; Snippet or textEdit, it's safe to
                                  ;; display/insert the label since
                                  ;; it'll be adjusted.  If no usable
                                  ;; insertText at all, label is best,
                                  ;; too.
                                  (cond ((or (eql insertTextFormat 2)
                                             textEdit
                                             (null insertText)
                                             (string-empty-p insertText))
                                         (string-trim-left label))
                                        (t insertText))))
                             (unless (zerop (length proxy))
                               (put-text-property 0 1 'eglot--lsp-item item proxy))
                             proxy))
                         items)))))
             (resolved (make-hash-table))
             (resolve-maybe
              ;; Maybe completion/resolve JSON object `lsp-comp' into
              ;; another JSON object, if at all possible.  Otherwise,
              ;; just return lsp-comp.
              (lambda (lsp-comp)
                (or (gethash lsp-comp resolved)
                    (setf (gethash lsp-comp resolved)
                          (if (and (eglot--server-capable :completionProvider
                                                          :resolveProvider)
                                   (plist-get lsp-comp :data))
                              (jsonrpc-request server :completionItem/resolve
                                               lsp-comp :cancel-on-input t)
                            lsp-comp)))))
             (bounds (bounds-of-thing-at-point 'symbol)))
        (list
         (or (car bounds) (point))
         (or (cdr bounds) (point))
         (lambda (probe pred action)
           (cond
            ((eq action 'metadata) metadata)               ; metadata
            ((eq action 'lambda)                           ; test-completion
             (test-completion probe (funcall proxies)))
            ((eq (car-safe action) 'boundaries) nil)       ; boundaries
            ((null action)                                 ; try-completion
             (try-completion probe (funcall proxies)))
            ((eq action t)                                 ; all-completions
             (all-completions
              ""
              (funcall proxies)
              (lambda (proxy)
                (let* ((item (get-text-property 0 'eglot--lsp-item proxy))
                       (filterText (plist-get item :filterText)))
                  (and (or (null pred) (funcall pred proxy))
                       (string-prefix-p
                        probe (or filterText proxy) completion-ignore-case))))))))
         :annotation-function
         (lambda (proxy)
           (eglot--dbind ((CompletionItem) detail kind)
               (get-text-property 0 'eglot--lsp-item proxy)
             (let* ((detail (and (stringp detail)
                                 (not (string= detail ""))
                                 detail))
                    (annotation
                     (or detail
                         (cdr (assoc kind eglot--kind-names)))))
               (when annotation
                 (concat " "
                         (propertize annotation
                                     'face 'font-lock-function-name-face))))))
         :company-kind
         ;; Associate each lsp-item with a lsp-kind symbol.
         (lambda (proxy)
           (when-let* ((lsp-item (get-text-property 0 'eglot--lsp-item proxy))
                       (kind (alist-get (plist-get lsp-item :kind)
                                        eglot--kind-names)))
             (pcase kind
               ("EnumMember" 'enum-member)
               ("TypeParameter" 'type-parameter)
               (_ (intern (downcase kind))))))
         :company-deprecated
         (lambda (proxy)
           (when-let ((lsp-item (get-text-property 0 'eglot--lsp-item proxy)))
             (or (seq-contains-p (plist-get lsp-item :tags)
                                 1)
                 (eq t (plist-get lsp-item :deprecated)))))
         :company-docsig
         ;; FIXME: autoImportText is specific to the pyright language server
         (lambda (proxy)
           (when-let* ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy))
                       (data (plist-get (funcall resolve-maybe lsp-comp) :data))
                       (import-text (plist-get data :autoImportText)))
             import-text))
         :company-doc-buffer
         (lambda (proxy)
           (let* ((documentation
                   (let ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy)))
                     (plist-get (funcall resolve-maybe lsp-comp) :documentation)))
                  (formatted (and documentation
                                  (eglot--format-markup documentation))))
             (when formatted
               (with-current-buffer (get-buffer-create " *eglot doc*")
                 (erase-buffer)
                 (insert formatted)
                 (current-buffer)))))
         :company-require-match 'never
         :company-prefix-length
         (save-excursion
           (when (car bounds) (goto-char (car bounds)))
           (when (listp completion-capability)
             (looking-back
              (regexp-opt
               (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
              (eglot--bol))))
         :exit-function
         (lambda (proxy status)
           (when (memq status '(finished exact))
             ;; To assist in using this whole `completion-at-point'
             ;; function inside `completion-in-region', ensure the exit
             ;; function runs in the buffer where the completion was
             ;; triggered from.  This should probably be in Emacs itself.
             ;; (github#505)
             (with-current-buffer (if (minibufferp)
                                      (window-buffer (minibuffer-selected-window))
                                    (current-buffer))
               (eglot--dbind ((CompletionItem) insertTextFormat
                              insertText textEdit additionalTextEdits label)
                   (funcall
                    resolve-maybe
                    (or (get-text-property 0 'eglot--lsp-item proxy)
                        ;; When selecting from the *Completions*
                        ;; buffer, `proxy' won't have any properties.
                        ;; A lookup should fix that (github#148)
                        (get-text-property
                         0 'eglot--lsp-item
                         (cl-find proxy (funcall proxies) :test #'string=))))
                 (let ((snippet-fn (and (eql insertTextFormat 2)
                                        (eglot--snippet-expansion-fn))))
                   (cond (textEdit
                          ;; Undo (yes, undo) the newly inserted completion.
                          ;; If before completion the buffer was "foo.b" and
                          ;; now is "foo.bar", `proxy' will be "bar".  We
                          ;; want to delete only "ar" (`proxy' minus the
                          ;; symbol whose bounds we've calculated before)
                          ;; (github#160).
                          (delete-region (+ (- (point) (length proxy))
                                            (if bounds
                                                (- (cdr bounds) (car bounds))
                                              0))
                                         (point))
                          (eglot--dbind ((TextEdit) range newText) textEdit
                            (pcase-let ((`(,beg . ,end)
                                         (eglot--range-region range)))
                              (delete-region beg end)
                              (goto-char beg)
                              (funcall (or snippet-fn #'insert) newText))))
                         (snippet-fn
                          ;; A snippet should be inserted, but using plain
                          ;; `insertText'.  This requires us to delete the
                          ;; whole completion, since `insertText' is the full
                          ;; completion's text.
                          (delete-region (- (point) (length proxy)) (point))
                          (funcall snippet-fn (or insertText label))))
                   (when (cl-plusp (length additionalTextEdits))
                     (eglot--apply-text-edits additionalTextEdits)))
                 (eglot--signal-textDocument/didChange)))))))))

  (advice-add 'eglot-completion-at-point :override #'old-eglot-completion-at-point)
  )

(use-package eglot-booster
  :elpaca (:repo "jdtsmith/eglot-booster" :host github)
  :after eglot
  :when (executable-find "emacs-lsp-booster")
  :config
  (eglot-booster-mode))

(use-package dape
  :elpaca (:host github :repo "svaante/dape")
  :hook (dape-active-mode . dape-breakpoint-global-mode)
  :config
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)
  (add-hook 'dape-on-stopped-hooks 'dape-transient)

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

  (transient-define-prefix dape-transient ()
    "Transient for dape."
    [["Stepping"
      ("n" "Next" dape-next :transient t)
      ("i" "Step in" dape-step-in :transient t)
      ("o" "Step out" dape-step-out :transient t)
      ("c" "Continue" dape-continue :transient t)
      ("r" "restart" dape-restart :transient t)]
     ["Breakpoints"
      ("bb" "Toggle" dape-breakpoint-toggle :transient t)
      ("bd" "Remove all" dape-breakpoint-remove-all :transient t)
      ("bl" "Add log breakpoing" dape-breakpoint-log :transient t)
      ("be" "Add expression breakpoint" dape-breakpoint-expression :transient t)
      ]
     ["Switch"
      ("i" "Info" dape-info)
      ("R" "Repl" dape-repl)
      ("m" "Memory" dape-read-memory)
      ("t" "Thread" dape-select-thread)
      ("w" "Watch" dape-watch-dwim)
      ("S" "Stack" dape-select-stack)]
     ["Quit"
      ("qq" "Quit" dape-quit :transient nil)
      ("qk" "Kill" dape-kill :transient nil)]])

  (require 'flutter)

  (defun dape--flutter-devices ()
    (let* ((collection (flutter--devices))
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
    (dape--with dape-request ((dape--live-connection t) "hotRestart" nil)))

  (defun dape-flutter-hotReload ()
    (interactive)
    (dape-request (dape--live-connection t) "hotReload" nil))

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
          (browse-url (format "%s?uri=%s" devtools-addr vm-service-uri))
          ))))
  )

(use-package breadcrumb
  :elpaca (:host github :repo "joaotavora/breadcrumb")
  :hook (elpaca-after-init . breadcrumb-mode))

(use-package consult-eglot
  :after (consult eglot)
  :commands (consult-eglot-symbols))

;;;; Builtin tree sitter
(use-package treesit
  :elpaca nil
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
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
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
  :config
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

;;;; Online document
(use-package devdocs
  :commands (devdocs-lookup-at-point devdocs-search-at-point +devdocs-lookup-at-point +devdocs-search-at-point devdocs-dwim)
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
  (defun +devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))

  (defun +devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol)))

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
    (devdocs-lookup nil (thing-at-point 'symbol t))))

;;;; recenter after imenu jump
(use-package imenu
  :elpaca nil
  :commands (imenu)
  :hook (imenu-after-jump . recenter))

(use-package symbols-outline
  :commands (symbols-outline-show)
  :elpaca (:files (:defaults "*.el" "icons"))
  :config
  (setq-local symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (setq symbols-outline-window-position 'left)
  (setq symbols-outline-use-nerd-icon-in-gui t)
  (symbols-outline-follow-mode))

(use-package xref
  :elpaca nil
  :defer nil
  :init
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  (setq xref-history-storage 'xref-window-local-history)
  :hook ((xref-after-return xref-after-jump) . recenter))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package python
  :elpaca nil
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
                                (setq-local tab-width 4))))

(use-package pyvenv
  :after python)

(use-package py-isort
  :after python
  :hook (python-mode . (lambda ()
                         (add-hook 'before-save-hook #'py-isort-before-save))))


(use-package web-mode
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
                     (my/web-vue-setup))))))

(use-package css-mode
  :elpaca nil
  :mode ("\\.css\\'" "\\.wxss\\'")
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

;; EmmetPac
(use-package emmet-mode
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  ;; :bind (:map web-mode-map
  ;;             ("C-j" . emmet-expand-yas))
  :config
  (add-hook 'emmet-mode-hook (lambda()
                               (setq emmet-indent-after-insert t))))
;; -EmmetPac

(use-package dart-ts-mode
  :mode ("\\.dart\\'" . dart-ts-mode)
  :elpaca (:repo "50ways2sayhard/dart-ts-mode" :host github)
  :init
  (defvar dart-lsp-command '("dart" "language-server" "--client-id" "emacs-eglot-dart"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 (cons 'dart-ts-mode (if (executable-find "fvm")
                                         (add-to-list 'dart-lsp-command "fvm")
                                       dart-lsp-command))))
  :config
  (add-to-list 'project-vc-extra-root-markers "pubspec.yaml")
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(dart-ts-mode :types
                                                      ((?c "Class"    font-lock-type-face)
                                                       (?e "Enum" font-lock-type-face)
                                                       (?E "EnumMember" font-lock-variable-name-face)
                                                       (?V "Constructor" font-lock-type-face)
                                                       (?C "Constant"    font-lock-constant-face)
                                                       (?f "Function"  font-lock-function-name-face)
                                                       (?m "Method"  font-lock-function-name-face)
                                                       (?p "Property" font-lock-variable-name-face)
                                                       (?F "Field"  font-lock-variable-name-face))))))

(use-package flutter
  :elpaca (:repo "50ways2sayhard/flutter.el" :host github)
  :after dart-ts-mode
  :init
  :config

  (with-eval-after-load 'bind
    (bind
     dart-ts-mode-map
     (bind-prefix "C-x ,"
       ;; "r" #'flutter-run-or-hot-reload
       "r" (lambda () (interactive)
             (if (and (featurep 'dape) (dape--live-connection t))
                 (dape-flutter-hotReload)
               (flutter-run-or-hot-reload)))
       ;; "R" #'flutter-run-or-hot-restart
       "R" (lambda () (interactive)
             (if (and (featurep 'dape) (dape--live-connection t))
                 (dape-flutter-hotRestart)
               (flutter-run-or-hot-restart)))
       "v" #'flutter-open-devtools
       "Q" #'flutter-quit
       "p" #'flutter-pub-get
       "tt" #'flutter-test-at-point
       "tf" #'flutter-test-current-file
       "tF" #'flutter-test-all)))

  (defvar flutter--modeline-device nil)

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
  )

;;; Terminal integration
(use-package vterm
  :disabled
  :commands (vterm--internal vterm-posframe-toggle +my/smart-switch-to-vterm-tab)
  :bind
  (("C-0" . #'vterm-posframe-toggle)
   :map vterm-mode-map
   ("M-v" . #'yank)
   ("C-x" . #'vterm--self-insert)
   ("C-s" . #'tab-bar-switch-to-recent-tab)
   ("s-<escape>" . #'vterm-send-escape))
  :init
  (setq vterm-always-compile-module t)
  (setq vterm-shell "fish")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
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
                   ;; :poshandler #'posframe-poshandler-frame-center
                   :poshandler #'posframe-poshandler-frame-top-center
                   :hidehandler #'vterm-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width (frame-width)
                   :height (/ (frame-height) 4)
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
            (select-frame-set-input-focus vterm-posframe--frame))))))
  :config
  (defvar +my-vterm-tab-buffer-name "vterm-tab")
  (defvar +my-vterm-tab-name "vterm")
  (defun +my/smart-switch-to-vterm-tab ()
    "Switch to vterm tab if exists, otherwise create a new vterm tab."
    (interactive)
    (defvar vterm-buffer-name)
    (defvar vterm-shell)
    (let ((vterm-buffer-name +my-vterm-tab-buffer-name))
      (if (get-buffer vterm-buffer-name)
          (progn
            (tab-bar-select-tab-by-name "vterm")
            (switch-to-buffer vterm-buffer-name))
        (let ((vterm-shell "tmux"))
          (tab-new)
          (tab-bar-rename-tab +my-vterm-tab-name)
          (call-interactively #'vterm)
          (delete-other-windows)))))

  (defun +my/smart-vterm-find-file (filename)
    (interactive)
    (if (string= (buffer-name) +my-vterm-tab-buffer-name)
        (progn
          (tab-bar-switch-to-recent-tab)
          (find-file filename))
      (find-file filename)))
  (add-to-list 'vterm-eval-cmds '("+my/smart-vterm-find-file" +my/smart-vterm-find-file))

  (defun vterm-project ()
  (interactive)
  (let* ((default-directory (or (project-root (project-current))
                               default-directory))
        (vterm-buffer-name (format "*vterm_%s*" default-directory)))
    (vterm))))

;;; Org Mode
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

(use-package org
  :elpaca nil
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

  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-k") #'+my-org/mark-done))


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

;; OrgDownload
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
   org-modern-todo nil
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

;;;; Notification for org todos
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
