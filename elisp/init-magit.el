;;; init-magit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-magit.el
;; Description: Initialize Magit
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 08:40:27 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Jan 12 14:04:56 2022 (+0800)
;;           By: John
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d magit
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes magit
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

(eval-when-compile
  (require 'init-const))

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

;; MagitPac
(use-package magit
  :defer t
  :if *git*
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'+magit-display-buffer-fn)
  (magit-auto-revert-mode -1)
  (setq magit-diff-refine-hunk (quote all))
  )
;; -MagitPac

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

(use-package smerge-mode
  :after magit
  :straight nil
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
                                      (hydra-smerge/body))))
         )
   :pretty-hydra
   ((:title "Smerge"
    :color pink :quit-key "q")
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :config
  ;; (local-leader-def
  ;;   :keymaps 'smerge-mode-map
  ;;   "n" '(smerge-next :wk "Next conflict")
  ;;   "p" '(smerge-prev :wk "Previous conflict")
  ;;   "RET" '(smerge-keep-current :wk "Accept current")
  ;;   "l" '(smerge-keep-lower :wk "Keep lower")
  ;;   "u" '(smerge-keep-upper :wk "Keep upper")
  ;;   "m" '(smerge-keep-mine :wk "Keep mine")
  ;;   "A" '(smerge-keep-all :wk "Keep all"))
  ;; (evil-define-key 'normal 'smerge-mode-map ",n" 'smerge-next)
  ;; (evil-define-key 'normal 'smerge-mode-map ",p" 'smerge-prev)
  ;; (evil-define-key 'normal 'smerge-mode-map ",," 'smerge-keep-current)
  ;; (evil-define-key 'normal 'smerge-mode-map ",l" 'smerge-keep-lower)
  ;; (evil-define-key 'normal 'smerge-mode-map ",u" 'smerge-keep-upper)
  ;; (evil-define-key 'normal 'smerge-mode-map ",m" 'smerge-mine)
  ;; (evil-define-key 'normal 'smerge-mode-map ",A" 'smerge-keep-all)
  ;; (evil-define-key 'normal 'smerge-mode-map ",c" 'smerge-keep-current)
  )

(provide 'init-magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-magit.el ends here
