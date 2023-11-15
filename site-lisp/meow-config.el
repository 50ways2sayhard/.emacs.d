;;; -*- lexical-binding: t -*-

;; TODO: g bindings
;; TODO: better undo/redo
;; TODO: mode specified bindings

(require 'meow)
(meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))
(meow-thing-register 'url 'url 'url)

(add-to-list 'meow-char-thing-table '(?a . angle))
(add-to-list 'meow-char-thing-table '(?u . url))

(defvar meow-two-char-escape-sequence "jk")
(defvar meow-two-char-escape-delay 0.5)
(defun meow--two-char-exit-insert-state (s)
  "Exit meow insert state when pressing consecutive two keys.

S is string of the two-key sequence."
  (when (meow-insert-mode-p)
    (cond ((derived-mode-p 'vterm-mode 'term-mode) (call-interactively 'term-send-raw))
          (t (let ((modified (buffer-modified-p))
                   (undo-list buffer-undo-list))
               (insert (elt s 0))
               (let* ((second-char (elt s 1))
                      (event
                       (if defining-kbd-macro
                           (read-event nil nil)
                         (read-event nil nil meow-two-char-escape-delay))))
                 (cond
                  ((null event) (ignore))
                  ((and (integerp event) (char-equal event second-char))
                   (backward-delete-char 1)
                   (set-buffer-modified-p modified)
                   (setq buffer-undo-list undo-list)
                   (push 'escape unread-command-events))
                  (t (push event unread-command-events)))))))))

(defun meow-two-char-exit-insert-state ()
  "Exit meow insert state when pressing consecutive two keys."
  (interactive)
  (meow--two-char-exit-insert-state meow-two-char-escape-sequence))
(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
            #'meow-two-char-exit-insert-state)

(with-no-warnings
  (defvar-keymap +meow-file-map
    :doc "File operations"
    "f" #'find-file
    "r" #'recentf-open-files
    "p" #'+open-configuration-folder
    "j" #'+consult-dir
    "h" #'(lambda () (interactive) (consult-fd default-directory)))
  (defvar-keymap +meow-buffer-map
    :doc "Buffer operations"
    "b" #'consult-buffer
    "d" #'kill-current-buffer
    "s" #'(lambda ()
            (interactive)
            (when (and (boundp 'eglot-managed-p) (eglot-managed-p))
              (call-interactively #'eglot-code-action-organize-imports))
            (call-interactively #'apheleia-format-buffer)
            (save-buffer)))
  (defvar-keymap +meow-code-map
    :doc "Code operations"
    "r" #'eglot-rename
    "a" #'eglot-code-actions
    "c" #'separedit
    "h" #'+eglot-help-at-point
    "f" #'apheleia-format-buffer
    "I" #'+eglot-organize-imports
    "i" #'consult-eglot-symbols)
  (defvar-keymap +meow-diagnostics-map
    :doc "Diagnostic operations"
    "b" #'flymake-start
    "l" #'consult-flymake
    "L" #'(lambda () (interactive) (consult-flymake t))
    "P" #'flymake-show-project-diagnostics
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error)
  (defvar-keymap +meow-jump-map
    :doc "Jump map"
    "j" #'avy-goto-char
    "l" #'avy-goto-line
    "J" #'avy-goto-char-2
    "e" #'consult-global-mark)
  (defvar-keymap +meow-vc-map
    :doc "VC map"
    "s" #'magit-status
    "B" #'magit-blame
    "u" #'aborn/simple-git-commit-push
    "y" #'magit-add-current-buffer-to-kill-ring
    "o" #'magit-open-repo
    "t" #'git-timemachine)
  (defvar-keymap +meow-open-map
    "t" #'org-todo-list
    "x" #'org-agenda
    "g" #'consult-org-agenda
    "o" #'(lambda () (interactive)(org-clock-out)(org-save-all-org-buffers))
    "d" #'consult-mark-done)
  (defvar-keymap +meow-project-map
    "p" #'project-switch-project
    "f" #'project-find-file
    "k" #'project-kill-buffers)
  (defvar-keymap +meow-quit-map
    "q" #'kill-emacs
    "r" #'restart-emacs)
  (defvar-keymap +meow-search-map
    "a" #'consult-org-agenda
    "d" #'+devdocs-dwim
    "D" #'+devdocs-search-at-point
    "h" #'+consult-ripgrep-current-directory
    "i" #'+my-imenu
    "I" #'consult-imenu-multi
    "p" #'consult-ripgrep
    "g" #'+my/google-it)
  (defvar-keymap +meow-toggle-map
    "e" #'+my/smart-switch-to-vterm-tab
    "t" #'dirvish-side
    "d" #'toggle-debug-on-error
    "l" #'toggle-truncate-lines
    "y" #'go-translate-at-point)
  (defvar-keymap +meow-window-map
    "u" #'winner-undo
    "o" #'winner-redo
    "d" #'kill-buffer-and-window
    "k" #'delete-other-windows)
  (defvar-keymap +meow-tab-map
    "c" #'tab-new
    "r" #'tab-bar-switch-to-recent-tab
    "d" #'tab-bar-close-tab
    "s" #'tab-bar-select-tab-by-name
    "t" #'+my/smart-switch-to-vterm-tab
    "1" #'(lambda () (interactive) (tab-bar-select-tab 1))
    "2" #'(lambda () (interactive) (tab-bar-select-tab 2))
    "3" #'(lambda () (interactive) (tab-bar-select-tab 3))
    "4" #'(lambda () (interactive) (tab-bar-select-tab 4))
    "5" #'(lambda () (interactive) (tab-bar-select-tab 5))))

(meow-leader-define-key
 `("f" . ,+meow-file-map)
 `("b" . ,+meow-buffer-map)
 `("c" . ,+meow-code-map)
 `("e" . ,+meow-diagnostics-map)
 `("j" . ,+meow-jump-map)
 `("g" . ,+meow-vc-map)
 `("o" . ,+meow-open-map)
 `("p" . ,+meow-project-map)
 `("q" . ,+meow-quit-map)
 `("s" . ,+meow-search-map)
 `("t" . ,+meow-toggle-map)
 `("w" . ,+meow-window-map)

 '("x" . org-capture)
 '("=" . er/expand-region)
 '("?" . consult-ripgrep)
 '("/" . noct-consult-ripgrep-or-line)
 '(":" . "M-x")
 '("SPC" . consult-project-extra-find)
 '(";" . +my/open-org-agenda)
 '("," . "C-x ,"))

(with-no-warnings
  (defvar-keymap +meow-g-map
    "d" #'xref-find-definitions
    "r" #'xref-find-references
    "c c" #'evilnc-comment-or-uncomment-lines
    "c Y" #'evilnc-copy-and-comment-lines))

(defun +meow-window-vsplit (&optional count file)
  "Split window right and move to the created window."
  (interactive "P<f>")
  (select-window
   (split-window (selected-window) (when count (- count)) 'right))
  (when (not count)
    (balance-windows (window-parent))))

(defun +meow-window-split (&optional count file)
  "Split window right and move to the created window."
  (interactive "P<f>")
  (select-window
   (split-window (selected-window) (when count (- count)) 'below))
  (when (not count)
    (balance-windows (window-parent))))

(add-hook 'meow-insert-exit-hook
          #'(lambda ()
              (corfu-quit)))

(defun meow-setup ()
  (interactive)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-use-keypad-when-execute-kbd t
        meow-expand-hint-remove-delay 5.0
        meow-use-clipboard t
        ;; meow-keypad-leader-dispatch "H-c"

        meow-keypad-ctrl-meta-prefix ?M
        meow-keypad-start-keys '((?C . ?c)
                                 (?h . ?h)
                                 (?X . ?x)))
  ;; I use C-w for windmove
  (setq meow--kbd-kill-region "s-x")
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("-" . previous-buffer)
   '("=" . next-buffer)
   '("<escape>" . ignore)
   '("," . "C-x ,"))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("," . "C-x ,")
   '("'" . meow-repeat)
   '(";" . meow-reverse)
   '("{" . meow-inner-of-thing)
   '("}" . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("-" . negative-argument)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("dd" . meow-delete)
   '("dD" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   `("g" . ,+meow-g-map)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . delete-window)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . undo)
   '("U" . undo-redo)
   '("N" . meow-undo)
   '("M" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("$" . end-of-line)
   '("RET" . ignore)
   '("<escape>" . ignore)
   '("C-w v" . +meow-window-vsplit)
   '("C-w s" . +meow-window-split)
   '("C-w 0" . balance-windows)
   '("C-w j" . windmove-down)
   '("C-w k" . windmove-up)
   '("C-w l" . windmove-right)
   '("C-w h" . windmove-left)
   '("C-i" . xref-go-back)
   '("C-o" . xref-go-forward)
   '("?" . consult-ripgrep)
   '("/" . noct-consult-ripgrep-or-line)
   `("C-s" . ,+meow-tab-map)
   '("%" . jump-out-of-pair)
   ))

(provide 'meow-config)
