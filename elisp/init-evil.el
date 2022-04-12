
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :demand t
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-want-keybinding 'nil
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
  (dolist (mode '(color-rg-mode smerge-mode vterm-mode))
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


  (require 'evil/+packages))
;;; Packages

(provide 'init-evil)
;;; init-evil.el ends here
