
;;; Code:
(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :demand t
  :init
  (setq evil-want-keybinding nil)
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
        blink-cursor-mode 'nil
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window)

  :init
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (setcdr evil-insert-state-map nil)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (dolist (mode '(snails-mode company-mode color-rg-mode hl-todo-mode smerge-mode))
    (add-to-list 'evil-emacs-state-modes mode)
    )

  (evil-declare-change-repeat 'company-complete)
  (unless noninteractive
    (setq save-silently t))
  )

;;
;;; Packages


(use-package general
  :ensure t
  :config
  (general-create-definer leader-def
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-,"
    )
  )

(use-package evil-magit
  :if (executable-find "git"))

(use-package evil-easymotion
  :ensure t
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))


(use-package evil-embrace
  :ensure t
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook ((ruby-mode enh-ruby-mode) . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((c++-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil-embrace-angle-bracket-modes-hook-h)
  :config
  (setq evil-embrace-show-help-p nil)


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


(use-package evil-escape
  :ensure t
  :hook (after-init . evil-escape-mode)
  :commands evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))


(use-package evil-exchange
  :ensure t
  :commands evil-exchange)


(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))


(use-package evil-snipe
  :ensure t
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
  :ensure t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


(use-package evil-traces
  :ensure t
  :after evil-ex
  :config
  (evil-traces-mode))


;; Allows you to use the selection for * and #
(use-package evil-visualstar
  :ensure t
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
                    "*" #'evil-visualstar/begin-search-forward
                    "#" #'evil-visualstar/begin-search-backward))


;;
;;; Text object plugins

(use-package exato
  :ensure t
  :commands evil-outer-xml-attr evil-inner-xml-attr)

(use-package evil-collection
  :ensure t
  :hook (evil-mode . evil-collection-init)
  :config
  ;; Disable `evil-collection' in certain modes
  (dolist (ig-mode '())
    (setq evil-collection-mode-list (remove ig-mode evil-collection-mode-list)))

  ;; Keybindings tweaks
  (evil-collection-define-key 'normal 'occur-mode-map
                              ;; consistent with ivy
                              (kbd "C-c C-e") 'occur-edit-mode)
  :custom
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-company-use-tng t)
  (evil-collection-outline-bind-tab-p t)
  (evil-collection-term-sync-state-and-mode-p nil)
  (evil-collection-setup-minibuffer nil)
  (evil-collection-setup-debugger-keys nil))


(provide 'init-evil)
;;; init-evil.el ends here
