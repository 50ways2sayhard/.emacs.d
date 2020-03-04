(eval-when-compile
  (require 'init-const))

;; evil mode
(evil-define-key 'normal 'global
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  ;; Comment
  "gcc" 'evilnc-comment-or-uncomment-lines
  "gcC" 'evilnc-comment-or-uncomment-to-the-line
  "gcr" 'comment-or-uncomment-region

  ;; goto
  "gd" 'xref-find-definitions
  )

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

;; Company
(with-eval-after-load 'company
  (general-define-key
   :states 'insert
   :keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "<tab>" 'company-complete-common-or-cycle
   )

  )


;; Leader def
(leader-def
  :keymaps 'override
  "<SPC>" '(counsel-projectile :wk "Project Find File")
  ":" '(execute-extended-command :which-key "M-x")
  "/" '(evilnc-comment-or-uncomment-lines :wk "Comment line")
  "\\" '(evilnc-comment-or-uncomment-to-the-line :wk "Comment to line")

  "b" '(:wk "Buffer")
  "b[" '(previous-buffer :wk "Previous buffer")
  "b]" '(next-buffer :wk "Next buffer")
  "bb" '(ivy-switch-buffer :wk "Switch buffer")
  "bd" '(kill-current-buffer :wk "Kill buffer")

  "c" '(:wk "Code")
  "cD" '(lsp-ui-peek-find-references :wk "Jump to implementation")
  "cd" '(lsp-ui-peek-find-definitions :wk "Jump to definition")
  "cf" '(format-all-buffer :wk "Format buffer")
  "ci" '(lsp-organize-imports :wk "Organize import")
  "cr" '(lsp-rename :wk "LSP rename")
  "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
  "co" '(lsp-ui-imenu :wk "Outline")
  "cJ" '(lsp-ivy-global-workspace-symbol :wk "Jump to Symbol in workspace")

  "e" '(:wk "Error")
  "eb" '(flycheck-buffer :wk "Check current buffer")
  ;; "el" '(flycheck-list-errors :wk "List errors")
  "el" '(+flycheck-list-errors :wk "List errors")
  "en" '(flycheck-next-error :wk "Next error")
  "ep" '(flycheck-previous-error :wk "Previous error")
  "ee" '(flycheck-explain-error-at-point :wk "Explain error at point")
  "ev" '(flycheck-verify-setup :wk "Verify setup")
  "es" '(flycheck-select-checker :wk "Select checker")

  "f" '(:wk "Files")
  "ff" '(find-file :wk "Find file")
  "fr" '(counsel-recentf :wk "Recent file")
  "fR" '(rename-file :wk "Rename file")
  "fl" '(locate-file :wk "Locate file")
  "fp" '(+open-configuration-folder :wk ".emacs.d")

  "g" '(:wh "Git")
  "gs" '(magit-status :wk "Git status")
  "gb" '(magit-branch-checkout :wk "Git checkout")
  "gB" '(magit-blame :wk "Git blame")
  "gm" '(gitmoji-picker :wk "Gitmoji")
  "gM" '((lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit))) :wk "Git stage and commit")
  "gf" '(magit-fetch :wk "Git fetch")
  "gF" '(magit-pull :wk "Git pull")

  "j" '(:wk "Jump")
  "jj" '(evil-avy-goto-char :wk "Jump to character")
  "jl" '(evil-avy-goto-line :wk "Jump to line")

  "m" '(:wk "Local leader")

  "o" '(:wk "Open")
  "op" '(treemacs :wk "Treemacs")
  "oy" '(my-youdao-search-at-point :wk "youdao")
  "oe" '(shell-here :wk "Shell")
  "og" '(google-this :wk "Google")
  "ot" '(org-todo-list :wk "Org todos")
  "ox" '(org-agenda :wk "Org agenda")

  "p" '(:wk "Project")
  "pp" '(projectile-switch-project :wk "Switch project")
  "pf" '(counsel-projectile :wk "Find file in project")
  "pr" '(projectile-recentf :wk "Recent file in project")
  "pt" '(magit-todos-list :wk "List project tasks")

  "q" '(:wk "Quit")
  "qq" '(kill-emacs :wk "Quit")
  "qr" '(restart-emacs :wk "Restart")


  "s" '(:wk "Search")
  "sb" '(swiper :wk "Search buffer")
  "sf" '(locate :wk "Locate file")
  "si" '(imenu :wk "Jump to symbol")
  "sp" '(counsel-projectile-rg :wk "Search project")
  "sT" '(load-theme :wk "Load theme")

  "w" '(:wk "Window")
  "wv" '(split-window-vertically :wk "Split window vertically")
  "wH" '(split-window-horizontally :wk "Split window horizontally")
  "wj" '(evil-window-down :wk "Focus window down")
  "wk" '(evil-window-up :wk "Focus window up")
  "wh" '(evil-window-left :wk "Focus window left")
  "wl" '(evil-window-right :wk "Focus window right")
  )

;; Python
(leader-def
  :states 'normal
  :keymaps 'python-mode-map
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
  )

(leader-def
  :states 'normal
  :keymaps '(js2-mode-map rjsx-mode-map)
  "mi" '(:wk "Imports")
  "mif" '(import-js-fix :wk "Fix imports")
  "mir" '(run-import-js :wk "Run import js")
  "mii" '(import-js-import :wk "Import module")
  )

(provide 'init-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bindings.el ends here
