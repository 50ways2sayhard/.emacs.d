(eval-when-compile
  (require 'init-const)
  (require 'init-func)
  )

(use-package general
  :ensure
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
    "gD" 'xref-find-references

    "/" 'consult-line-symbol-at-point
    "'" 'noct-consult-ripgrep-or-line
    )

  (evil-ex-define-cmd "W" 'evil-write)
  (general-def "<escape>" 'keyboard-quit)
  (general-def "C-;" 'embrace-commander)
  (general-def [C-return] '+default/newline-below)
  (general-def "C-RET" '+default/newline-below)
  (general-def [C-S-return] '+default/newline-above)
  (general-def "C-S-RET" '+default/newline-above)
  (general-def "M-?" '+consult-ripgrep-at-point)

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
    "<SPC>" '(consult-projectile :wk "Project Find File")
    ":" '(execute-extended-command :which-key "M-x")
    "/" '(consult-ripgrep :wk "Search in project")
    "\\" '(evilnc-comment-or-uncomment-to-the-line :wk "Comment to line")
    "." '(noct-consult-ripgrep-or-line :wk "Swiper")
    "`" '(selectrum-repeat :wk "Repeat last search")
    "[" '(previous-buffer :wk "Previous buffer")
    "]" '(next-buffer :wk "Next buffer")

    "b" '(:wk "Buffer")
    "b[" '(previous-buffer :wk "Previous buffer")
    "b]" '(next-buffer :wk "Next buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bd" '(kill-current-buffer :wk "Kill buffer")

    "c" '(:wk "Code")
    "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "cf" '(format-all-buffer :wk "Format buffer")
    "cD" '(xref-find-references :wk "Jump to implementation")
    "cd" '(lsp-ui-peek-find-definitions :wk "Jump to definition")
    "ci" '(lsp-organize-imports :wk "Organize import")
    "cr" '(lsp-rename :wk "LSP rename")
    "co" '(lsp-ui-imenu :wk "Outline")
    "cJ" '(consult-lsp-symbols :wk "Jump to Symbol in workspace")
    "cp" '(citre-peek :wk "Citre peek")
    "ca" '(citre-ace-peek :wk "Citre ace peek")
    "cj" '(citre-jump+ :wk "Citre jump")
    "ck" '(citre-jump-back :wk "Citre jump back")
    "cu" '(citre-update-this-tags-file :wk "Citre update this tag")
    "cU" '(citre-update-tags-file :wk "Citre update tag")
    "cc" '(separedit :wk "Write comment")
    "ch" '(lsp-ui-doc-show :wk "Toggle lsp-ui-doc")

    "e" '(:wk "Error")
    "eb" '(flycheck-buffer :wk "Check current buffer")
    "el" '(+flycheck-list-errors :wk "List errors")
    "ef" '(consult-flycheck :wk "Find error")
    "en" '(flycheck-next-error :wk "Next error")
    "ep" '(flycheck-previous-error :wk "Previous error")
    "ee" '(flycheck-explain-error-at-point :wk "Explain error at point")
    "ev" '(flycheck-verify-setup :wk "Verify setup")
    "es" '(flycheck-select-checker :wk "Select checker")

    "f" '(:wk "Files")
    "ff" '(find-file :wk "Find file")
    "fr" '(recentf-open-files :wk "Recent file")
    "fR" '(+my-rename-file :wk "Rename file")
    "fp" '(+open-configuration-folder :wk ".emacs.d")
    "fD" '(+my-delete-file :wk "Delete file")
    "f<SPC>" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "fo" '((lambda() (interactive)(find-file +org-capture-file-gtd)) :which-key "Org files")

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
    "jJ" '(evil-avy-goto-char-2 :wk "Jump to character 2")

    "m" '(:wk "Local leader")

    "o" '(:wk "Open")
    "oy" '(my-youdao-search-at-point :wk "youdao")
    "oe" '(shell-here :wk "Shell")
    "ot" '(org-todo-list :wk "Org todos")
    "ox" '(org-agenda :wk "Org agenda")

    "p" '(:wk "Project")
    "pk" '(projectile-kill-buffers :wk "Kill project buffers" )
    "pp" '(projectile-switch-project :wk "Switch project")
    "pf" '(consult-projectile :wk "Find file in project")
    "pt" '(magit-todos-list :wk "List project tasks")
    "pS" '(projectile-save-project-buffers :wk "Save project buffers")

    "q" '(:wk "Quit")
    "qq" '(kill-emacs :wk "Quit")
    "qr" '(restart-emacs :wk "Restart")

    "s" '(:wk "Search")
    "sa" '(consult-org-agenda :wk "Search agenda")
    "sd" '(+devdocs-lookup-at-point :wk "Devdocs lookup")
    "sD" '(+devdocs-search-at-point :wk "Devdocs search")
    "sf" '(locate :wk "Locate file")
    "si" '(+my-imenu :wk "Jump to symbol")
    "sI" '(consult-project-imenu :wk "Jump to symbol all buffer")
    "sp" '(consult-ripgrep :wk "Search project")
    "sP" '(color-rg-search-project :wk "Color-rg Search project")
    "sy" '(color-rg-search-symbol-in-project :wk "Color-rg Search symbol")
    "sT" '(load-theme :wk "Load theme")

    "t" '(:wk "Toggle")
    "tc" '(calendar :wk "Toggle calendar")
    "te" '(vterm-posframe-toggle :wk "Shell")
    "tl" '(toggle-truncate-lines :wk "Toggle line wrap")
    "td" '(toggle-debug-on-error :wk "Toggle debug on error")
    "tt" '(+treemacs/toggle :wk "Treemacs")
    "ti" '(imenu-list-smart-toggle :wk "imenu-list")
    "tc" '(olivetti-mode :wk "Center")

    "w" '(:wk "Window")
    "wv" '(split-window-vertically :wk "Split window vertically")
    "wH" '(split-window-horizontally :wk "Split window horizontally")
    "wj" '(evil-window-down :wk "Focus window down")
    "wk" '(evil-window-up :wk "Focus window up")
    "wh" '(evil-window-left :wk "Focus window left")
    "wl" '(evil-window-right :wk "Focus window right")

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
    )
  )
(provide 'init-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bindings.el ends here
