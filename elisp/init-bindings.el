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
    ;; "K" 'lsp-ui-doc-glance
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
  (general-def "M->" '+lookup-xref-references-backend-fn)
  (general-def "M-y" 'consult-yank-from-kill-ring)

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
    "/" '(consult-ripgrep :wk "Search in project")
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
    "fr" '(recentf-open-files :wk "Recent file")
    "fR" '(+my-rename-file :wk "Rename file")
    "fp" '(+open-configuration-folder :wk ".emacs.d")
    "fD" '(+my-delete-file :wk "Delete file")
    "f<SPC>" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "fo" '((lambda() (interactive)(find-file +org-capture-file-gtd)) :which-key "Org files")

    "g" '(:wk "Git")
    "gs" '(magit-status :wk "Git status")
    "gbb" '(magit-branch-checkout :wk "Git checkout")
    "gB" '(magit-blame :wk "Git blame")
    "gm" '(gitmoji-picker :wk "Gitmoji")
    "gM" '((lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit))) :wk "Git stage and commit")
    "gf" '(magit-fetch :wk "Git fetch")
    "gF" '(magit-pull :wk "Git pull")
    "go" '(magit-open-repo :wk "Open repo")
    "gby" '(magit-add-current-buffer-to-kill-ring :wk "Copy current branch name")

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
    "sI" '(consult-project-imenu :wk "Jump to symbol all buffer")
    "sp" '(consult-ripgrep :wk "Search project")
    "sP" '(color-rg-search-project :wk "Color-rg Search project")
    "sy" '(color-rg-search-symbol-in-project :wk "Color-rg Search symbol")
    "sT" '(load-theme :wk "Load theme")
    "sc" '(:wk "In current file")
    "scs" '(color-rg-search-input-in-current-file :wk "Search input")
    "sci" '(color-rg-search-symbol-in-current-file :wk "Search symbol at point")

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
