;; -*- lexical-binding: t -*-

;;; init-selectrum.el ---
;;
;; Filename: init-selectrum.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Sun May  2 14:40:03 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 726
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

;; Completion styles
;; (setq completion-styles '(basic partial-completion substring initials flex))
(defvar +my-completion-styles '(basic partial-completion substring initials flex))
(setq completion-styles +my-completion-styles)

(autoload 'ffap-file-at-point "ffap")

(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn
                             (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table)))) 'append)

(defun +complete-fido-enter-dir ()
  (interactive)
  (let ((candidate (vertico--candidate))
        (current-input (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond
     ((and (vertico-directory--completing-file-p)
           (string= (car (last (s-split "/" current-input))) ".."))
      (progn
        (vertico-directory-delete-word 1)))

     ((and (vertico-directory--completing-file-p)
           (file-directory-p candidate)
           (not (string= candidate "~/")))
      (vertico-insert))

     (t (insert "/")))))

(use-package vertico
  :straight (vertico :includes (vertico-quick vertico-repeat vertico-directory)
                     :files (:defaults "extensions/vertico-*.el"))
  :hook (+self/first-input . vertico-mode)
  :bind
  (:map vertico-map
        ("C-<return>" . open-in-external-app))
  :custom
  (vertico-cycle nil)
  :init
  ;; (vertico-mode)
  :config
  (set-face-background 'vertico-current "#42444a")

  ;; Configure directory extension.
  (use-package vertico-quick
    :after vertico
    :ensure nil
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))
  (use-package vertico-repeat
    :after vertico
    :ensure nil
    :config
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (with-eval-after-load 'general
      (general-def "C-c r" 'vertico-repeat)
      ))
  (use-package vertico-directory
    :after vertico
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)
                ("C-w" . vertico-directory-up)
                ("/" . +complete-fido-enter-dir))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    )
  )

;; A few more useful configurations...
(use-package emacs
  :init
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
  :straight (:host github :repo "minad/consult")
  :commands (+consult-ripgrep-at-point noct-consult-ripgrep-or-line consult-line-symbol-at-point)
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
  :init
  (use-package consult-project-extra
    :after consult
    :straight (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra")
    :config
    ;; WORKAROUND
    (setq consult-project-buffer-sources consult-project-extra-sources)
    )
  (use-package consult-flycheck
    :after (consult flycheck))
  (use-package consult-dir
    :ensure t
    :after consult
    :bind (("C-x C-d" . consult-dir)
           :map selectrum-minibuffer-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))

  (use-package consult-lsp
    :after (consult lsp))
  (use-package consult-eglot
    :after (consult eglot))

  :config
  (setq consult-preview-key "M-p")
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-args "fd --color=never --full-path ARG OPTS")

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)


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
      (setq evil-ex-search-direction 'forward)))

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
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler))


(use-package orderless
  :after marginalia
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  (savehist-mode)
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

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
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
  :hook (+self/first-input . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))

(use-package mini-frame
  ;; :if *sys/mac*
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-detach-on-hide nil)
  (resize-mini-frames t)
  :config
  (setq mini-frame-show-parameters `((left . 0.5)
                                     (top . ,(/ (frame-pixel-height) 2))
                                     (background-mode 'dark)
                                     (foreground-color . "#bbc2cf")
                                     (background-color . "#242730")
                                     ;; (internal-border-width . 1)
                                     ;; (child-frame-border-width . 1)
                                     (min-width . 80)
                                     (width . 0.618)
                                     (no-accept-focus . t)))
  (when (and (not noninteractive) (require 'mini-frame nil t)) ;batch 模式下miniframe 有问题
    (add-to-list 'mini-frame-ignore-functions 'y-or-n-p)
    (add-to-list 'mini-frame-ignore-functions 'yes-or-no-p)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex)
    (add-to-list 'mini-frame-ignore-commands 'org-time-stamp)
    (add-to-list 'mini-frame-ignore-commands 'org-deadline)
    (add-to-list 'mini-frame-ignore-commands 'org-schedule)
    (add-to-list 'mini-frame-ignore-commands 'pp-eval-expression)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-forward)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-backward))
  )

(use-package all-the-icons-completion
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(provide 'init-mini-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-selectrum.el ends here
