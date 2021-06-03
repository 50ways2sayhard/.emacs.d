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
;;     Update #: 322
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


(use-package selectrum
  :config
  (global-set-key (kbd "C-c C-r") #'selectrum-repeat)
  (selectrum-mode +1)
  (use-package selectrum-prescient
    :init
    (setq selectrum-prescient-enable-filtering nil)
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))

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
  (defun selectrum-fido-backward-updir ()
    "Delete char before or go up directory, like `ido-mode'."
    (interactive)
    (if (and (eq (char-before) ?/)
             (eq (selectrum--get-meta 'category) 'file))
        (save-excursion
          (goto-char (1- (point)))
          (when (search-backward "/" (point-min) t)
            (delete-region (1+ (point)) (point-max))))
      (call-interactively 'backward-delete-char)))

  (defun selectrum-fido-delete-char ()
    "Delete char or maybe call `dired', like `ido-mode'."
    (interactive)
    (let ((end (point-max)))
      (if (or (< (point) end) (not (eq (selectrum--get-meta 'category) 'file)))
          (call-interactively 'delete-char)
        (dired (file-name-directory (minibuffer-contents)))
        (exit-minibuffer))))

  (defun selectrum-fido-enter-dir ()
    (interactive)
    (let ((candidate (selectrum-get-current-candidate)))
      (if (and (eq (selectrum--get-meta 'category) 'file)
               (file-directory-p candidate)
               (not (string-equal candidate "~/")))
          (selectrum-insert-current-candidate)
        (insert "/"))))

  (defun selectrum-fido-do-backward-updir ()
    (interactive)
    (if (and (eq (char-before) ?/)
             (eq (selectrum--get-meta 'category) 'file))
        (save-excursion
          (goto-char (1- (point)))
          (when (search-backward "/" (point-min) t)
            (delete-region (1+ (point)) (point-max))))))


  (define-key selectrum-minibuffer-map (kbd "DEL") 'selectrum-fido-backward-updir)
  (define-key selectrum-minibuffer-map (kbd "/") 'selectrum-fido-enter-dir)
  (define-key selectrum-minibuffer-map (kbd "C-d") 'selectrum-fido-delete-char)
  (define-key selectrum-minibuffer-map (kbd "C-w") 'selectrum-fido-do-backward-updir)
  )

(use-package consult
  :straight (:host github :repo "minad/consult")
  :after projectile
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
  (use-package consult-projectile
    :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-preview-key nil)
  (setq consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

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
    (if (or (not buffer-file-name)
            (buffer-narrowed-p)
            (ignore-errors
              (file-remote-p buffer-file-name))
            (jka-compr-get-compression-info buffer-file-name)
            (<= (buffer-size)
                (/ noct-consult-ripgrep-or-line-limit
                   (if (eq major-mode 'org-mode) 4 1))))
        (progn (consult-line)
               (my-consult-grep-set-evil-search-pattern))

      (when (file-writable-p buffer-file-name)
        (save-buffer))
      (let ((consult-ripgrep-command
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
        (my-consult-grep-set-evil-search-pattern))))

  ;; Configure initial narrowing per command
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t)))))

(use-package consult-lsp
  :straight (:host github :repo "gagbo/consult-lsp")
  :after lsp)

(use-package orderless
  :demand t
  :config
  (savehist-mode)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (defun dm/orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
        orderless-style-dispatchers '(dm/orderless-dispatch))
  )

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  :bind (:map minibuffer-local-completion-map
              ("M-A" . marginalia-cycle)
              ("C-i" . marginalia-cycle-annotators)))

(use-package mini-frame
  :if *sys/mac*
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :config
  (setq resize-mini-frames t)
  (setq mini-frame-show-parameters `((left . 0.5)
                                     (top . ,(/ (frame-pixel-height) 2))
                                     (background-mode 'dark)
                                     (foreground-color . "#bbc2cf")
                                     (background-color . "#242730")
                                     (min-width . 80)
                                     (width . 0.8)))
  (setq mini-frame-create-lazy nil)
  (when (and (not noninteractive) (require 'mini-frame nil t)) ;batch 模式下miniframe 有问题
    (add-to-list 'mini-frame-ignore-functions 'y-or-n-p)
    (add-to-list 'mini-frame-ignore-functions 'yes-or-no-p)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-forward)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-backward))
  )


(use-package affe
  :straight (:host github :repo "minad/affe")
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches
        affe-find-command "fd --color=never --full-path"))

(provide 'init-mini-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-selectrum.el ends here
