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
;;     Update #: 136
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


(eval-when-compile
  (require 'init-custom))

(cond
 ((string-equal my-mini-buffer-completion "vertico")
  (use-package vertico
    :straight (:type git :host github :repo "minad/vertico" :branch "main")
    :init
    (vertico-mode)
    :config
    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (setq vertico-cycle t))
  )
 ((string-equal my-mini-buffer-completion "selectrum")
  (use-package selectrum
    :config
    (selectrum-mode +1)
    (defun selectrum-up-directory ()
      (interactive)
      (let ((directory (expand-file-name (minibuffer-contents-no-properties))))
        (delete-minibuffer-contents)
        (insert (string-trim-right directory "[^/]+/?"))))
    (define-key minibuffer-local-map (kbd "C-DEL") #'selectrum-up-directory)
    )
  ))

(use-package counsel
  :disabled
  :hook (after-init . counsel-mode)
  :bind (:map counsel-mode-map
              ([remap swiper] . counsel-grep-or-swiper)
              ([remap swiper-backward] . counsel-grep-or-swiper-backward)
              ([remap dired] . counsel-dired)
              ([remap set-variable] . counsel-set-variable)
              ([remap insert-char] . counsel-unicode-char)
              ([remap recentf-open-files] . counsel-recentf)

              ("C-x j"   . counsel-mark-ring)
              ("C-h F"   . counsel-faces)

              ("C-c B" . counsel-bookmarked-directory)
              ("C-c L" . counsel-load-library)
              ("C-c O" . counsel-find-file-extern)
              ("C-c P" . counsel-package)
              ("C-c R" . counsel-list-processes)
              ("C-c f" . counsel-find-library)
              ("C-c g" . counsel-grep)
              ("C-c h" . counsel-command-history)
              ("C-c i" . counsel-git)
              ("C-c j" . counsel-git-grep)
              ("C-c o" . counsel-outline)
              ("C-c r" . counsel-rg)
              ("C-c z" . counsel-fzf)

              ("C-c c B" . counsel-bookmarked-directory)
              ("C-c c F" . counsel-faces)
              ("C-c c L" . counsel-load-library)
              ("C-c c O" . counsel-find-file-extern)
              ("C-c c P" . counsel-package)
              ("C-c c R" . counsel-list-processes)
              ("C-c c a" . counsel-apropos)
              ("C-c c e" . counsel-colors-emacs)
              ("C-c c f" . counsel-find-library)
              ("C-c c g" . counsel-grep)
              ("C-c c h" . counsel-command-history)
              ("C-c c i" . counsel-git)
              ("C-c c j" . counsel-git-grep)
              ("C-c c l" . counsel-locate)
              ("C-c c m" . counsel-minibuffer-history)
              ("C-c c o" . counsel-outline)
              ("C-c c p" . counsel-pt)
              ("C-c c r" . counsel-rg)
              ("C-c c s" . counsel-ag)
              ("C-c c t" . counsel-load-theme)
              ("C-c c u" . counsel-unicode-char)
              ("C-c c w" . counsel-colors-web)
              ("C-c c v" . counsel-set-variable)
              ("C-c c z" . counsel-fzf)

              :map counsel-find-file-map
              ("C-w" . counsel-up-directory)
              )
  :init
  (setq enable-recursive-minibuffers t)
  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and *sys/mac* (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))

  )

(use-package orderless
  :custom (completion-styles '(orderless))
  :config
  (savehist-mode)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  )
;; (use-package embark)
(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package mini-frame
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters `((left . 0.5)
                                (top . ,(/ (frame-pixel-height) 2))
                                (background-mode 'dark)
                                (foreground-color . "#bbc2cf")
                                (background-color . "#242730")
                                (min-width . 80)
                                (min-height . ,(if (member this-command
                                                           '(swiper
                                                             swiper-backward swiper-all
                                                             swiper-isearch swiper-isearch-backward
                                                             counsel-grep-or-swiper counsel-grep-or-swiper-backward))
                                                   16
                                                 0))
                                (width . 0.8)
                                ))


  (mini-frame-advice-functions '(read-from-minibuffer
                                 read-string
                                 completing-read))
  :config
  (when (and (not noninteractive) (require 'mini-frame nil t)) ;batch 模式下miniframe 有问题
    (add-to-list 'mini-frame-ignore-functions 'y-or-n-p)
    (add-to-list 'mini-frame-ignore-functions 'yes-or-no-p)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-forward)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-backward)
    )

  )

(provide 'init-mini-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-selectrum.el ends here
