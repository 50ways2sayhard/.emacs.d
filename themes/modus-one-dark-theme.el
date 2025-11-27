;;; ef-one-dark-theme.el --- A dark theme combining EF and Doom One styles -*- lexical-binding: t -*-

;; Copyright (C) 2023 Your Name
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Your Name <your@email.com>
;; URL: https://github.com/your-repo/ef-one-dark-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; A dark theme combining the EF themes' approach with Doom One's color scheme.

;;; Code:


(eval-and-compile
  (require 'modus-themes)

  (defvar modus-one-dark-palette
    (modus-themes-generate-palette
     ;; We provide the two base colors of Solarized, plus most of its
     ;; accents.  These form the BASE-COLORS we pass as an argument.
     ;; All other color values come from those.  The BASE-COLORS here
     ;; are enough to generate a new palatte that has no traces of, say,
     ;; the `modus-vivendi' color values.
     '((bg-main "#282C34")
       (bg-dim "#21242b")
       ;; (fg-main "#ABB2BF")
       (fg-main "#bbc2cf")
       (fg-dim "#5b6268")
       (red "#E06C75")
       (green "#98C379")
       (yellow "#E5C07B")
       (blue "#61AFEF")
       (magenta "#C678DD")
       (cyan "#56B6C2")
       (orange "#da8548")
       (grey "#3f444a"))
     ;; The COOL-OR-WARM-PREFERENCE is derived internally based on
     ;; `bg-main'.  We can pass it here if we feel strongly about it.
     nil
     ;; If we need to specify the CORE-PALETTE from where to inherit any
     ;; missing colors and/or semantic mappings, we can give it here.
     ;; Though nil is the appropriate starting point, as the code will
     ;; handle things internally.
     nil
     ;; And here are our MAPPINGS where we can specify what values apply
     ;; to which semantic color.  The `modus-themes-list-colors' shows
     ;; them all.
     ;;
     ;; Note that in our BASE-COLORS above we never wrote what, say,
     ;; `magenta-warmer' is: it is derived programmatically from the
     ;; `magenta' we have there.  Absent that, it would be taken from
     ;; the CORE-PALETTE.
     '((cursor blue)
       (fg-region unspecified)
       (fringe unspecified)
       (bg-hover grey)
       (bg-diff-context bg-dim)
       (bg-hover-secondary grey)
       (bg-hl-line bg-dim)
       (bg-paren-match fg-dim)
       (fg-prompt blue)
       (bg-completion fg-dim)
       (bg-mark-select bg-dim)
       (fg-mark-select fg-dim)

       (bg-tab-bar bg-alt)
       (bg-tab-current bg-main)
       (bg-tab-other bg-active)

       (bg-line-number-active unspecified)
       (fg-line-number-active accent-0)
       (bg-line-number-inactive unspecified)

       (bg-prominent-err bg-err)
       (bg-prominent-warning bg-warning)
       (bg-prominent-note bg-info)
       (fg-prominent-err err)
       (fg-prominent-warning warning)
       (fg-prominent-note info)

       (bg-space unspecified)
       (fg-space border)

       (bg-active-argument bg-warning)
       (fg-active-argument warning)
       (bg-active-value bg-info)
       (fg-active-value info)

       (bg-mark-delete bg-err)
       (fg-mark-delete err)
       (bg-mark-select bg-info)
       (fg-mark-select info)
       (bg-mark-other bg-warning)
       (fg-mark-other warning)

       (fg-search-current fg-main)
       (fg-search-lazy fg-main)
       (fg-search-static fg-main)
       (fg-search-replace fg-main)

       (fg-search-rx-group-0 fg-main)
       (fg-search-rx-group-1 fg-main)
       (fg-search-rx-group-2 fg-main)
       (fg-search-rx-group-3 fg-main)

       (fg-completion-match-0 accent-0)
       (fg-completion-match-1 accent-1)
       (fg-completion-match-2 accent-2)
       (fg-completion-match-3 accent-3)

       (accent-0 magenta)
       (accent-1 blue)
       (accent-2 green)
       (accent-3 yellow)

       (fg-link blue)

       ;; Code mappings
       (builtin magenta)
       (keyword magenta)
       (type yellow)
       (variable fg-main)
       (variable-use fg-main)
       (string green)
       (docstring green)
       (comment fg-dim)
       (constant orange)
       (property orange)
       (fnname blue)
       (fnname-call blue)

       (underline-note blue)
       (underline-err red)
       (underline-warning yellow)

       (info blue)

       (bg-changed-fringe yellow)
       (bg-removed-fringe red)
       (bg-added-fringe green)
       (fg-added green)
       (bg-added green-cooler)
       (fg-removed red)
       (bg-removed red-cooler)
       (fg-changed yellow)
       (bg-changed yellow-cooler)

       (keybind blue)

       ;; Date
       (date-common green)
       (date-weekday blue)

       (prose-table green)
       )
     ))

  (modus-themes-theme
   'modus-one-dark
   'modus-one-themes
   "Sample of a basic Solarized dark port."
   'dark
   'modus-one-dark-palette
   nil
   nil)
  )

;;; modus-one-dark-theme.el ends here
