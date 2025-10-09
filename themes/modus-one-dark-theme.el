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

  (defconst modus-one-dark-palette-partial
    '(
;;; Basic values
      (bg-main     "#282C35")
      (fg-main     "#c0c4d2")
      (bg-dim      "#21252C")
      (fg-dim      "#5b6265")
      (bg-alt      "#1c2024")
      (fg-alt      "#9ca0a5")

      (bg-active   "#3f4445")
      (bg-inactive "#1c1f20")

;;; Extended foreground colors (from mood-one)
      (gray         "#3f444a")  ; mood-one's gray
      (orange       "#da8548")  ; mood-one's orange
      (teal         "#4db5bd")  ; mood-one's teal
      (dark-blue    "#2257a0")  ; mood-one's dark-blue
      (pink         "#d9aeee")  ; mood-one's pink
      (violet       "#a9a1e1")  ; mood-one's violet
      (dark-cyan    "#5699af")  ; mood-one's dark-cyan

;;; Basic hues for foreground values
      (red             "#ff6c6b")
      (red-warmer      "#ff5f5f")
      (red-cooler      "#e47980")
      (red-faint       "#f3a0a0")
      (green           "#98be65")
      (green-warmer    "#5faf5f")
      (green-cooler    "#3fc489")
      (green-faint     "#a9c99f")
      (yellow          "#ecbe7b")
      (yellow-warmer   "#d6a86c")
      (yellow-cooler   "#e0b87a")
      (yellow-faint    "#c7b09a")
      (blue            "#51afef")
      (blue-warmer     "#5fafff")
      (blue-cooler     "#12b4ff")
      (blue-faint      "#a0a0cf")
      (magenta         "#c678dd")
      (magenta-warmer  "#d9aeee")  ; alias for pink
      (magenta-cooler  "#a9a1e1")  ; alias for violet
      (magenta-faint   "#d0b0ff")
      (cyan            "#46d9ff")
      (cyan-warmer     "#00d7ff")
      (cyan-cooler     "#65c5a8")
      (cyan-faint      "#99bfcf")

;;; Basic hues for background values
      (bg-red-intense     "#a02f50")
      (bg-green-intense   "#30682f")
      (bg-yellow-intense  "#8a5e4f")
      (bg-blue-intense    "#2257a0")
      (bg-magenta-intense "#6d3f8f")
      (bg-cyan-intense    "#0280b9")

      (bg-red-subtle      "#6f202a")
      (bg-green-subtle    "#2a532f")
      (bg-yellow-subtle   "#5d4a2a")
      (bg-blue-subtle     "#3a3e73")
      (bg-magenta-subtle  "#4a2d5a")
      (bg-cyan-subtle     "#334d69")

;;; Diffs
      (bg-added          "#304a4f")
      (bg-added-faint    "#3B564C")
      (bg-added-refine   green-warmer)
      (fg-added          "#a0d0f0")

      (bg-changed        "#4a4a2a")
      (bg-changed-faint  "#40483B")
      (bg-changed-refine yellow-cooler)
      (fg-changed        "#d0c080")

      (bg-removed        "#5a3142")
      (bg-removed-faint  "#522330")
      (bg-removed-refine red-cooler)
      (fg-removed        "#f0bfcf")

;;; Graphs
      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#0fed00")
      (bg-graph-green-1   "#007800")
      (bg-graph-yellow-0  "#e0c00a")
      (bg-graph-yellow-1  "#a07d40")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#bf94fe")
      (bg-graph-magenta-1 "#5f509f")
      (bg-graph-cyan-0    "#47dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special hues
      (bg-mode-line-active       "#1c2024")
      (fg-mode-line-active       "#c0c4d2")
      (bg-completion      "#3f4445")
      (bg-hover           "#47586E")
      (bg-hover-secondary "#665f7a")
      (bg-hl-line         "#212428")
      (bg-paren           "#47586E")
      (bg-err             "#501a2d")
      (bg-warning         "#4a352a")
      (bg-info            "#0f3f4f")

      (border        "#3f4445")
      (cursor        "#51afef")
      (fg-intense    magenta)

      (modeline-err     red)
      (modeline-warning yellow)
      (modeline-info    green)

      (underline-err     "#ff6c6b")
      (underline-warning "#ecbe7b")
      (underline-info    "#46d9ff")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#6f6600")

;;; Mappings

;;;; General mappings
      (bg-fringe unspecified)
      (fg-fringe unspecified)

      (bg-region "#3f4445")
      (fg-region unspecified)

      (err red)
      (warning yellow)
      (info green)

      (link blue)
      (link-alt cyan)
      (name magenta)
      (keybind blue)
      (identifier yellow)
      (prompt magenta)

;;;; Code mappings
      (builtin magenta)
      (comment "#5b6265")
      (property blue)
      (constant blue)
      (fnname violet)
      (keyword blue)
      (preprocessor cyan)
      (docstring "#5b6265")
      (string green)
      (type yellow)
      (variable pink)  ; using mood-one's violet
      (rx-escape cyan)
      (rx-construct red)

;;;; Accent mappings
      (accent-0 magenta)
      (accent-1 blue)
      (accent-2 green)
      (accent-3 yellow)

;;;; Date mappings
      (date-common magenta-faint)
      (date-deadline red)
      (date-deadline-subtle red-faint)
      (date-event fg-alt)
      (date-holiday red-warmer)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-scheduled-subtle yellow-faint)
      (date-weekday magenta)
      (date-weekend blue-faint)

;;;; Prose mappings
      (prose-code blue)
      (prose-done green)
      (prose-macro green-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-table-formula info)
      (prose-tag yellow-faint)
      (prose-todo orange)  ; using mood-one's orange
      (prose-verbatim pink) ; using mood-one's pink

;;;; Mail mappings
      (mail-cite-0 yellow)
      (mail-cite-1 red)
      (mail-cite-2 cyan-faint)
      (mail-cite-3 green-faint)
      (mail-part red-faint)
      (mail-recipient cyan-faint)
      (mail-subject pink)  ; using mood-one's pink
      (mail-other magenta-faint)

;;;; Search mappings
      (bg-search-match bg-yellow-intense)
      (bg-search-current bg-yellow-subtle)
      (bg-search-lazy bg-blue-intense)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 bg-magenta-intense)
      (bg-search-rx-group-1 bg-green-intense)
      (bg-search-rx-group-2 bg-red-subtle)
      (bg-search-rx-group-3 bg-cyan-subtle)

;;;; Space mappings
      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings
      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Terminal mappings
      (bg-term-black           "black")
      (fg-term-black           "black")
      (bg-term-black-bright    "gray35")
      (fg-term-black-bright    "gray35")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-cooler)
      (fg-term-green-bright    green-cooler)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-warmer)
      (fg-term-yellow-bright   yellow-warmer)

      (bg-term-blue            blue)
      (fg-term-blue            blue)
      (bg-term-blue-bright     blue-cooler)
      (fg-term-blue-bright     blue-cooler)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "gray65")
      (fg-term-white           "gray65")
      (bg-term-white-bright    "white")
      (fg-term-white-bright    "white")

;;;; Rainbow mappings
      (rainbow-0 blue)
      (rainbow-1 magenta)
      (rainbow-2 green)
      (rainbow-3 orange)
      (rainbow-4 yellow)
      (rainbow-5 red)
      (rainbow-6 magenta-cooler)
      (rainbow-7 green-cooler)
      (rainbow-8 yellow-cooler))
    "The complete `ef-one-dark' palette with all EF themes variables.")

  (defcustom modus-one-dark-palette-overrides nil
    "Overrides for `modus-one-dark-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Ef themes,
refer to `ef-themes-common-palette-overrides'.

To preview the palette entries, use `ef-themes-preview-colors' or
`ef-themes-preview-colors-current' (read the documentation for
further details)."
    ;; :group 'modus-themes
    ;; :package-version '(modus-themes . "1.0.0")
    :type '(repeat (list symbol (choice symbol string)))
    :link '(info-link "(modus-themes) Palette overrides"))


  (defconst ef-themes-palette-common
    '((fringe unspecified)
      (fg-region unspecified)

      (bg-diff-context bg-dim)

      (bg-tab-bar bg-alt)
      (bg-tab-current bg-main)
      (bg-tab-other bg-active)

      (fg-link-symbolic fg-alt)
      (underline-link border)
      (underline-link-symbolic border)

      (border-mode-line-active unspecified)
      (bg-mode-line-inactive bg-alt)
      (fg-mode-line-inactive fg-dim)
      (border-mode-line-inactive unspecified)

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

      (fg-completion-match-0 accent-0)
      (fg-completion-match-1 accent-1)
      (fg-completion-match-2 accent-2)
      (fg-completion-match-3 accent-3)

      (fg-heading-0 rainbow-0)
      (fg-heading-1 rainbow-1)
      (fg-heading-2 rainbow-2)
      (fg-heading-3 rainbow-3)
      (fg-heading-4 rainbow-4)
      (fg-heading-5 rainbow-5)
      (fg-heading-6 rainbow-6)
      (fg-heading-7 rainbow-7)
      (fg-heading-8 rainbow-8))
    "Common palette mappings for the Ef themes.")

  (defconst modus-one-dark-palette
    (append ef-themes-palette-common modus-one-dark-palette-partial modus-themes-common-palette-mappings))

  (modus-themes-theme
    modus-one-dark
    modus-themes
    "Custom `One-Dark' theme combining modus themes and Doom One styles."
    dark
    modus-vivendi-palette
    modus-one-dark-palette
    modus-one-dark-palette-overrides))

;;; modus-one-dark-theme.el ends here
