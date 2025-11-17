;;; doom-nightfox-theme.el --- Nightfox (Neovim) theme port for Doom Emacs -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: ChatGPT (Nightfox port)
;; URL: https://github.com/EdenEast/nightfox.nvim
;;
;;; Commentary:
;; A full, Doom Emacs–optimized port of the Nightfox colorscheme.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-nightfox-theme nil
  "Options for the `doom-nightfox' theme."
  :group 'doom-themes)

(defcustom doom-nightfox-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-nightfox-theme
  :type 'boolean)

(defcustom doom-nightfox-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-nightfox-theme
  :type 'boolean)

(defcustom doom-nightfox-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-nightfox-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-nightfox
  "A dark theme port of EdenEast's Nightfox."

  ;; name        gui         256           16
  ((bg         '("#192330" "#1b212c"       "black"        ))
   (bg-alt     '("#131a24" "#111821"       "black"        ))
   (base0      '("#0f141d" "#0f141d"       "black"        ))
   (base1      '("#1f2d3d" "#1f2d3d"       "brightblack"  ))
   (base2      '("#22394f" "#22394f"       "brightblack"  ))
   (base3      '("#2a3b4d" "#2a3b4d"       "brightblack"  ))
   (base4      '("#33415e" "#33415e"       "brightblack"  ))
   (base5      '("#526175" "#526175"       "brightblack"  ))
   (base6      '("#6f7a8c" "#6f7a8c"       "brightblack"  ))
   (base7      '("#b4bec8" "#b4bec8"       "brightblack"  ))
   (base8      '("#cdcecf" "#cdcecf"       "white"        ))
   (fg         '("#cdcecf" "#cdcecf"       "brightwhite"  ))
   (fg-alt     '("#b4bec8" "#b4bec8"       "white"        ))

   (red        '("#c94f6d" "#ff5f87"       "red"          ))
   (orange     '("#d98258" "#ff9f5f"       "brightred"    ))
   (yellow     '("#dbc074" "#ffd787"       "yellow"       ))
   (green      '("#81b29a" "#87d7af"       "green"        ))
   (teal       '("#63cdcf" "#5fd7d7"       "brightcyan"   ))
   (blue       '("#719cd6" "#5f87ff"       "brightblue"   ))
   (dark-blue  '("#4a6ba5" "#4a6ba5"       "blue"         ))
   (blue-alt   '("#86abdc" "#87afff"       "brightblue"   ))
   (magenta    '("#9d79d6" "#af87ff"       "magenta"      ))
   (violet     '("#baa0e8" "#d7afff"       "brightmagenta"))
   (cyan       '("#7adce6" "#87e0ff"       "brightcyan"   ))
   (dark-cyan  '("#548681" "#5fd7af"       "cyan"         ))
   (grey       base4)

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base3)
   (selection      base2)
   (builtin        blue)
   (comments       (if doom-nightfox-brighter-comments base6 base5))
   (doc-comments   (doom-lighten (if doom-nightfox-brighter-comments base6 base5) 0.15))
   (constants      magenta)
   (functions      blue)
   (keywords       magenta)
   (methods        teal)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      fg)
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-nightfox-brighter-modeline)
   (-modeline-pad
    (when doom-nightfox-padded-modeline
      (if (integerp doom-nightfox-padded-modeline) doom-nightfox-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base7)
   (modeline-bg
    (if -modeline-bright
        (doom-blend blue bg 0.45)
      (doom-darken bg 0.1)))
   (modeline-bg-alt
    (if -modeline-bright
        (doom-blend teal bg 0.45)
      (doom-darken bg 0.15)))
   (modeline-bg-inactive   (doom-darken bg 0.15))
   (modeline-bg-inactive-l (doom-darken bg 0.2)))


  ;;;; Base theme face overrides
  (((default &override) :background bg :foreground fg)
   ((cursor &override) :background blue)
   (fringe :background bg :foreground base5)
   (vertical-border :foreground base4)
   (hl-line :background (doom-lighten base1 0.12) :extend t)
   (lazy-highlight :background (doom-blend blue bg 0.3) :foreground fg :weight 'bold)
   (region :background base2 :extend t)
   (highlight :background base1 :foreground fg :distant-foreground bg :extend t)
   (shadow :foreground base4)
   (link :foreground cyan :underline t)
   (show-paren-match :background base3 :foreground yellow :weight 'bold)
   (minibuffer-prompt :foreground blue :weight 'bold)
   (tooltip :background base2 :foreground fg)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   ;;;; font-lock
   (font-lock-builtin-face        :foreground magenta)
   (font-lock-comment-face        :foreground comments :slant 'italic)
   (font-lock-comment-delimiter-face :foreground comments :slant 'italic)
   (font-lock-constant-face       :foreground cyan)
   (font-lock-function-name-face  :foreground blue)
   (font-lock-keyword-face        :foreground magenta :weight 'bold)
   (font-lock-string-face         :foreground green)
   (font-lock-type-face           :foreground yellow)
   (font-lock-variable-name-face  :foreground fg)
   (font-lock-warning-face        :foreground warning :weight 'bold)

   ;;;; mode-line
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)
   (doom-modeline-bar :background blue)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   (solaire-default-face :inherit 'default :background bg)

   ;;;; org-mode
   (org-level-1 :foreground blue :weight 'bold :height 1.2)
   (org-level-2 :foreground magenta :weight 'bold)
   (org-level-3 :foreground green :weight 'bold)
   (org-level-4 :foreground yellow)
   (org-block :background bg-alt :extend t)
   (org-code  :background bg-alt :foreground yellow :extend t)
   (org-verbatim :foreground cyan)
   (org-table :foreground teal)
   (org-link :foreground blue :underline t)
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground green :weight 'bold)
   (org-ellipsis :underline nil :foreground yellow)

   ;;;; company
   (company-tooltip            :background bg-alt :foreground fg)
   (company-tooltip-selection  :background base2 :foreground fg)
   (company-tooltip-common     :foreground blue :weight 'bold)
   (company-scrollbar-bg       :background bg-alt)
   (company-scrollbar-fg       :background base1)
   (company-tooltip-search     :background base2 :foreground blue)

   ;;;; completion/vertico/consult
   (vertico-current            :background base2 :foreground fg :extend t)
   (completions-common-part    :foreground blue :weight 'bold)
   (consult-line-number        :foreground base5)
   (consult-file               :foreground fg)
   (completions-first-difference :foreground magenta)

   ;;;; ivy/helm (compat)
   (ivy-current-match :background base2 :extend t)
   (ivy-minibuffer-match-face-1 :foreground blue)
   (helm-selection :background base2 :extend t)

   ;;;; lsp/diagnostics
   (lsp-face-highlight-textual :background base1 :distant-foreground fg :extend t)
   (lsp-ui-sideline-code-action :foreground yellow)
   (lsp-ui-peek-header :background base0 :foreground blue)
   (lsp-ui-peek-footer :background base0)
   (lsp-ui-peek-list :background bg)
   (lsp-ui-peek-selection :background base2 :extend t)
   (lsp-ui-doc-background :background bg-alt)

   (error   :foreground error)
   (warning :foreground warning)
   (success :foreground success)
   (flycheck-error   :underline `(:style wave :color ,red))
   (flycheck-warning :underline `(:style wave :color ,yellow))
   (flycheck-info    :underline `(:style wave :color ,blue))

   ;;;; treemacs
   (treemacs-root-face           :foreground blue :weight 'bold :height 1.2)
   (treemacs-directory-face      :foreground blue)
   (treemacs-file-face           :foreground fg)
   (treemacs-git-modified-face   :foreground yellow)
   (treemacs-git-renamed-face    :foreground green)
   (treemacs-git-untracked-face  :foreground cyan)
   (treemacs-git-conflict-face   :foreground red)
   (treemacs-tags-face           :foreground magenta)
   (treemacs-hl-line-face        :background (doom-lighten base2 0.15) :extend t)

   ;;;; magit
   (magit-section-heading        :foreground blue :weight 'bold)
   (magit-branch-local           :foreground blue)
   (magit-branch-remote          :foreground green)
   (magit-diff-added             :foreground green :background (doom-blend green bg 0.1) :extend t)
   (magit-diff-added-highlight   :foreground green :background (doom-blend green bg 0.2) :weight 'bold :extend t)
   (magit-diff-removed           :foreground red :background (doom-blend red bg 0.1) :extend t)
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :weight 'bold :extend t)
   (magit-diff-context           :foreground base5 :background bg :extend t)
   (magit-diff-context-highlight :foreground fg :background base1 :extend t)
   (magit-diffstat-added         :foreground green)
   (magit-diffstat-removed       :foreground red)
   (magit-section-highlight      :background bg :extend t)

   ;;;; diff-hl
   (diff-hl-change :foreground blue :background blue)
   (diff-hl-insert :foreground green :background green)
   (diff-hl-delete :foreground red :background red)

   ;;;; dired
   (dired-directory :foreground blue :weight 'bold)
   (dired-header    :foreground magenta :weight 'bold)
   (dired-flagged   :foreground red)
   (dired-marked    :foreground yellow)

   ;;;; term/eshell
   (eshell-prompt        :foreground blue :weight 'bold)
   (eshell-ls-directory  :foreground blue)
   (eshell-ls-executable :foreground green)
   (eshell-ls-symlink    :foreground cyan)

   ;;;; which-key
   (which-key-key-face                  :foreground blue :weight 'bold)
   (which-key-group-description-face    :foreground magenta)
   (which-key-command-description-face  :foreground fg)
   (which-key-local-map-description-face :foreground cyan)

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground magenta)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground yellow)
   (rainbow-delimiters-depth-5-face :foreground cyan)

   ;;;; dashboard
   (dashboard-heading :foreground blue :weight 'bold)
   (dashboard-items-face :foreground fg))

  ;;;; Base theme variable overrides-
  ())

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'doom-nightfox)
;;; doom-nightfox-theme.el ends here
