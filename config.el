;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Alex Garibay"
      user-mail-address "alex@alexgaribay.com")

(setq magit-margin-settings nil)

(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

(setq package-native-compile t)

;; Custom functions

(defun clipboard-copy-region (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "pbcopy"))

;;
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
(setq doom-font (font-spec :family "Fira Mono" :size 12.0 :dpi 100)
      doom-variable-pitch-font (font-spec :family "Fira Mono" :size 14.0 :dpi 130))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
;; (setq display-line-numbers-type 'relative)

;;
;; (setq magit-git-executable "git")

;; (after! ivy
;;   ;; (add-to-list 'ivy-extra-directories ".elixir_ls")
;;   ;; (add-to-list 'ivy-extra-directories ".git")
;;   ;; (add-to-list 'ivy-extra-directories "_build")
;;   ;; (add-to-list 'ivy-extra-directories "build")
;;   ;; (add-to-list 'ivy-extra-directories "deps")
;;   ;; (add-to-list 'ivy-extra-directories "node_modules")
;;     (setq counsel-find-file-ignore-regexp "\\.elixir_ls\\'")
;;     (setq counsel-find-file-ignore-regexp "\\.git\\'")
;;     (setq counsel-find-file-ignore-regexp "\\_build\\'")
;;     (setq counsel-find-file-ignore-regexp "\\deps\\'")
;;     (setq counsel-find-file-ignore-regexp "\\node_modules\\'")
;;     )

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;

;; ;; Treemacs line color remap
;; (defun my-remap-hl-line ()
;;   "Remap hl-line face."
;;   (face-remap-add-relative 'hl-line `(:background ,(face-background "#cccccc") :foreground ,(face-foreground 'lazy-highlight))))

;; (with-eval-after-load 'treemacs
;;   (add-hook 'treemacs-mode-hook #'my-remap-hl-line))

;; Custom functions
;;
(defun winum-assign-func ()
  (cond
   ((equal (buffer-name) "*Ediff Control Panel*") 4)
   ((string-match-p (buffer-name) ".*\\*Treemacs\\*.*") 0)
   ((string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 0)
   (t nil)))

(defun split-window-vertically-and-focus ()
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1))

(defun split-window-horizontally-and-focus ()
  (interactive)
  (evil-window-split)
  (evil-window-down 1))

;; Package Configuration

;; Disable line numbers in neotree
(add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))
(set-popup-rule! "^ ?\\*NeoTree" :ignore t)

;; Tramp
(require 'tramp)
(add-to-list 'tramp-remote-path "/usr/bin/")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
;; (setq tramp-verbose 1)
(setq vc-handled-backends '(Git))

;; find-file-in-project

(after! find-file-in-project
(add-to-list 'ffip-prune-patterns "*/_build")
(add-to-list 'ffip-prune-patterns "*/deps")
(add-to-list 'ffip-prune-patterns "*/.elixir_ls")
(setq ffip-use-rust-fd t))


;; Global word wrapping
(setq global-visual-line-mode t)

(after! doom-modeline
  (doom-modeline-mode 1)
  (setq doom-modeline-height 22)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-major-mode-icon nil)
  (custom-set-faces
    '(mode-line ((t (:family "Fira Code"))))
    '(mode-line-inactive ((t (:family "Fira Code")))))
  )

;; Window Numbering
;; (after! winum
;;   (winum-mode 1)
;;   (setq winum-auto-assign-0-to-minibuffer nil)
;;   (add-to-list 'winum-assign-functions #'winum-assign-func))

(after! neotree
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
)

(setq  doom-themes-neotree-file-icons t)
(with-eval-after-load 'doom-themes
  (doom-themes-neotree-config))

;;
;; Javascript

(setq js2-basic-offset 2)
(setq js-indent-level 2)

;; Web-mode
(setq web-mode-indent-style 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-block-padding 2)
(setq web-mode-part-padding 2)

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; Eglot

(use-package
 eglot
 :ensure nil
 :config
 (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/elixir-ls/release/language_server.sh"))
 (add-to-list 'eglot-server-programs '(dart-mode . ((concat (getenv "FLUTTER_ROOT") "/bin/cache/dart-sdk/bin/dart") "language-server")))
)

(use-package
 eldoc-box
 :ensure nil
 :config
 (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
)

;; Elixir

(use-package
 elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 :hook (elixir-ts-mode . exunit-mode)
 :config
 (map! :leader :desc "Format buffer" :prefix "c" "f" #'eglot-format-buffer)
 (map! :leader :desc "Format buffer" :prefix "mf" "b" #'eglot-format-buffer)
 (map! :leader
 :prefix "mt"
         :nv :desc "Test" "t" #'exunit-verify-single
         :nv "b" #'exunit-verify
         :nv "a" #'exunit-verify-all)
)

(use-package flycheck-mode
  :hook elixir-ts-mode
  :ensure flycheck
  :config
  (flycheck-credo-setup))

;; Dart

(use-package
  dart-mode
  :hook (dart-mode . eglot-ensure))

(use-package
 emacs
 :ensure nil
 :custom
 (treesit-language-source-alist
  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))
)

;;
;; Key Bindings
(map! :leader :desc "Shell Command on Region" "|" #'shell-command-on-region)
(map! :leader :desc "Maximize window" :prefix "w" "m" #'doom/window-maximize-buffer)
(map! :leader :desc "Toggle neotree" :prefix "p" "t" #'neotree-toggle)
;;(map! :leader :desc "Find file in project" :prefix "p" "f" #'+ivy/projectile-find-file)
(map! :leader :desc "Split vertically and focus new window" :prefix "w" "V" #'split-window-vertically-and-focus)
(map! :leader :desc "Split horizontally and focus new window" :prefix "w" "S" #'split-window-horizontally-and-focus)
(map! :leader
      :desc "Project tree"        "0" #'neotree-show
      :desc "Select Window 1"     "1" #'winum-select-window-1
      :desc "Select Window 2"     "2" #'winum-select-window-2
      :desc "Select Window 3"     "3" #'winum-select-window-3
      :desc "Select Window 3"     "4" #'winum-select-window-4)
(map! :leader :desc "Jump to symbol" :prefix "s" "j" #'imenu)
(map! :leader :desc "Search project" "/" #'+default/search-project)
(map! :leader :desc "Magit status" :prefix "g" "s" #'magit-status)
(map! :leader :desc "Copy region to clipboard" :prefix "=" "c" #'clipboard-copy-region)
(map! :leader :desc "Paste clipboard to region" :prefix "=" "v" #'clipboard-yank)
(map! :leader :desc "Copy region to clipboard" :prefix "\\" "c" #'clipboard-copy-region)
(map! :leader :desc "Paste clipboard to region" :prefix "\\" "v" #'clipboard-yank)

(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)
