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

;; Performance optimizations from modern TypeScript config
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

;; Custom functions

(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

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
;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-xcode)

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

(setq resize-mini-windows t)
;;(setq max-mini-window-height)

(after! neotree
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-width 60)
  )

(setq  doom-themes-neotree-file-icons t)
(with-eval-after-load 'doom-themes
  (doom-themes-neotree-config))

;;
;; Javascript/TypeScript

(setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq typescript-indent-level 2)

(use-package lsp-mode
  :hook ((elixir-mode . lsp-deferred)
         (elixir-ts-mode . lsp-deferred)
         (heex-mode . lsp-deferred)
         (heex-ts-mode . lsp-deferred)
         (javascript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rustic-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-eldoc-enable-hover nil)
  (add-to-list 'exec-path (expand-file-name "~/elixir-ls/release"))
  ;; Enable completion at point for JSX/TSX
  (setq lsp-completion-provider :none)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  ;; Enable additional text edits for auto-imports and prop completion
  (setq lsp-completion-enable-additional-text-edit nil)
  ;; Prefer the manually installed ElixirLS in ~/elixir-ls if present
  (let ((elixir-ls (expand-file-name "~/elixir-ls/release/language_server.sh")))
    (when (file-executable-p elixir-ls)
      (setq lsp-elixir-local-server-command elixir-ls
            lsp-elixir-server-command (list elixir-ls)))))

;; Company configuration for better React completions
(after! company
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-show-quick-access t)
  (add-to-list 'company-backends 'company-capf))

;; Better completion behavior for JSX
(after! company
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil))

;; Enable LSP code actions and snippets for React
(after! lsp-mode
  (setq lsp-auto-execute-action nil)  ; Don't auto-execute, but show options
  (setq lsp-enable-snippet t)         ; Enable snippet completions
  ;; Configure TypeScript server for better React support
  (lsp-register-custom-settings
   '(("typescript.suggest.completeFunctionCalls" t t)
     ("typescript.suggest.includeCompletionsForImportStatements" t t)
     ("typescript.suggest.autoImports" t t)
     ("typescript.preferences.generateReturnInDocTemplate" t t)))
  )

(after! lsp-rust
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t))

(after! lsp-elixir
  ;; Mirror the defaults exposed by the ElixirLS VS Code extension.
  (setq lsp-elixir-auto-build t
        lsp-elixir-dialyzer-enabled t
        lsp-elixir-incremental-dialyzer nil
        lsp-elixir-dialyzer-format "dialyxir_long"
        lsp-elixir-fetch-deps nil
        lsp-elixir-mix-env "test"
        lsp-elixir-mix-target nil
        lsp-elixir-suggest-specs t
        lsp-elixir-auto-insert-required-alias t
        lsp-elixir-signature-after-complete t
        lsp-elixir-enable-test-lenses t))

;; Key bindings for React prop assistance
(map! :after js2-mode
      :map js2-mode-map
      :localleader
      :desc "Add missing props" "a p" #'lsp-execute-code-action)

(map! :after typescript-mode
      :map typescript-mode-map
      :localleader
      :desc "Add missing props" "a p" #'lsp-execute-code-action)

(map! :after typescript-ts-mode
      :map typescript-ts-mode-map
      :localleader
      :desc "Add missing props" "a p" #'lsp-execute-code-action)

(map! :after js-ts-mode
      :map js-ts-mode-map
      :localleader
      :desc "Add missing props" "a p" #'lsp-execute-code-action)

(map! :after tsx-ts-mode
      :map tsx-ts-mode-map
      :localleader
      :desc "Add missing props" "a p" #'lsp-execute-code-action)

(use-package consult-lsp
  :after lsp-mode
  :commands consult-lsp-file-symbols consult-lsp-symbols
  :init
  (map! :leader
        :prefix ("s" . "search")
        :desc "Symbols in buffer" "j" #'consult-lsp-file-symbols))

(after! rustic
  ;; Align Rust test bindings with Elixir's test leader prefix.
  (map! :map rustic-mode-map
        :leader
        :prefix "mt"
        :desc "Test at point" "t" #'rustic-cargo-current-test
        :desc "Test module/buffer" "b" #'rustic-cargo-test-dwim
        :desc "Test project" "a" #'rustic-cargo-test)
  (when (boundp 'rust-ts-mode-map)
    (map! :map rust-ts-mode-map
          :leader
          :prefix "mt"
          :desc "Test at point" "t" #'rustic-cargo-current-test
          :desc "Test module/buffer" "b" #'rustic-cargo-test-dwim
          :desc "Test project" "a" #'rustic-cargo-test)))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'typescript-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'typescript-tsx-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'javascript-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'js-mode)
  (add-to-list 'lsp-tailwindcss-major-modes 'web-mode))

;; Web-mode
(setq web-mode-indent-style 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-block-padding 2)
(setq web-mode-part-padding 2)

(with-eval-after-load "ediff"
  (add-hook 'ediff-before-setup-windows-hook #'neotree-hide))

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Terminal configuration
(after! vterm
  ;; Disable line numbers in vterm
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  ;; Disable evil-mode in vterm
  (evil-set-initial-state 'vterm-mode 'emacs))


;; Elixir

(defun mix-format ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compilation-start "mix format")))

(defun mix-format-buffer ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (current-buffer-name (buffer-file-name))
         (file-name (file-relative-name current-buffer-name default-directory)))
    (compilation-start (format "mix format %s" file-name))))

(use-package
  elixir-mode
  :hook (elixir-ts-mode . lsp-deferred)
  :hook (heex-ts-mode . lsp-deferred)
  :hook (elixir-ts-mode . exunit-mode)
  :config
  (map! :after elixir-ts-mode :map elixir-ts-mode-map :leader :desc "Format buffer" :prefix "c" "f" #'mix-format-buffer)
  (map! :after elixir-ts-mode :map elixir-ts-mode-map :leader :desc "Format buffer" :prefix "mf" "b" #'mix-format-buffer)
  (map! :after elixir-ts-mode :map elixir-ts-mode-map :leader :desc "Format all" :prefix "mf" "a" #'mix-format)
  (map! :after elixir-ts-mode :map elixir-ts-mode-map :leader :prefix "mt"
        :nv :desc "Test" "t" #'exunit-verify-single
        :nv "b" #'exunit-verify
        :nv "a" #'exunit-verify-all)
  )

(use-package flycheck-mode
  :hook elixir-ts-mode
  :ensure flycheck
  :config
  (flycheck-credo-setup))

;; Treesitter languages

(use-package
  emacs
  :ensure nil
  :custom
  (treesit-language-source-alist
   '((heex "https://github.com/phoenixframework/tree-sitter-heex")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  )

(dolist (mapping '((elixir-mode . elixir-ts-mode)
                   (heex-mode . heex-ts-mode)
                   (javascript-mode . js-ts-mode)
                   (js-mode . js-ts-mode)
                   (typescript-mode . typescript-ts-mode)
                   (tsx-mode . tsx-ts-mode)
                   (rust-mode . rust-ts-mode)))
  (when (fboundp (cdr mapping))
    (add-to-list 'major-mode-remap-alist mapping)))

(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; Apheleia configuration for auto-formatting
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'javascript-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier))

;; Combobulate for structural editing
(use-package combobulate
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;;
;; Key Bindings
(map! :leader :desc "Shell Command on Region" "|" #'shell-command-on-region)
(map! :leader :desc "Maximize window" :prefix "w" "m" #'doom/window-maximize-buffer)
(map! :leader :desc "Toggle neotree" :prefix "p" "t" #'neotree-toggle)
(map! :leader :desc "Shell at project root" :prefix "p" "!" #'project-eshell)
;;(map! :leader :desc "Find file in project" :prefix "p" "f" #'+ivy/projectile-find-file)
(map! :leader :desc "Split vertically and focus new window" :prefix "w" "V" #'split-window-vertically-and-focus)
(map! :leader :desc "Split horizontally and focus new window" :prefix "w" "S" #'split-window-horizontally-and-focus)
(map! :leader
      :desc "Project tree"        "0" #'neotree-show
      :desc "Select Window 1"     "1" #'winum-select-window-1
      :desc "Select Window 2"     "2" #'winum-select-window-2
      :desc "Select Window 3"     "3" #'winum-select-window-3
      :desc "Select Window 3"     "4" #'winum-select-window-4)
;;(map! :leader :desc "Jump to symbol" :prefix "s" "j" #'imenu)
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
