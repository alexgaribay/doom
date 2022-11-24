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
;; treemacs

(with-eval-after-load 'treemacs
        (setq treemacs-no-png-images nil)
        (setq lsp-treemacs-sync-mode nil)
        (display-line-numbers-mode -1)
  )
(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
;;

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
(setq doom-font (font-spec :family "monospace" :size 16)
      doom-variable-pitch-font (font-spec :family "Fira Mono"))

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

(after! ivy
  ;; (add-to-list 'ivy-extra-directories ".elixir_ls")
  ;; (add-to-list 'ivy-extra-directories ".git")
  ;; (add-to-list 'ivy-extra-directories "_build")
  ;; (add-to-list 'ivy-extra-directories "build")
  ;; (add-to-list 'ivy-extra-directories "deps")
  ;; (add-to-list 'ivy-extra-directories "node_modules")
    (setq counsel-find-file-ignore-regexp "\\.elixir_ls\\'")
    (setq counsel-find-file-ignore-regexp "\\.git\\'")
    (setq counsel-find-file-ignore-regexp "\\_build\\'")
    (setq counsel-find-file-ignore-regexp "\\deps\\'")
    (setq counsel-find-file-ignore-regexp "\\node_modules\\'")
    )

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
(after! winum
  (winum-mode 1)
  (setq winum-auto-assign-0-to-minibuffer nil)
  (add-to-list 'winum-assign-functions #'winum-assign-func))

(after! neotree
  (setq neo-smart-open t)
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  )

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

;; Python

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

(use-package vue-mode
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 0)
  :mode "\\.vue\\'"
  :hook
  (vue-mode . lsp))

;; Swift
;; (use-package lsp-sourcekit
;;   :after lsp-mode
;;   :config
;;   (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

;; (use-package swift-mode
;;   :hook (swift-mode . (lambda () (lsp))))
;; (setq swift-mode:basic-offset 2)
;; Elixir

;; (use-package lsp-mode
;;   :defer
;;   :commands lsp
;;   :diminish lsp-mode
;;   :hook
;;   (elixir-mode . lsp)
;;   :init
;;   (add-to-list 'exec-path "~/elixir-ls/release/")
;;   :config
;;   (progn
;;    (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-tramp-connection "~/elixir-ls/release/language_server.sh")
;;                      :major-modes '(elixir-mode)
;;                      :remote? t
;;                      :server-id 'elixir-ls-remote))))


(eval-after-load "elixir-mode"
  '(defun elixir-format--mix-executable ()
     (string-trim-right (shell-command-to-string "asdf which mix"))))

(use-package flycheck-mode
  :hook elixir-mode
  :ensure flycheck
  :config
  (use-package flycheck-credo)
  (flycheck-credo-setup))

;; (require 'mmm-mode)
(require 'web-mode)
;; (setq mmm-global-mode 'maybe)
;; (setq mmm-parse-when-idle 't)
;; (setq mmm-set-file-name-for-modes '(web-mode))
;; (custom-set-faces '(mmm-default-submode-face ((t (:background nil)))))
;; (let ((class 'elixir-eex)
;;     (submode 'web-mode)
;;     (front "^[ ]+~L\"\"\"")
;;     (back "^[ ]+\"\"\""))
;;   (mmm-add-classes (list (list class :submode submode :front front :back back)))
;;   (mmm-add-mode-ext-class 'elixir-mode nil class))

;; (define-advice web-mode-guess-engine-and-content-type (:around (f &rest r) guess-engine-by-extension)
;;   (if (and buffer-file-name (equal "ex" (file-name-extension buffer-file-name)))
;;       (progn (setq web-mode-content-type "html")
;;          (setq web-mode-engine "elixir")
;;          (web-mode-on-engine-setted))
;;     (apply f r)))

;; Syntax Highlighting

(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 3000)
(setq-default lsp-file-watch-ignored ())
(add-to-list 'lsp-file-watch-ignored ".elixir_ls")
(add-to-list 'lsp-file-watch-ignored "deps")
(add-to-list 'lsp-file-watch-ignored "_build")
(add-to-list 'lsp-file-watch-ignored "assets/node_modules")

(with-eval-after-load 'elixir-mode
  (map! :leader :desc "Describe" :prefix "mh" "d" #'lsp-describe-thing-at-point)
  (map! :leader :desc "Find references" :prefix "mh" "r" #'lsp-find-references)
  (map! :leader :desc "Format buffer" :prefix "mf" "b" #'elixir-format)
  (map! :leader :desc "Go to definition" :prefix "mg" "d" #'lsp-goto-implementation)
  (map! :leader :desc "Go to definition" :prefix "mg" "d" #'lsp-goto-implementation)

  (map! :leader
        :prefix "mt"
          :nv :desc "Test" "t" #'exunit-verify-single
          :nv "b" #'exunit-verify
          :nv "a" #'exunit-verify-all)
  )

(setq lsp-dart-sdk-dir (concat (getenv "FLUTTER_ROOT") "/bin/cache/dart-sdk"))
(setq lsp-dart-flutter-sdk-dir (getenv "FLUTTER_ROOT"))

;; Dart/Flutter
;; (use-package lsp-mode
;;   :hook (dart-mode . lsp))

(with-eval-after-load "projectile"
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(with-eval-after-load 'dart-mode
  (map! :leader :desc "Run all tests" :prefix "mt" "a" #'lsp-dart-run-all-tests)
  (map! :leader :desc "Run tests in buffer" :prefix "mt" "b" #'lsp-dart-run-test-file)
  (map! :leader :desc "Run test at point" :prefix "mt" "t" #'lsp-dart-run-test-at-point)

  (map! :leader :desc "Show Dart outline" :prefix "mo" "d" #'lsp-dart-show-outline)
  (map! :leader :desc "Show Flutter outline" :prefix "mo" "f" #'lsp-dart-show-flutter-outline)
  (map! :leader :desc "Show test tree" :prefix "mo" "t" #'lsp-dart-test-show-tree)

  (map! :leader :desc "Pub get" :prefix "mp" "g" #'lsp-dart-pub-get)
  (map! :leader :desc "Pub upgrade" :prefix "mp" "u" #'lsp-dart-pub-upgrade)

  (map! :leader :desc "Dart Run" :prefix "mm" "r" #'lsp-dart-run)
  )

;;
;; LSP
(setq lsp-ui-sideline-enable nil)
(setq-default lsp-ui-doc-enable nil)
(setq lsp-ui-peek-enable nil)

;; Eglot

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(elixir-mode "~/elixir-ls/release/language_server.sh"))
  (add-to-list 'eglot-server-programs
               '(swift-mode "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
  (add-to-list 'eglot-server-programs
               '(dart-mode (concat (getenv "FLUTTER_ROOT") "/bin/cache/dart-sdk")))
)
;;
;; Key Bindings
(map! :leader :desc "Shell Command on Region" "|" #'shell-command-on-region)
(map! :leader :desc "Maximize window" :prefix "w" "m" #'doom/window-maximize-buffer)
(map! :leader :desc "Toggle neotree" :prefix "p" "t" #'neotree-toggle)
(map! :leader :desc "Find file in project" :prefix "p" "f" #'+ivy/projectile-find-file)
(map! :leader :desc "Show file in project tree" :prefix "p" "F" #'treemacs-find-file)
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
