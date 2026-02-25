;;; init.el --- Initialization -*- lexical-binding: t; -*-
;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Extra functions :3
(require 'cl-lib)

(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)

(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))
(straight-use-package '(eldoc :type built-in))

;;; ──────────────────────────────────────────────
;;; Defaults
;;; ──────────────────────────────────────────────
(use-package emacs
  :ensure nil

  :custom
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  (use-dialog-box nil)
  (use-short-answers t)
  (display-line-numbers-type 'relative)

  (scroll-margin 8)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)

  (make-backup-files t)
  (backup-directory-alist `(("." . ,(expand-file-name "saves/" user-emacs-directory))))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "saves/" user-emacs-directory) t)))
  (lock-file-name-transforms `((".*" ,(expand-file-name "saves/" user-emacs-directory) t)))

  (indent-tabs-mode nil)
  (tab-width 4)

  (grep-command "rg -nS --no-heading ")
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg"
     ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"
     ".terraform" "_build" "vendor"))

  (url-configuration-directory (expand-file-name "cache/url/" user-emacs-directory))
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (bookmark-file (expand-file-name "cache/bookmarks" user-emacs-directory))
  (multisession-directory (expand-file-name "cache/multisession/" user-emacs-directory))
  (project-list-file (expand-file-name "cache/projects" user-emacs-directory))
  (savehist-file (expand-file-name "cache/history" user-emacs-directory))
  (save-place-file (expand-file-name "cache/saveplace" user-emacs-directory))
  (transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory))
  (transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))

  (custom-file (expand-file-name "custom.el" user-emacs-directory))

  :config
  (dolist (dir '("cache/url" "cache/multisession" "cache/transient" "saves"))
    (make-directory (expand-file-name dir user-emacs-directory) t))

  (let ((font (if (eq system-type 'darwin)
                  "Iosevka NF 15"
                "Iosevka NF 14")))
    (set-frame-font font nil t))

  (global-display-line-numbers-mode 1)
  (global-auto-revert-mode 1)
  (pixel-scroll-precision-mode 1)
  (electric-pair-mode 1)
  (recentf-mode 1)
  (save-place-mode 1)
  (show-paren-mode 1)
  (setq show-paren-delay 0)

  (dolist (hook '(eshell-mode-hook vterm-mode-hook term-mode-hook))
    (add-hook hook (lambda () (display-line-numbers-mode 0))))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (with-current-buffer (get-buffer-create "*scratch*")
                (let ((logo (format "\
    ;;
    ;; Trans rights. | Loading time : %s | Packages : %s
    ;;
"
                                    (emacs-init-time)
                                    (hash-table-count straight--recipe-cache))))
                  (insert (propertize logo 'face 'font-lock-comment-face))
                  (goto-char (point-min))
                  (set-buffer-modified-p nil))))))

(load custom-file 'noerror 'nomessage)

(add-to-list 'exec-path "~/.local/bin")

(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config (exec-path-from-shell-initialize))

;;; ──────────────────────────────────────────────
;;; Evil Mode
;;; ──────────────────────────────────────────────
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-define-key '(normal visual) 'global
    (kbd "C-h") #'windmove-left
    (kbd "C-l") #'windmove-right
    (kbd "C-k") #'windmove-up
    (kbd "C-j") #'windmove-down))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package evil-cleverparens
  :ensure t
  :hook ((lisp-mode       . evil-cleverparens-mode)
         (emacs-lisp-mode . evil-cleverparens-mode)
         (scheme-mode     . evil-cleverparens-mode)
         (sly-mrepl-mode  . evil-cleverparens-mode)))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode 1))

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package evil-lion
  :ensure t
  :config (evil-lion-mode 1))

(use-package avy
  :ensure t
  :config
  (evil-define-key '(normal visual motion) 'global
    (kbd "s") #'avy-goto-char-2
    (kbd "S") #'avy-goto-line))

;;; ──────────────────────────────────────────────
;;; Completion Framework
;;; ──────────────────────────────────────────────
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :ensure nil
  :init (savehist-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s f"   . consult-find)))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :bind (:map corfu-map ("<tab>" . corfu-complete)))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :after corfu
  :config (corfu-terminal-mode +1))

(use-package which-key
  :ensure nil
  :config (which-key-mode 1))

(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

(use-package general
  :ensure t
  :config
  (general-create-definer leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer local-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix ",")

  (leader-def
    "SPC" '(execute-extended-command :wk "M-x")
    "."   '(find-file :wk "Find file")
    ","   '(consult-buffer :wk "Switch buffer")
    "/"   '(consult-ripgrep :wk "Ripgrep")
    ";"   '(comment-line :wk "Comment line")

    "b"   '(:ignore t :wk "Buffer")
    "bb"  '(consult-buffer :wk "Switch")
    "bd"  '(kill-current-buffer :wk "Kill")
    "bk"  '(kill-current-buffer :wk "Kill")
    "bn"  '(next-buffer :wk "Next")
    "bp"  '(previous-buffer :wk "Previous")
    "bs"  '(save-buffer :wk "Save")

    "f"   '(:ignore t :wk "File")
    "ff"  '(find-file :wk "Find")
    "fr"  '(consult-recent-file :wk "Recent")
    "fs"  '(save-buffer :wk "Save")

    "g"   '(:ignore t :wk "Git")
    "gg"  '(magit-status :wk "Status")
    "gb"  '(magit-blame :wk "Blame")
    "gl"  '(magit-log-current :wk "Log")
    "gd"  '(magit-diff-dwim :wk "Diff")

    "p"   '(:ignore t :wk "Project")
    "pp"  '(project-switch-project :wk "Switch")
    "pf"  '(project-find-file :wk "Find file")
    "pg"  '(consult-ripgrep :wk "Grep")
    "pk"  '(project-kill-buffers :wk "Kill buffers")
    "pd"  '(project-find-dir :wk "Find dir")
    "pe"  '(project-eshell :wk "Eshell")

    "s"   '(:ignore t :wk "Search")
    "ss"  '(consult-line :wk "Search buffer")
    "sp"  '(consult-ripgrep :wk "Search project")
    "si"  '(consult-imenu :wk "Imenu")

    "t"   '(:ignore t :wk "Toggle")
    "tz"  '(olivetti-mode :wk "Zen mode")
    "tf"  '(focus-mode :wk "Focus mode")

    "c"   '(:ignore t :wk "Code")
    "ca"  '(eglot-code-actions :wk "Code action")
    "cr"  '(eglot-rename :wk "Rename")
    "cd"  '(xref-find-definitions :wk "Definition")
    "cR"  '(xref-find-references :wk "References")
    "cf"  '(apheleia-format-buffer :wk "Format")
    "ci"  '(eglot-find-implementation :wk "Implementation")

    "o"   '(:ignore t :wk "Open")
    "ot"  '(vterm :wk "Terminal")
    "oe"  '(eshell :wk "Eshell")

    "h"   '(:ignore t :wk "Help")
    "hf"  '(describe-function :wk "Function")
    "hv"  '(describe-variable :wk "Variable")
    "hk"  '(describe-key :wk "Key")
    "hm"  '(describe-mode :wk "Mode")

    "w"   '(:ignore t :wk "Window")
    "wv"  '(split-window-right :wk "Split vertical")
    "ws"  '(split-window-below :wk "Split horizontal")
    "wd"  '(delete-window :wk "Delete")
    "wo"  '(delete-other-windows :wk "Only this")
    "w="  '(balance-windows :wk "Balance")))

;;; ──────────────────────────────────────────────
;;; Icons
;;; ──────────────────────────────────────────────
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ──────────────────────────────────────────────
;;; Theme & Modeline
;;; ──────────────────────────────────────────────
(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-project-detection 'project)
  (doom-modeline-enable-word-count nil))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package focus
  :ensure t
  :custom
  (focus-mode-to-thing '((prog-mode . defun)
                         (text-mode . paragraph)
                         (org-mode  . paragraph)))
  :hook ((prog-mode . focus-mode)
         (org-mode  . focus-mode)))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100)
  :commands olivetti-mode)

;;; ──────────────────────────────────────────────
;;; Org Mode
;;; ──────────────────────────────────────────────
(use-package org
  :ensure nil
  :defer t
  :hook (org-mode . org-indent-mode)
  :custom
  (org-babel-lisp-eval-fn #'sly-eval)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (lisp       . t)
     (emacs-lisp . t)
     (ruby       . t)
     (C          . t))))

(with-eval-after-load 'org
  (require 'ox-md))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; ──────────────────────────────────────────────
;;; Markdown
;;; ──────────────────────────────────────────────
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t))

;;; ──────────────────────────────────────────────
;;; Spellcheck
;;; ──────────────────────────────────────────────
(use-package jinx
  :ensure t
  :hook ((org-mode markdown-mode) . jinx-mode)
  :bind ("M-$" . jinx-correct))

;;; ──────────────────────────────────────────────
;;; Git
;;; ──────────────────────────────────────────────
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;; ──────────────────────────────────────────────
;;; Terminal
;;; ──────────────────────────────────────────────
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000))

;;; ──────────────────────────────────────────────
;;; Formatting
;;; ──────────────────────────────────────────────
(use-package apheleia
  :ensure t
  :config (apheleia-global-mode +1))

;;; ──────────────────────────────────────────────
;;; Tree-sitter
;;; ──────────────────────────────────────────────
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ──────────────────────────────────────────────
;;; LSP (Eglot)
;;; ──────────────────────────────────────────────
(use-package eglot
  :straight (:type git :host github :repo "joaotavora/eglot")
  :hook ((go-ts-mode      . eglot-ensure)
         (c-ts-mode       . eglot-ensure)
         (c++-ts-mode     . eglot-ensure)
         (rustic-mode      . eglot-ensure)
         (odin-ts-mode     . eglot-ensure)
         (bash-ts-mode     . eglot-ensure)
         (sh-mode          . eglot-ensure)
         (ruby-ts-mode     . eglot-ensure)
         (terraform-mode   . eglot-ensure)
         (zig-mode         . eglot-ensure)
         (zig-ts-mode      . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  :config
  (setq-default eglot-stay-out-of '(indentation))
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)
  (add-to-list 'eglot-server-programs
               '((odin-mode odin-ts-mode) . ("ols")))
  (add-to-list 'eglot-server-programs
               '((bash-ts-mode sh-mode) . ("bash-language-server" "start")))
  (add-to-list 'eglot-server-programs
               '((zig-mode zig-ts-mode) . ("zls")))
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve"))))

;;; ──────────────────────────────────────────────
;;; Language: Go
;;; ──────────────────────────────────────────────
(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook (go-ts-mode . (lambda ()
                        (setq-local tab-width 4)
                        (setq-local indent-tabs-mode t)
                        (setq-local go-ts-mode-indent-offset 4)
                        (electric-indent-local-mode -1))))

;;; ──────────────────────────────────────────────
;;; Language: Common Lisp (Sly)
;;; ──────────────────────────────────────────────
(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "sbcl")
  :hook
  (lisp-mode . sly-mode)
  :config
  (require 'sly-autodoc)
  (add-hook 'lisp-mode-hook #'sly-autodoc-mode)
  (sly-setup '(sly-fancy)))

(use-package sly-quicklisp
  :ensure t
  :after sly)

;;; ──────────────────────────────────────────────
;;; Language: Scheme (Geiser)
;;; ──────────────────────────────────────────────
(use-package geiser
  :ensure t
  :defer t)

(use-package geiser-guile
  :ensure t
  :after geiser
  :custom
  (geiser-default-implementation 'guile))

;;; ──────────────────────────────────────────────
;;; Language: Rust
;;; ──────────────────────────────────────────────
(use-package rustic
  :ensure t
  :custom
  (rustic-format-on-save nil)
  (rustic-lsp-client 'eglot))

;;; ──────────────────────────────────────────────
;;; Language: C/C++
;;; ──────────────────────────────────────────────
(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'"   . c-ts-mode)
         ("\\.h\\'"   . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'"  . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)))

;;; ──────────────────────────────────────────────
;;; Language: Odin
;;; ──────────────────────────────────────────────
(use-package odin-ts-mode
  :straight (:host github :repo "Sampie159/odin-ts-mode")
  :mode ("\\.odin\\'" . odin-ts-mode))

;;; ──────────────────────────────────────────────
;;; Language: Zig
;;; ──────────────────────────────────────────────
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

;;; ──────────────────────────────────────────────
;;; Language: Ruby
;;; ──────────────────────────────────────────────
(use-package ruby-ts-mode
  :ensure nil
  :mode (("\\.rb\\'"      . ruby-ts-mode)
         ("Gemfile\\'"    . ruby-ts-mode)
         ("Rakefile\\'"   . ruby-ts-mode)
         ("\\.rake\\'"    . ruby-ts-mode)))

;;; ──────────────────────────────────────────────
;;; Language: Terraform / HCL
;;; ──────────────────────────────────────────────
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

;;; ──────────────────────────────────────────────
;;; Language: Jsonnet
;;; ──────────────────────────────────────────────
(use-package jsonnet-mode
  :ensure t
  :mode "\\.jsonnet\\'\\|\\.libsonnet\\'")

;;; ──────────────────────────────────────────────
;;; File modes (YAML, Dockerfile, etc.)
;;; ──────────────────────────────────────────────
(use-package dockerfile-ts-mode
  :ensure nil
  :mode ("\\(?:Dockerfile\\|Containerfile\\)\\(?:\\.[a-zA-Z0-9]+\\)?\\'"
         . dockerfile-ts-mode))

(use-package envrc
  :ensure t
  :config (envrc-global-mode 1))

;;; ──────────────────────────────────────────────
;;; Eshell
;;; ──────────────────────────────────────────────
(setq eshell-prompt-function
      (lambda ()
        (format "[%s] %s λ "
                (format-time-string "%H:%M")
                (abbreviate-file-name (eshell/pwd)))))
(setq eshell-prompt-regexp "^\\[.*\\] .* λ ")

(use-package eat
  :ensure t
  :hook (eshell-mode . eat-eshell-mode))

;;; ──────────────────────────────────────────────
;;; Rx Aliases
;;; ──────────────────────────────────────────────
(rx-define uuid
  (and
   (repeat 8 (any "0-9a-f")) "-"
   (repeat 4 (any "0-9a-f")) "-"
   (repeat 4 (any "0-9a-f")) "-"
   (repeat 4 (any "0-9a-f")) "-"
   (repeat 12 (any "0-9a-f"))))

;;; ──────────────────────────────────────────────
;;; Eldoc — display and navigation
;;; ──────────────────────────────────────────────
(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.3)
               (slot . 0)))

(defvar my/eldoc-lang-config
  `((go-ts-mode
     :extract "^func\\s-+([^)]+)\\s-+\\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "func \\(\\w+ \\S+\\) %s\\(" name))
     :glob "*.go")

    (rust-mode
     :extract "^\\(?:pub \\)?fn \\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "(pub )?fn %s\\(" name))
     :glob "*.rs")

    (rustic-mode
     :extract "^\\(?:pub \\)?fn \\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "(pub )?fn %s\\(" name))
     :glob "*.rs")

    (ruby-ts-mode
     :extract "^\\s-*def \\(?:self\\.\\)?\\([[:alnum:]_?!]+\\)"
     :group 1
     :rg-pattern ,(lambda (name) (format "def (self\\.)?%s" (regexp-quote name)))
     :glob "*.rb")

    (zig-mode
     :extract "^\\(?:pub \\)?fn \\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "(pub )?fn %s\\(" name))
     :glob "*.zig")

    (zig-ts-mode
     :extract "^\\(?:pub \\)?fn \\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "(pub )?fn %s\\(" name))
     :glob "*.zig")

    (odin-ts-mode
     :extract "^\\([[:alnum:]_]+\\)\\s-*::"
     :group 1
     :rg-pattern ,(lambda (name) (format "%s\\s*::" name))
     :glob "*.odin")

    (odin-mode
     :extract "^\\([[:alnum:]_]+\\)\\s-*::"
     :group 1
     :rg-pattern ,(lambda (name) (format "%s\\s*::" name))
     :glob "*.odin")

    (c-ts-mode
     :extract "^[[:alnum:]_*]+\\s-+\\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "\\w+\\s+%s\\(" name))
     :glob "*.{c,h}")

    (c++-ts-mode
     :extract "^[[:alnum:]_*:]+\\s-+\\([[:alnum:]_]+\\)("
     :group 1
     :rg-pattern ,(lambda (name) (format "\\w+\\s+%s\\(" name))
     :glob "*.{cpp,cc,hpp,h}"))
  "Per-language eldoc buttonization config.")

(defun my/eldoc-get-config ()
  "Get the lang config for the current source buffer."
  (let ((src-mode nil))
    (dolist (b (buffer-list))
      (when (and (not src-mode)
                 (with-current-buffer b
                   (and (bound-and-true-p eglot--managed-mode)
                        (derived-mode-p 'prog-mode))))
        (setq src-mode (buffer-local-value 'major-mode b))))
    (when src-mode
      (assq src-mode my/eldoc-lang-config))))

(defun my/eldoc-get-source-buffer (mode)
  "Find the source buffer matching MODE."
  (seq-find
   (lambda (b)
     (with-current-buffer b
       (derived-mode-p mode)))
   (buffer-list)))

(defun my/buttonize-eldoc ()
  "Buttonize function/method signatures in *eldoc* buffer."
  (interactive)
  (let ((buf (get-buffer "*eldoc*")))
    (if (not buf)
        (message "my/buttonize: no *eldoc* buffer")
      (let* ((config (my/eldoc-get-config)))
        (if (not config)
            (message "my/buttonize: no language config for current buffer")
          (let* ((mode (car config))
                 (props (cdr config))
                 (extract (plist-get props :extract))
                 (group (plist-get props :group))
                 (rg-fn (plist-get props :rg-pattern))
                 (glob (plist-get props :glob))
                 (src (my/eldoc-get-source-buffer mode))
                 (project-root (when src
                                 (with-current-buffer src
                                   (when (project-current)
                                     (expand-file-name
                                      (project-root (project-current))))))))
            (if (not project-root)
                (message "my/buttonize: no project root found")
              (with-current-buffer buf
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      (count 0))
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward extract nil t)
                      (let ((name (match-string group))
                            (beg (match-beginning group))
                            (end (match-end group)))
                        (unless (button-at beg)
                          (let ((method name)
                                (pattern-fn rg-fn)
                                (file-glob glob)
                                (root project-root))
                            (make-text-button
                             beg end
                             'face 'link
                             'follow-link t
                             'help-echo (format "Jump to %s" method)
                             'action
                             (lambda (_button)
                               (let* ((pattern (funcall pattern-fn method))
                                      (cmd (format
                                            "rg -n --no-heading '%s' --glob '%s' %s"
                                            pattern file-glob root))
                                      (output (string-trim
                                               (shell-command-to-string cmd)))
                                      (hit nil))
                                 (when (and (not (string-empty-p output))
                                            (string-match
                                             "^\\([^:]+\\):\\([0-9]+\\):" output))
                                   (setq hit
                                         (list (match-string 1 output)
                                               (string-to-number
                                                (match-string 2 output)))))
                                 (if hit
                                     (progn
                                       (find-file (car hit))
                                       (goto-char (point-min))
                                       (forward-line (1- (cadr hit)))
                                       (recenter))
                                   (message "Could not find definition for %s"
                                            method))))))
                          (setq count (1+ count))))))
                  (when (> count 0)
                    (evil-local-set-key 'normal (kbd "RET") #'push-button)
                    (evil-local-set-key 'normal (kbd "gd") #'push-button)
                    (evil-local-set-key 'normal (kbd "q") #'quit-window))
                  (message "my/buttonize: %d methods linked" count))))))))))

(defun my/eldoc-buttonize-and-show ()
  "Show eldoc buffer, then buttonize when content is ready."
  (interactive)
  (eldoc-doc-buffer t)
  (let ((attempts 0)
        (timer nil))
    (setq timer
          (run-with-timer
           0.3 0.3
           (lambda ()
             (setq attempts (1+ attempts))
             (let ((buf (get-buffer "*eldoc*")))
               (when (or (> attempts 15)
                         (and buf (> (buffer-size buf) 0)))
                 (cancel-timer timer)
                 (when (and buf (> (buffer-size buf) 0))
                   (my/buttonize-eldoc)))))))))

(defun my/setup-eldoc-k ()
  "Override K to use eldoc buttonize."
  (evil-local-set-key 'normal (kbd "K") #'my/eldoc-buttonize-and-show))

(dolist (hook '(go-ts-mode-hook
               rustic-mode-hook
               rust-mode-hook
               ruby-ts-mode-hook
               zig-mode-hook
               zig-ts-mode-hook
               odin-ts-mode-hook
               odin-mode-hook
               c-ts-mode-hook
               c++-ts-mode-hook))
  (add-hook hook #'my/setup-eldoc-k))

;;; init.el ends here
