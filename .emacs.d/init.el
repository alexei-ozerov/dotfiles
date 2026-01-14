;;; Initial Setup
;; Bootstrap straight.el
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
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)

;; Make sure straight doesn't download an older version of project
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))
(straight-use-package '(eldoc :type built-in))

;;; Defaults
(use-package emacs 
  :ensure nil
  
  :custom 
  ;; UI
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (use-dialog-box nil)
  (use-short-answers t)
  (display-line-numbers-type 'relative)

  ;; Scrolling
  (scroll-margin 8)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)

  ;; File Management
  (make-backup-files t)
  (backup-directory-alist `(("." . "~/.saves")))
  (auto-save-file-name-transforms `((".*" "~/.saves/" t)))
  (lock-file-name-transforms `((".*" "~/.saves/" t)))
  (savehist-mode 1)
  
  ;; Indentation
  (indent-tabs-mode nil)
  (tab-width 4)

  ;; Ripgrep
  (grep-command "rg -nS --no-heading ")
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  
  ;; Cache paths
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

  ;; Custom file (keep init.el clean)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))

  :config

  (set-frame-font "Iosevka NF 14" nil t)

  (global-display-line-numbers-mode 1)
  (global-auto-revert-mode 1)
  (pixel-scroll-precision-mode 1)
  (electric-pair-mode 1)
  
  (use-package exec-path-from-shell
    :ensure t
    :config (exec-path-from-shell-initialize))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (with-current-buffer (get-buffer-create "*scratch*")
                (let ((logo "
    ;;      ___           ___                   ___           ___           ___           ___           ___      
    ;;     /\\  \\         /\\  \\                 /\\__\\         /\\  \\         /\\  \\         /\\__\\         /\\__\\     
    ;;    /::\\  \\       /::\\  \\               /:/ _/_       |::\\  \\       /::\\  \\       /:/  /        /:/ _/_    
    ;;   /:/\\:\\  \\     /:/\\:\\  \\             /:/ /\\__\\      |:|:\\  \\     /:/\\:\\  \\     /:/  /        /:/ /\\  \\   
    ;;  /:/  \\:\\  \\   /:/ /::\\  \\           /:/ /:/ _/_    __|:|\\:\\  \\   /:/ /::\\  \\   /:/  /  ___   /:/ /::\\ \\  
    ;; /:/__/ \\:\\__\\ /:/_/:/\\:\\__\\         /:/_/:/ /\\__\\ /::::|_\\:\\__\\ /:/_/:/\\:\\__\\ /:/__/  /\\__\\ /:/_/:/\\:\\__\\ 
    ;; \\:\\  \\ /:/  / \\:\\/:/  \\/__/         \\:\\/:/ /:/  / \\:\\~~\\  \\/__/ \\:\\/:/  \\/__/ \\:\\  \\ /:/  / \\:\\/:/ /:/  / 
    ;;  \\:\\  /:/  /   \\::/__/               \\::/_/:/  /   \\:\\  \\        \\::/__/       \\:\\  /:/  /   \\::/ /:/  /  
    ;;   \\:\\/:/  /     \\:\\  \\                \\:\\/:/  /     \\:\\  \\        \\:\\  \\        \\:\\/:/  /     \\/_/:/  /   
    ;;    \\::/  /       \\:\\__\\                \\::/  /       \\:\\__\\        \\:\\__\\        \\::/  /        /:/  /    
    ;;     \\/__/         \\/__/                 \\/__/         \\/__/         \\/__/         \\/__/         \\/__/     
    ;;
    ;;   Loading time : %s
    ;;   Packages     : %s
  "))
                  (insert 
                   (propertize (format logo 
                                       (emacs-init-time)
                                       (number-to-string (length package-activated-list)))
                               'face 'fixed-pitch
                               'font-lock-face 'default)))
                (set-buffer-modified-p nil))))

  ;; End emacs
  )

;; Load Custom file
(load custom-file 'noerror)

;; Load Path
(add-to-list 'exec-path "~/.local/bin")

;;; Load Packages
;; Evil Mode & Keys 
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package evil-cleverparens
  :ensure t
  :hook ((lisp-mode . evil-cleverparens-mode)
         (emacs-lisp-mode . evil-cleverparens-mode)
         (sly-mrepl-mode . evil-cleverparens-mode)))

;; Org Mode
(use-package org
  :ensure nil
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  (org-babel-do-load-languages 
   'org-babel-load-languages 
   '((shell . t)
     (lisp . t)
     (emacs-lisp . t)
     )))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "test")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Completion
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

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :bind (:map corfu-map ("<tab>" . corfu-complete)))

(use-package corfu-terminal
  :ensure t)
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; Icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :after marginalia
  :config (nerd-icons-completion-mode))
(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Themes
(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

;; Status Bar
(use-package doom-modeline
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

;; Group minor modes
(use-package minions
  :ensure t
  :config (minions-mode 1))

;; Development
;; Git
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Terminal 
(use-package vterm)

;; K8s
(use-package kubernetes)

;; Treemacs (File Browser)
(use-package treemacs)
(use-package treemacs-evil
  :after treemacs)
(use-package treemacs-projectile
  :after treemacs)

;; Format
(setq-default tab-width 4)
(setq-default indent-tabs-mode t) ; Go requires tabs
(setq-default go-ts-mode-indent-offset 4)
(setq-default go-mode-indent-offset 4)

(use-package apheleia
  :ensure t
  :config (apheleia-global-mode +1))

;; LSP (Eglot)
(use-package eglot
  :straight (:type built-in) ;; Use built-in package (>= emacs 29)
  :hook ((go-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (rustic-mode . eglot-ensure)
         (odin-mode . eglot-ensure))
  :config
  (setq-default eglot-stay-out-of '(indentation))
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider))
  (add-to-list 'eglot-server-programs '((odin-mode odin-ts-mode) . ("ols"

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Go
(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook (go-ts-mode . (lambda ()
                        (setq-local tab-width 4)
                        (setq-local indent-tabs-mode t)
                        (setq-local go-ts-mode-indent-offset 4)
                        (electric-indent-local-mode -1))))

(use-package go-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-hook 'go-ts-mode-hook (lambda () (electric-indent-local-mode -1))))

;; Sly (CL)
(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "sbcl")
  (sly-completing-read-style 'basic)
  :hook
  (lisp-mode . sly-mode)
  (lisp-mode . corfu-mode)
  :config
  (require 'sly-autodoc)
  (add-hook 'lisp-mode-hook 'sly-autodoc-mode)
  (sly-setup '(sly-fancy)))

(use-package sly-quicklisp
  :ensure t)

;; Rust
(use-package rustic
  :ensure t
  :custom
  (rustic-format-on-save nil) ;; handled by apheleia
  (rustic-lsp-client 'eglot))

;; Terraform 
(use-package terraform-mode)

;; C/C++
(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)))

;; Odin
(use-package odin-ts-mode
  :straight (:host github :repo "Sampie159/odin-ts-mode")
  :mode ("\\.odin\\'" . odin-ts-mode)
  :hook (odin-ts-mode . eglot-ensure))

;; Elixir
(use-package elixir-ts-mode
  :mode ("\\.exs\\'" . odin-ts-mode)
  :hook (elixir-ts-mode . eglot-ensure))

;; IRC 
(use-package circe 
  :ensure t)

;;; Customization
(setq eshell-prompt-function
      '(lambda ()
         (format "[%s] %s λ "
                 (format-time-string "%H:%M")
                 (eshell/pwd))))

(defun disable-line-numbers-in-eshell ()
  "Disable line numbers (both display-line-numbers-mode and linum-mode) in Eshell."
  (display-line-numbers-mode 0) ; Emacs (>= 26)
  (linum-mode 0)                ; For older Emacs versions
  )
(add-hook 'eshell-mode-hook 'disable-line-numbers-in-eshell)

;; Org Lisp Eval
(setq org-babel-lisp-eval-fn #'sly-eval)

;; Movement
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

;; Rx Aliases
(rx-define uuid
  (and
   (repeat 8 (any "0-9a-f")) "-"
   (repeat 4 (any "0-9a-f")) "-"
   (repeat 4 (any "0-9a-f")) "-"
   (repeat 4 (any "0-9a-f")) "-"
   (repeat 12 (any "0-9a-f")))
  )
