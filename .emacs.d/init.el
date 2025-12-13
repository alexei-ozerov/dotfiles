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

;; Setup MELPA/GNU packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (unless (package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Defaults
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
  
  ;; Indentation
  (indent-tabs-mode nil)
  (tab-width 4)
  
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

  (set-frame-font "Iosevka NF 18" nil t)

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

(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

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

;; Icons
(use-package nerd-icons :ensure t)
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

;; Themes
(add-to-list 'load-path "~/.emacs.local/")
; (add-to-list 'load-path "~/.emacs.local/ef-themes")
;
; (use-package ef-themes
;   :ensure t
;   :config
;   (setq ef-themes-mixed-fonts t
;         ef-themes-variable-pitch-ui t)
;
;   (defun ef-themes-overrides ()
;     (custom-set-faces
;      '(font-lock-function-name-face ((t :weight bold :height 1.1 :inherit ef-themes-function)))
;      '(font-lock-type-face ((t :slant italic :inherit ef-themes-type)))
;      '(font-lock-constant-face ((t :weight bold :inherit ef-themes-constant)))
;      '(font-lock-variable-name-face ((t :inherit ef-themes-variable)))
;      '(font-lock-property-name-face ((t :slant italic :inherit ef-themes-variable)))))
;
;   (add-hook 'ef-themes-post-load-hook #'ef-themes-overrides)
;
;   (ef-themes-select 'ef-summer))

(use-package catppuccin-theme
  :ensure t)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)

;; Status Bar
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

;; Group minor modes
(use-package minions
  :ensure t
  :config (minions-mode 1))

;; Development
;; Project Management
(use-package project :ensure nil)

;; Git
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Format
(use-package apheleia
  :ensure t
  :config (apheleia-global-mode +1))

;; LSP (Eglot)
(use-package eglot
  :ensure nil ; Built-in since Emacs 29
  :hook ((go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (rustic-mode . eglot-ensure)
         (odin-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((odin-mode odin-ts-mode) . ("ols"))))

;; Treesitter 
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

;; Go
(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'")

;; Rust
(use-package rustic
  :ensure t
  :custom
  (rustic-format-on-save nil) ;; handled by apheleia
  (rustic-lsp-client 'eglot))

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

;; Org Mode
(use-package org
  :ensure nil
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t))))

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

