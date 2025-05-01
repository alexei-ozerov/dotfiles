;; Alexei Ozerov Emacs Configuration

;; Add Exec Path
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "~/builds/ols")
(add-to-list 'exec-path "~/go/bin")

;; Load Emacs Local Modes
(add-to-list 'load-path "~/.emacs.local/")

;; ef-themes: https://protesilaos.com/emacs/ef-themes-pictures
(add-to-list 'load-path "~/.emacs.local/ef-themes")

;; General Configuration
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(global-display-line-numbers-mode)

(setq display-line-numbers 'relative)

(global-auto-revert-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves/" t)))
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable Splash Screen
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Configure Spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Set Font & Size
(set-frame-font "Iosevka NF 18" nil t)

;; Optimizations
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; MELPA Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install Packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Org Mode
(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)
(global-prettify-symbols-mode t)

;; Vterm
(use-package vterm
    :ensure t)

;; eVIl Mode
(use-package evil 
  :init
  :ensure t 
  :init
  (evil-mode))

;; Drag Stuff Mode
(use-package drag-stuff
  :init
  :ensure t)

(drag-stuff-mode t)
(define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)

(use-package evil-collection
  :ensure t
  :after evil)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
        (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Magit
(use-package magit
  :bind
  ("C-x g" . magit-status)  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)  (with-eval-after-load 'magit-remote
    (magit-define-popup-action 'magit-push-popup ?P
      'magit-push-implicitly--desc
      'magit-push-implicitly ?p t))  (add-hook 'with-editor-mode-hook 'evil-insert-state))

;; Minibuffer Customization
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

;; Fonts
(use-package nerd-icons
  :init
  :ensure t)

;; Autocomplete
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Language Modes
(use-package go-mode
  :ensure t)

(use-package terraform-mode
  :ensure t
  :custom (terraform-indent-level 2)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package eglot
  :init
  :ensure t)

(use-package eglot-booster
    :ensure t
	:after eglot
	:config	(eglot-booster-mode))

;; Local Emacs Packages 
(require 'ef-themes)

;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(setq ef-themes-to-toggle '(ef-summer ef-winter))

(setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 variable-pitch light 1.9)
        (1 variable-pitch light 1.8)
        (2 variable-pitch regular 1.7)
        (3 variable-pitch regular 1.6)
        (4 variable-pitch regular 1.5)
        (5 variable-pitch 1.4) ; absence of weight means `bold'
        (6 variable-pitch 1.3)
        (7 variable-pitch 1.2)
        (t variable-pitch 1.1)))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
(load-theme 'ef-summer :no-confirm)

;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
(ef-themes-select 'ef-summer)

;; Configure Split Movement Via Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k")    'windmove-up)
(global-set-key (kbd "C-j")  'windmove-down)

;; Simple C Mode
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; AStyle C Formatter
(defun astyle-buffer ()
  (interactive
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number))))

;; Require
(require 'odin-mode)
(require 'company-go)
(require 'go-mode)

;; Eglot Odin Config
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((odin-mode odin-ts-mode) . ("ols"))))

;; Treesitter Grammer List
;; TODO: Review this list and convert to tagged releases
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
