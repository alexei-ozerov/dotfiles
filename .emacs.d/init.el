;;;
;;; ozerova's emacs configuration
;;;
;;; compiled from a variety of 3rd party sources while I learn emacs
;;;
;;; 3rd party sources:
;;;   * https://github.com/LionyxML/emacs-solo
;;;   * https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
;;;

;; Add Exec Path
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "/usr/bin/")
(add-to-list 'exec-path "~/go/bin")

;; Load Emacs Local Modes
(add-to-list 'load-path "~/.emacs.local/")

;; ef-themes: https://protesilaos.com/emacs/ef-themes-pictures
(add-to-list 'load-path "~/.emacs.local/ef-themes")

;; General Configuration
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

;; MELPA Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("gnu"   . "http://elpa.gnu.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; Install Packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Org Mode
(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)
(global-prettify-symbols-mode t)

;; Sly
(use-package sly
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

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package savehist
  :init
  (savehist-mode))

;; Fonts
(use-package nerd-icons
  :init
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Autocomplete
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Language Modes
(use-package go-mode
  :ensure t)

(use-package eglot
  :init
  :ensure t)

(use-package eglot-booster
	:straight ( eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster")
	:after eglot
	:config (eglot-booster-mode))

;; Local Emacs Packages 
(require 'ef-themes)
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
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)
(mapc #'disable-theme custom-enabled-themes)
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

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; Eglot Odin Config
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((odin-mode odin-ts-mode) . ("ols"))))
