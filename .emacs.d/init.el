;; Alexei Ozerov Emacs Configuration

;; General Configuration
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Disable Splash Screen
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Set Font & Size
(set-frame-font "Iosevka NF 28" nil t)

;; Auto-refresh buffers when files on disk change.
(global-auto-revert-mode t)

;; Ensure unique names when matching files exist in the buffer.
;; e.g. This helps when you have multiple copies of "main.rs"
;; open in different projects. It will add a "myproj/main.rs"
;; prefix when it detects matching names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Place backups in a separate folder.
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves/" t)))

;; I store automatic customization options in a gitignored file,
;; but this is definitely a personal preference.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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

(use-package evil 
  :init
  :ensure t 
  :init
  (evil-mode))

(use-package terraform-mode
  :ensure t
  :custom (terraform-indent-level 2)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

;; Local Emacs Packages 
;; ef-themes: https://protesilaos.com/emacs/ef-themes-pictures
(add-to-list 'load-path "~/.emacs.local/ef-themes")
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
(add-to-list 'load-path "~/.emacs.local/")
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
