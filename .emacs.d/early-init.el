;;; early-init.el --- Pre-GUI initialization -*- lexical-binding: t; -*-
;;;
;;; 3rd party sources:
;;;   * https://github.com/LionyxML/emacs-solo
;;;   * https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(setenv "LSP_USE_PLISTS" "true")

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend increases booting speed
(setq vc-handled-backends '(Git))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31

;; Catppuccin Mocha palette — eliminates white flash
(setq default-frame-alist
      '((fullscreen           . maximized)
        (menu-bar-lines       . 0)
        (tool-bar-lines       . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (left-fringe          . 0)
        (right-fringe         . 0)
        (background-color     . "#1e1e2e")   ; Mocha Base
        (foreground-color     . "#cdd6f4"))) ; Mocha Text

(setq initial-frame-alist default-frame-alist)

;; Better window management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "Emacs - [p] " (project-name project))
            (concat "Emacs - " (buffer-name))))))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

(setq inhibit-compacting-font-caches t)
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; early-init.el ends here
