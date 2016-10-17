(require 'cl) ;; Access to unprefixed names i.e. loop and return instead of cl-loop and cl-return
(load "package")

;; Add marmalade package archive
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Add melpa package archive
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Add elpy package archive
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)

;; Packages
(defvar yondon/packages '(auto-complete
			  autopair
			  projectile
        helm
        helm-projectile
			  editorconfig
			  web-mode
			  sass-mode
			  scss-mode
			  markdown-mode
			  robe
			  smartparens
        elpy
        auctex
        exec-path-from-shell
			  zenburn-theme)
  "Default packages")

;; Install default packages
(defun yondon/packages-installed-p ()
  (loop for pkg in yondon/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (yondon/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg yondon/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)
;; Editorconfig
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'editorconfig)
(editorconfig-mode 1)

;; Splash screen
;; Skip to scratch buffer
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1) ;; Turn off scroll bar
(tool-bar-mode -1) ;; Turn off tool bar
(menu-bar-mode -1) ;; Turn off menu bar

(setq visible-bell t) ;; Turn off system beep
(setq font-lock-maximum-decoration t) ;; Maximum colors
(setq echo-keystrokes 0.1) ;; Turn down time to echo keystrokes

(windmove-default-keybindings) ;; Navigate between windows using alt-1, alt-2, shift-left, shift-up, shift-right

(global-auto-revert-mode t) ;; Auto-refresh buffers when files change on disk

;; Marking text to treat regions like other text editors
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t) ;; So system clipboard and Emacs clipboard get along

;; Convert tabs to 2 spaces
(setq tab-width 2
      indent-tabs-mode nil)
(setq-default tab-width 2)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq column-number-mode t) ;; Turn on column numbers

(show-paren-mode t) ;; Always highlight parentheses

(require 'autopair) ;; Brace structures closed as soon as opening character is typed

(require 'auto-complete-config) ;; Turn on auto complete
(ac-config-default)

(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode t)

(setq make-backup-files nil) ;; Turn off backup files

;; See when file actually ends
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no

;; Key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(custom-safe-themes
	 (quote
		("86096212283f66bca40bfe557f36eeb92446b31bac743be802cb5f19bfa1ce5a" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a71be4e5e9e418025daea651f8a1628953abb7af505da5e556e95061b6a6e389" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Projectile
(require 'helm)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm) ;; Use helm completion mode
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

;; Load custom configs
(add-to-list 'load-path "~/.emacs.d/custom")

;; Add path for pdflatex
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

(load "custom_web.el")

(add-hook 'python-mode-hook
          (lambda()
            (load "custom_python.el")))

(add-hook 'ruby-mode-hook
          (lambda()
            (load "custom_ruby.el")))

(add-hook 'markdown-mode-hook
          (lambda()
            (load "custom_markdown.el")))
